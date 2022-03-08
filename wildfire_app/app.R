library(shiny)
library(shinythemes)
library(tidyverse)
library(palmerpenguins)
library(here)
library(sf)


# CREATE USER INTERFACE
ui <- fluidPage(
  theme = shinytheme('yeti'),
  titlePanel('Wildfire in CA Chaparral'),
  navbarPage(
    '',
    tabPanel('WIDGET 1a',
             sidebarLayout(
               sidebarPanel('[CAN TYPE SM HERE]',
                            #CHECKBOX:
                            checkboxGroupInput(inputId='pick_species',
                                               label='Choose species:',
                                               choices=unique(starwars$species)
                            ) # END checkboxGroupInput
               ), # END of sidebarPanel
               mainPanel('Checkbox, Dropdown, Point Plot',
                         plotOutput('starwars_plot')
                         ) # END of mainPanel
             ) # END of sidebarLayout
    ), # END of tabPanel for widget 1a
    
    tabPanel('WIDGET 1',
             sidebarLayout((
               sidebarPanel('TEST',
                            # Date range
                            ) # END DateRange
             ), # END of sidebarPanel
             mainPanel('Fires Map',
                       plotOutput('fire_map')
                       ) # END of mainPanel
             ) # END of sidebarLayout
    ), # END of tabPanel for widget 1a
    
    tabPanel('WIDGET 2',
             sidebarLayout(
               sidebarPanel('[CAN TYPE SM HERE]',
                            # RADIO BUTTONS:
                            radioButtons(inputId='penguin_species',
                                         label='Choose penguin species:',
                                         choices=c('Adelie Button'='Adelie','Chinstrap Button'='Chinstrap','Gentoo Button'='Gentoo')),
                            # DROPDOWN SELECT
                            selectInput(inputId = 'pt_color',
                                        label = 'Choose point color:',
                                        choices = c('Awesome red!'='red','Blue!'='blue','Grass!'='green'))
               ), # END sidebar panel  
               mainPanel('Radio Buttons, Dropdown, Point Plot, Summary Table',
                         plotOutput(outputId='penguin_plot'), # renders 'penguin_plot' which was created under Server below
                         tableOutput(outputId = 'penguin_table')
                         ) # END of mainPanel 
             ) # END sidebarLayout
    ), # END of tabPanel for widget 2
    
    tabPanel('WIDGET 3',
             sidebarLayout(
               sidebarPanel('Select LFM Step Size',
                            # DROPDOWN SELECT
                            selectInput(inputId = 'lfm_step',
                                        label = 'To calculate mean fire size by step increments of LFM',
                                        choices = c('1 %'=1.,'5 %'=5.,'10 %'=10., '20 %'=20., '50 %'=50.))
               ), # END sidebar panel 
               mainPanel('Average Fire Size calculated for given LFM Step Size',
                         plotOutput(outputId='mean_fire_plot')
                         
               ) # END of mainPanel
             )), # END of tabPanel for widget 3
    
    tabPanel('WIDGET 4a',
             sidebarLayout(
               sidebarPanel('[CAN TYPE SM HERE]',
                            # DROPDOWN SELECT
                            selectInput(inputId = 'large',
                                        label = 'Define Large Penguin by Body Mass (g):',
                                        choices = c('4000 g'=4000.,'5000 g'=5000.,'6000 g'=6000.))
               ), # END sidebar panel 
               mainPanel('Dropdown, Histogram',
                         plotOutput(outputId='large_penguin_plot')
                         
               ) # END of mainPanel
             )), # END of tabPanel for widget 4a
    
    tabPanel('WIDGET 4',
             sidebarLayout(
               sidebarPanel('Choose a minimum fire size:',
                            # RADIO BUTTONS:
                            radioButtons(inputId='large_fire',
                                         label='',
                                         choices=c('No minimum fire size'=0.,'1 km^2'=1.,'10 km^2'=10.,'100 km^2'=100.,'1,000 km^2'=1000.)),
                            'Number of Bins:',
                            # SLIDER:
                            sliderInput(inputId='bins',
                                        label='',
                                        min=1,
                                        max=100,
                                        value=30)
               ), # END sidebar panel 
               mainPanel('Select a Minimum Fire Size and Number of Bins to Vary LFM Bin Width',
                         plotOutput(outputId='lfm_hist')
               ) # END of mainPanel
             )) # END of tabPanel for widget 4

  ), # END navbarpage
) # END UI



# CREATE SERVER FUNCTION
server <- function(input,output) {
  # Read in data as spatial feature
  eco261ab_cent_rec <- st_read(dsn = here("BurnScarsFRAP_wRecLFM_Chamise_CentEco261AB_Refined022021"))
  # Ensure numeric
  eco261ab_cent_rec$LFM_Av20km <- as.numeric(eco261ab_cent_rec$LFM_Av20km)
  eco261ab_cent_rec$area_km2 <- as.numeric(eco261ab_cent_rec$area_km2)
  # Drop NA's
  eco261ab_cent_rec <- eco261ab_cent_rec %>%
    drop_na(area_km2) %>%
    drop_na(LFM_Av20km)
  
  # WIDGET 1a
  starwars_reactive <- reactive({
    starwars%>%
      filter(species%in% input$pick_species)
  }) # END starwars_reactive
  output$starwars_plot <- renderPlot(
    ggplot(data=starwars_reactive(), # starwars_reactive is a "reactive" dataframe, therefore must end like w/ () : reactive_df()
           aes(x=mass, y=height)) +
      geom_point(aes(color=species))
  ) # END output starwars_plot
  
  # WIDGET 1
  
  output$fire_map <- renderPlot({
    plot(eco261ab_cent_rec['LFM_Av20km'])
  })
  
  # WIDGET 2
  penguin_select <- reactive({
    penguins %>% 
      filter(species==input$penguin_species) # match widget input to df column 'species'
  }) # END penguin_select reactive
  output$penguin_plot <- renderPlot({
    ggplot(data=penguin_select(), # penguin_select is a "reactive" dataframe, therefore must end like w/ () : reactive_df()
           aes(x=flipper_length_mm, y=body_mass_g))+
      geom_point(color=input$pt_color)
  })  # END penguin_select output
  # 2- Table
  penguin_table <- reactive({
    penguins %>%
      filter(species == input$penguin_species) %>%
      group_by(sex) %>%
      summarize(
        mean_flip = mean(flipper_length_mm),
        mean_mass = mean(body_mass_g)
      )
  }) # END penguin_table reactive
  output$penguin_table<-renderTable({
    penguin_table()
  }) # END penguin_table output
  
  # WIDGET 3
  min_lfm <- min(eco261ab_cent_rec$LFM_Av20km)
  max_lfm <- max(eco261ab_cent_rec$LFM_Av20km)
  
  lfm_step_input <- 5 # reactive(input$lfm_step) # HERE !!!
  
  Seq <- seq(min_lfm, max_lfm+lfm_step_input,lfm_step_input)
  
  lfm.class <- cut(eco261ab_cent_rec$LFM_Av20km, Seq, include.lowest = TRUE)
  mean.area <- tapply(eco261ab_cent_rec$area_km2, lfm.class, mean)
  class.mids <- Seq[-1] - diff(Seq)/2
  
  output$mean_fire_plot <- renderPlot({
    plot(mean.area~class.mids,
         xlab='LFM',
         ylab='Average Fire Size',
         xlim=range(Seq))
  }) # END mean_fire_plot output
  
  # WIDGET 4a
  big_penguins <- reactive({
    penguins %>% 
      filter(body_mass_g>=input$large)
  }) # END penguin_large reactive
  output$large_penguin_plot <- renderPlot({
    ggplot(data=big_penguins(), aes(x=species, y=body_mass_g))+
      geom_jitter(aes(color=sex))
  })  # END penguin_large output
  
  # WIDGET 4
  # HERE !!!
  #big_fires <- reactive({
  #  eco261ab_cent_rec %>% 
  #    filter(big_fires>=input$large_fire)
  #}) # END penguin_large reactive
  output$lfm_hist <- renderPlot({
    big_fires <- eco261ab_cent_rec # HERE !!!
    x <- big_fires$LFM_Av20km
    
    bins <- seq(min(x), max(x), length.out=input$bins+1)
    hist(x, breaks=bins, col='orange', border='black',
         xlab='Live Fuel Moisture (LFM)',
         main='Frequency of Fires with Varied LFM')
  })
  

  
} # END SERVER



# COMBINE UI & SERVER INTO APP
shinyApp(ui=ui, server=server)







