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
    tabPanel('WIDGET 1',
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
    ), # END of tabPanel for widget 1
    
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
               sidebarPanel('Number of Bins',
                            # SLIDER:
                            sliderInput(inputId='bins',
                                        label='LFM',
                                        min=1,
                                        max=100,
                                        value=30)
               ), # END sidebar panel 
               mainPanel('Select Number of Bins to Vary LFM Bin Width',
                         plotOutput(outputId='lfm_hist')
               ) # END of mainPanel
             )), # END of tabPanel for widget 3
    
    tabPanel('WIDGET 4',
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
             )) # END of tabPanel for widget 4
  ), # END navbarpage
  
) # END UI



# CREATE SERVER FUNCTION
server <- function(input,output) {
  
  # Read in Burn Scar & LFM data here as shapefile:
  eco261ab_cent_rec <- read_sf(dsn = here("BurnScarsFRAP_wRecLFM_Chamise_CentEco261AB_Refined022021"))
  eco261ab_cent_rec$LFM_Av20km <- as.numeric(eco261ab_cent_rec$LFM_Av20km)
  lfm_rec_20km <- eco261ab_cent_rec$LFM_Av20km
  area_km2 <- eco261ab_cent_rec$area_km2
  
  # WIDGET 1
  starwars_reactive <- reactive({
    starwars%>%
      filter(species%in% input$pick_species)
  }) # END starwars_reactive
  output$starwars_plot <- renderPlot(
    ggplot(data=starwars_reactive(), # starwars_reactive is a "reactive" dataframe, therefore must end like w/ () : reactive_df()
           aes(x=mass, y=height)) +
      geom_point(aes(color=species))
  ) # END output starwars_plot
  
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
  output$lfm_hist <- renderPlot({
    x <- lfm_rec_20km
    x <- na.omit(x)
    bins <- seq(min(x), max(x), length.out=input$bins+1)
    hist(x, breaks=bins, col='orange', border='black',
         xlab='Live Fuel Moisture (LFM)',
         main='Frequency of Fires with Varied LFM')
  })
  
  # WIDGET 4
  big_penguins <- reactive({
    penguins %>% 
      filter(body_mass_g>=input$large) # match widget input to df column 'species'
  }) # END penguin_large reactive
  output$large_penguin_plot <- renderPlot({
    ggplot(data=big_penguins(), aes(x=species, y=body_mass_g))+
      geom_jitter(aes(color=sex))
  })  # END penguin_large output
  
} # END SERVER



# COMBINE UI & SERVER INTO APP
shinyApp(ui=ui, server=server)







