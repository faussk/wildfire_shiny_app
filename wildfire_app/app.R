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
             sidebarLayout(
               sidebarPanel('Enter Date Range',
                            dateRangeInput('dateRange',
                                           label = 'Date range input: yyyy-mm-dd',
                                           start = Sys.Date() - 2, end = Sys.Date() + 2
                            ), #END of date range
                            verbatimTextOutput("dateRangeText")
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
                                        choices = c('1 %'=1.,'5 %'=5.))#,'10 %'=10., '20 %'=20., '50 %'=50.))
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
                                         choices=c('No minimum fire size'=0.,'1 km^2'=1.,'5 km^2'=5.,'10 km^2'=10.)),
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
  output$dateRangeText  <- renderText({
    paste("input$dateRange is", 
          paste(as.character(input$dateRange), collapse = " to ")
    )
  })
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
  
  # # WORKSHOP REACTIVE
  # # input$lfm_step
  # 
  # # lfm_class <- reactive({
  # #   cut(eco261ab_cent_rec$LFM_Av20km, seq(min_lfm, max_lfm+input$lfm_step,input$lfm_step), include.lowest = TRUE)
  # #   }) # END lfm_class reactive
  # mean_area <- reactive({
  #   tapply(eco261ab_cent_rec$area_km2, (cut(eco261ab_cent_rec$LFM_Av20km, seq(min_lfm, max_lfm+input$lfm_step,input$lfm_step), include.lowest = TRUE)), mean)
  #   })
  # class_mids <- reactive({
  #   seq(min_lfm, max_lfm+input$lfm_step,input$lfm_step)[-1] - diff(seq(min_lfm, max_lfm+input$lfm_step,input$lfm_step))/2
  #   }) # END class_mids reactive
  # # range_Seq <- reactive({
  # #   range(seq(min_lfm, max_lfm+input$lfm_step,input$lfm_step))
  # # }) # END range_Seq reactive
  # 
  # output$mean_fire_plot <- renderPlot({
  #   plot(mean_area()~class_mids(),
  #        xlab='LFM',
  #        ylab='Average Fire Size')
  # }) # END mean_fire_plot output
  
  # WORKING NON REACTIVE
  lfm_step_input <- reactive({input$lfm_step})
  Seq <- seq(min_lfm, max_lfm+lfm_step_input(),lfm_step_input())

  lfm_class <- cut(eco261ab_cent_rec$LFM_Av20km, Seq, include.lowest = TRUE)
  mean_area <- tapply(eco261ab_cent_rec$area_km2, lfm_class, mean)
  class_mids <- Seq[-1] - diff(Seq)/2

  output$mean_fire_plot <- renderPlot({
    plot(mean_area~class_mids,
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
  big_fires <- reactive({
   eco261ab_cent_rec %>%
     filter(eco261ab_cent_rec$area_km2>=input$large_fire)
  }) # END big_fires reactive
  
  output$lfm_hist <- renderPlot({
    bins <- seq(min(big_fires()$LFM_Av20km), max(big_fires()$LFM_Av20km), length.out=input$bins+1)
    hist(big_fires()$LFM_Av20km, breaks=bins, col='orange', border='black',
         xlab='Live Fuel Moisture (LFM)',
         main='Frequency of Fires with Varied LFM')
  }) # END big_fires output
} # END SERVER



# COMBINE UI & SERVER INTO APP
shinyApp(ui=ui, server=server)







