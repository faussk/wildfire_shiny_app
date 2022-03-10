library(shiny)
library(shinythemes)
library(tidyverse)
library(palmerpenguins)
library(here)
library(sf)
library(lubridate)
library(RColorBrewer)


# CREATE USER INTERFACE
ui <- fluidPage(
  theme = shinytheme('yeti'),
  titlePanel('Wildfire in CA Chaparral'),
  navbarPage(
    '',
    
    tabPanel('WIDGET 1',
             sidebarLayout(
               sidebarPanel('Enter Date Range',
                            dateRangeInput('dateRange',
                                           label = 'Date range input: yyyy',
                                           start = '1990-01-01', 
                                           end = '2000-01-01',
                                           format = 'yyyy'
                            ), #END of date range
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
                            radioButtons(inputId='n',
                                         label='Choose gif:',
                                         choices=c('Simple'='Simple','Prob Spread'='ProbSpread','Steps Burn'='StepsBurn','Combined'='Combined')),
               ), # END of sidebarPanel
               mainPanel('Fire Spread Gif',
                         plotOutput('flam_gif')
               ) # END of mainPanel
             ) # END of sidebarLayout
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
  eco261ab_cent_rec$YEAR_ <- as.numeric(eco261ab_cent_rec$YEAR_)
  # Drop NA's
  eco261ab_cent_rec <- eco261ab_cent_rec %>%
    drop_na(area_km2) %>%
    drop_na(LFM_Av20km)
  
  # WIDGET 1
  eco261ab_cent_rec_dateRed <- reactive({
    eco261ab_cent_rec %>%
      filter(YEAR_>=year(input$dateRange[1]) & YEAR_<=year(input$dateRange[2]))
  }) # END eco261ab_cent_rec_dateRed reactive
  output$fire_map <- renderPlot({
    plot(eco261ab_cent_rec_dateRed()['LFM_Av20km'], col=brewer.pal(n=10,name='RdYlGn'))
  }) # END fire_map output

  # WIDGET 2
  # #flam_gif
  output$flam_gif <- renderImage({
    filename <- here(paste('gifs/',input$n, '.gif', sep=''))
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Gif Name", input$n))
  }, deleteFile = FALSE)
  
  # WIDGET 3
  min_lfm <- min(eco261ab_cent_rec$LFM_Av20km)
  max_lfm <- max(eco261ab_cent_rec$LFM_Av20km)
  eco261ab_cent_rec_df <- st_drop_geometry(eco261ab_cent_rec)
  
  class_mids <- reactive({
    seq(min_lfm, max_lfm+as.numeric(input$lfm_step),as.numeric(input$lfm_step))[-1] - diff(seq(min_lfm, max_lfm+as.numeric(input$lfm_step),as.numeric(input$lfm_step)))/2
    }) # END class_mids reactive
  mean_area <- reactive({
    tapply(eco261ab_cent_rec_df$area_km2, cut(eco261ab_cent_rec_df$LFM_Av20km, seq(min_lfm, max_lfm+as.numeric(input$lfm_step),as.numeric(input$lfm_step)), include.lowest = TRUE), mean)
  }) # END mean_area reactive
  
  output$mean_fire_plot <- renderPlot({
    plot(mean_area()~class_mids(),
         xlab='LFM',
         ylab='Average Fire Size',
         pch=16,
         cex=1.7,
         col='dark red')
  }) # END mean_fire_plot output
  
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







