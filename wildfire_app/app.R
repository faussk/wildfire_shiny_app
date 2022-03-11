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
    
    tabPanel('WELCOME MAP',
             sidebarLayout(
               sidebarPanel('Welcome!',
                            helpText('This application explores the effect of changes in plant fuels on wildfire spread in CA chaparral.'),
                            helpText('Live fuel moisture was assigned to fire perimeters by proximity to point LFM samples. A simple average was taken among all samples within 20km of the fire centroid and 10 days pre and post the burn event.'),
                            dateRangeInput('dateRange',
                                           label = 'Enter Date Range: YYYY to YYYY',
                                           start = '1990-01-01', 
                                           end = '2000-01-01',
                                           format = 'yyyy'
                            ), #END of date range
                            ), # END of sidebarPanel
               mainPanel('Fire Perimeter Map',
                         helpText('The map displays fire perimeters where color indicates change in live fuel moisture (LFM). Warmer colors represent lower LFM (more flammable) and cooler colors represent higher LFM (less flammable).'),
                       plotOutput('fire_map'),
                       'Data Summary',
                       helpText(' '),
                       'National Live Fuel Moisture Database: ',
                       helpText('Provides a record of point samples of LFM across the United States. LFM data used in this study was restricted to Chamise *Adenstoma fasciculatum*, a dominant shrub in CA chaparral.'),
                       helpText('USFS Wildland Fire Assessment System. (n.d.). National Fuel Moisture Database. United States Forest Service. Retrieved date from https://www.wfas.net/index.php/national-fuel-moisture-database-moisture-drought-103'),
                       helpText(' '),
                       'Fire Perimeters: ',
                       helpText('Fire perimeter data for the state of CA from 1950 to 2017. Comprehensive record of fires across CA.'),
                       helpText('CA Fire and Resource Assessment Program. (2017). CA Department of Forestry and Fire Protection. Retrieved date from https://frap.fire.ca.gov/frap-projects/fire-perimeters/'),
                       'Ecoregion 261 A&B: ',
                       helpText('Ecoregion 261 is defined as the CA Coastal CHaparral Forest and Shrub Province. This region experiences a Mediterreanean climate and is characterized by fire-adapted shrublands. Fire perimeter data fwas restricted to Ecoregion 261 A&B to assume LFM of Chamise *Adenstoma fasciculatum* as representative.'),
                       helpText('McNab, W. H., Cleland, D. T., Freeouf, J. A., Keys Jr., J. E., Nowacki, G. J., & Carpenter, C. A. (2007). Description of “Ecological subregions: Sections of the conterminous United States” (first approximation). 242. https://www.treesearch.fs.fed.us/pubs/48672%0A')
                       ) # END of mainPanel
             ) # END of sidebarLayout
    ), # END of tabPanel for widget 1
    
    tabPanel('AV FIRE SIZE',
             sidebarLayout(
               sidebarPanel('Average Fire Size',
                            helpText('This plot displays the average fire size within each step increment increase in LFM. It is expected that as LFM decreases fire size will increase.'),
                            # DROPDOWN SELECT
                            selectInput(inputId = 'lfm_step',
                                        label = 'Select LFM Step Size',
                                        choices = c('1 %'=1.,'5 %'=5.,'10 %'=10., '20 %'=20., '50 %'=50.))
               ), # END sidebar panel 
               mainPanel(plotOutput(outputId='mean_fire_plot')
               ) # END of mainPanel
             )), # END of tabPanel for widget 2
    
    tabPanel('FIRE HISTOGRAM',
             sidebarLayout(
               sidebarPanel('Fire Histogram', 
                            helpText('This plot displays the number of fires for each step increment increase in LFM. It is expected that as LFM decreases number of fires will increase.'), 
                            helpText('Select a minimum fire size to see how the plot changes if fires are restricted to only larger fires.'),
                            # RADIO BUTTONS:
                            radioButtons(inputId='large_fire',
                                         label='Choose a Minimum Fire Size:',
                                         choices=c('No minimum fire size'=0.,'1 km^2'=1.,'5 km^2'=5.,'10 km^2'=10.)),
                            # SLIDER:
                            sliderInput(inputId='bins',
                                        label='Choose Number of Bins:',
                                        min=1,
                                        max=100,
                                        value=30)
               ), # END sidebar panel 
               mainPanel(plotOutput(outputId='lfm_hist')
               ) # END of mainPanel
             )), # END of tabPanel for widget 3
    
    tabPanel('CA MODEL DEMO',
             sidebarLayout(
               sidebarPanel('Demonstration of CA Fire Spread Model',
                            helpText('A cellular automata model was developed to explore how fire moves through vegetation under different conditions.'),
                            'Cell States:',
                            helpText('Red - active fire'),
                            helpText('Black - burned'),
                            helpText('Grey - unburnable'),
                            helpText('Green Gradient - vegetation where shade represents fuel moisture, yellow=highest probability of spread'),
                            # RADIO BUTTONS:
                            radioButtons(inputId='n',
                                         label='Choose Initial Conditions:',
                                         choices=c('Probability of Spread Varied'='ProbSpread','Duration of Combustion Varied'='StepsBurn','Combined Prob. Spread and Duration Varied'='Combined','Simple CA Demonstration'='Simple')),
               ), # END of sidebarPanel
               mainPanel(plotOutput('flam_gif')
               ) # END of mainPanel
             ) # END of sidebarLayout
    ) # END of tabPanel for widget 4
    
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
    plot(eco261ab_cent_rec_dateRed()['LFM_Av20km'], 
         col=brewer.pal(n=10,name='RdYlGn'),
         main='CA FRAP Fire Perimeters with LFM')
  }) # END fire_map output
  
  # WIDGET 2
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
         main='Average Fire Size',
         xlab='LFM',
         ylab='Average Fire Size',
         pch=16,
         cex=1.7,
         col='dark red')
  }) # END mean_fire_plot output
  
  # WIDGET 3
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
  
  # WIDGET 4
  # #flam_gif
  output$flam_gif <- renderImage({
    filename <- here(paste('gifs/',input$n, '.gif', sep=''))
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Gif Name", input$n))
  }, deleteFile = FALSE)
  
} # END SERVER



# COMBINE UI & SERVER INTO APP
shinyApp(ui=ui, server=server)







