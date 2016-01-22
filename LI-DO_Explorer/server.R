#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Packages ----
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(shiny)
library(ggvis)
library(dygraphs)
library(xts)
library(magrittr)
library(dataRetrieval)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(sp)
library(maptools)
library(metricsgraphics)
library(htmltools)

load('appData.RData')


# Globals - pre-load data from USGS site ----
# 
# siteIDs <- c('405004073391001', 
#              '405240073314901', 
#              '405232073281801', 
#              '405318073250101', 
#              '405329073212601', 
#              '403739073401001', 
#              '403731073353801', 
#              '403734073153401', 
#              '404513072555701', 
#              '404338073082101', 
#              '404327073072701', 
#              '404149073051301', 
#              '404153073030701')
# 
# 
# siteNames <- c('Hemp', 
#                'OBH', 
#                'CSH', 
#                'Hunt', 
#                'NPH', 
#                'HB', 
#                'MB', 
#                'FI', 
#                'BP', 
#                'NCB', 
#                'GSB1', 
#                'GSB2', 
#                'GSB3')
# 
# names(siteIDs) <- siteNames
# 
# 
# DOparameter <- "00300"
# 
# 
# DOdata <- readNWISuv(siteIDs, DOparameter, "", "")
# 
# siteInfo <- attr(DO, "siteInfo")
# 
# siteLocations <- siteInfo %>% 
#   select(station_nm, site_no, starts_with("dec")) %>% 
#   rename(Lat = dec_lat_va, Lon = dec_lon_va)
# 
# DOdata %>% left_join(siteLocations, by = 'site_no') -> DOdata

DOdata %<>% mutate(DO_0.5m = ifelse(is.na(X_00300_00011), X_0.5m.above.seabed_00300_00011, X_00300_00011))


bluepoints <- readOGR(".","BluepointsProperty", encoding = "ESRI Shapefile")

bluepoints <- spTransform(bluepoints, CRS("+init=epsg:4326"))


# Define server ----
shinyServer(function(input, output) {
  
  output$siteMap <- renderLeaflet({ 
# Leaflet Map ----
    DO_map <- leaflet(siteLocations) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(layerId = ~station_nm, popup = ~htmlEscape(station_nm), options = popupOptions(zoomAnimation = TRUE, closeOnClick = FALSE)) %>% 
      addPolygons(data = bluepoints, fill = FALSE, weight = 3) 
    DO_map
    
  })
  
# dyGraph data munge
    
    
  dyData <- reactive({
    DOdata %>% filter(station_nm == input$siteMap_marker_click) %>%
      rename('DO 0.5m' = DO_0.5m) %>%
      select(dateTime, starts_with("DO"))  
  })
  
# Testing marker click output----
  output$maptext <- renderText(input$siteMap_marker_click$id)
  
# Plot Function ----
  output$tsPlots <- renderDygraph({
    if(is.null(input$siteMap_marker_click))
      return(NULL)
    
    dyData() %>%
      xts(order.by = .$dateTime) %>% 
      dygraph(., main = input$siteMap_marker_click$id) %>%
      dyAxis("y", valueRange = c(-1, 11)) %>%
      dySeries('DO 0.5m', 'DO 0.5m') %>%
      dyRangeSelector(retainDateWindow = FALSE) %>%
      dyOptions(useDataTimezone = FALSE) %>% 
      dyShading(from = 2.3, to = -1, axis = "y", color = "#ffd1d1") %>% 
      dyShading(from = 2.3, to = 4.8, axis = 'y', color = "#ffe8d1")
    
    
  })
  
})
