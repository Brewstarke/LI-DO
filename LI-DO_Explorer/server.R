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
library(scales)

load('appData.RData')


# Globals - pre-load data from USGS site ----

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
# siteInfo <- attr(DOdata, "siteInfo")
# 
# siteLocations <- siteInfo %>%
#   select(station_nm, site_no, starts_with("dec")) %>%
#   rename(Latitude = dec_lat_va, Longitude = dec_lon_va)
# 
# DOdata %>% left_join(siteLocations, by = 'site_no') -> DOdata
# 
# 
# DOdata %<>% mutate(DO_0.5m = ifelse(is.na(X_00300_00011), X_0.5m.above.seabed_00300_00011, X_00300_00011)) %>%
# 	select(dateTime, station_nm, DO_0.5m, Latitude, Longitude) %>%
#   rename(dateTimeGMT = dateTime) %>%
#   mutate(dateTime = as.POSIXct(format(.$dateTimeGMT, tz = "EST")))


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
    
    
  mappedData <- reactive({
    DOdata %>% 
  	filter(station_nm == input$siteMap_marker_click) %>%
	rename('DO 0.5m' = DO_0.5m) %>%
      	select(dateTime, starts_with("DO"), station_nm)  
  })
  
# Testing marker click output----
  output$maptext <- renderText(length(mappedData()$dateTime))
  
# Dyplot Time Series Plot Function ----
  output$tsPlots <- renderDygraph({
    if(is.null(input$siteMap_marker_click))
      return(NULL)
    
  mappedData() %>%
    	select(dateTime, `DO 0.5m`) %>% 
	xts(order.by = .$dateTime) %>% 
	dygraph(., main = input$siteMap_marker_click$id) %>%
	dyAxis("y", valueRange = c(-1, 11)) %>%
	dySeries() %>%
	dyRangeSelector(retainDateWindow = FALSE) %>%
	dyOptions(useDataTimezone = FALSE) %>% 
	dyShading(from = 2.3, to = -1, axis = "y", color = "#ffd1d1") %>% 
	dyShading(from = 2.3, to = 4.8, axis = 'y', color = "#ffe8d1")

  })
  
  
 # Raster Plot ---- 
  output$DO_raster <- renderPlot({
  	if(is.null(input$siteMap_marker_click))
  		return(NULL)
  	mappedData() %>% 
	  	select(dateTime, `DO 0.5m`) %>% 
	  	gather(depth, DO, -dateTime) %>% 
	  	separate(dateTime, into = c("Date", "Time"), sep = " ") %>% 
	  	mutate(Time = as.POSIXct(.$Time, format = "%H:%M:%S", tz = 'GMT')) %>%
		  	ggplot(aes(y = Date, x = Time, fill = DO)) +
		  	ggtitle(input$siteMap_marker_click$id) +
		  	geom_raster(interpolate = TRUE, hjust = 0, vjust = 0) +
		  	scale_fill_gradient(low = 'red', high = 'green') +
		  	scale_x_datetime(breaks = date_breaks('1 hour'), labels = date_format("%H"), expand = c(0, 0))+ 
  			theme_bw()
	})
  
})
