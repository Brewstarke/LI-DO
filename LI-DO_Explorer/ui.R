#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(jsonlite)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(shiny)
library(dygraphs)
library(xts)
library(magrittr)
library(dataRetrieval)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(sp)
library(maptools)
library(htmltools)


# Define UI for application that draws a histogram
shinyUI(navbarPage("Long Island DO Monitoring", theme = "www/bootstrap.css",
                   tabPanel("Monitoring Stations"),
                    fluidPage(
                    	fluidRow(
                	  column(12,
                	         tags$h5("Data presented within this explorer have been made available through the", 
                	         	a(href = 'http://waterdata.usgs.gov/nwis', "USGS NWIS Web Interface:")
                	         ),
                                leafletOutput("siteMap"))
	                    ),
	                   fluidRow(
	                     column(6,
	                            h4("Time Series Plot"),
	                            dygraphOutput("tsPlots", width = '100%', height = 400)
	                            ),
	                     column(6,
	                            h4("Raster Plot"),
	                            plotOutput("DO_raster"))
	                     )
                     
                	   )
)
)

