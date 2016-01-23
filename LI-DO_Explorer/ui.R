#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

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


# Define UI for application that draws a histogram
shinyUI(navbarPage("Long Island DO Monitoring", theme = "www/bootstrap.css",
                   tabPanel("Monitoring Stations"),
                    fluidRow(
                        column(12,
                                leafletOutput("siteMap"))
                    ),
                   fluidRow(
                     column(6,
                            h3("Time Series Plot"),
                            dygraphOutput("tsPlots", width = '100%', height = 400)
                                   ),
                     column(6,
                            h4("Raster Plot"),
                            renderPlot("DO_raster")),
                     column(12, 
                            verbatimTextOutput('maptext'))
                     
                     )
))
