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
shinyUI(navbarPage("Long Island DO Monitoring", 
                   tabPanel("Monitoring Stations"),
                    fluidRow(
                        column(12,
                                leafletOutput("siteMap"))
                    ),
                   fluidRow(
                     column(8,
                            h3("Plots and plots..."),
                            dygraphOutput("tsPlots")
                                   ),
                     column(3,
                            h4("descriptions or sliders...")),
                     column(12, 
                            verbatimTextOutput('maptext'))
                     
                     ),
                  tabPanel("Site Comparisons"),
                   
                  tabPanel("Quanitying Impacts")
))