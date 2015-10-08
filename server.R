
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#### packages

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(ggthemes)
library(lubridate)
library(shiny)
library(xlsx)
library(ggvis)
library(dygraphs)
library(xts)
library(magrittr)
library(ggvis)

# Funciton curtosey Paolo Cavatore- designed to convert xts into useable form for dplyr functions.
# xts2df <- function(x) {
# 	data.frame(date=index(x), coredata(x))
# }
# df2xts <- function(x){
# 	xts(x = x[,-1], order.by = x[,1], unique = TRUE)
# }


server <- function(input, output, session) { 
	
	DO_data <- reactive({
		
		DO_data <-  DO_wide[input$sites] %>% 
			xts(order.by = DO_wide$DateTime)
	})
	
	output$message <- renderText({
		if(is.null(input$sites)) return ("Select Site to see DO trends")
		("DO Concentration (mg/L)")
	})
	
	
	dyplot <- reactive({
		dygraph(DO_data()) %>%
		dyRangeSelector() %>% 
		dyHighlight(highlightSeriesBackgroundAlpha = 0.2) %>% 
		dyRoller(showRoller = FALSE, rollPeriod = 4 * input$slider) %>% 
		dyAxis("y", label = "Dissolved Oxygen Concentration (mg/L)", valueRange = c(0, 15)) %>%
		dyShading(from = 0, to = 3, axis = "y", color = "#f2c9cc") %>% 
		dyLegend(width = 400, labelsSeparateLines = TRUE)
	})
	
	output$timeseries <- renderDygraph({
		if (is.null(input$sites)) return(NULL)
		dyplot()
	
	})
	
	# Breakdown of daily processes.
	DO_daily <- DO_wide %>% 
		separate(col = DateTime, into = c("Year", "Month", "Day"), sep = "-") %>% 
		separate(col = Day, into = c('Day','Time'), sep = " ")
	
	DO_daily %>% ggplot(mapping = aes(x = Time, y = Bellport, group = Day)) + geom_line() + facet_grid(Month ~ .)
		
	
}
