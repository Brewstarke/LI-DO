
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
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

load('.Rdata')

dashboardPage(
	dashboardHeader(title = 'DO and LI'),
	dashboardSidebar(
		checkboxGroupInput(inputId = 'sites', label = 'Select Sites to Display', choices = names(DO_wide)[-1])
		),
	dashboardBody(
		fluidRow(
			box(width = 12, title = textOutput('message'), status = "primary", dygraphOutput("timeseries")),
			box(width = 6, title = 'Smooth Line', sliderInput(inputId = 'slider', label = "Apply a rolling average", min = 0, max = 100, step = 1, value = 1, post = ' hours'))
		)
	)
)


