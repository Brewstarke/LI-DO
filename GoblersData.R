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
library(dataRetrieval)
library(leaflet)
library(DT)
library(RColorBrewer)
library(rgdal)
library(sp)
library(maptools)
library(metricsgraphics)
library(readr)


# ----

DO_Data <- read.csv("~/R_Code/DO_monitoring/GoblersData_DONOTSHARE.csv", header = TRUE, stringsAsFactors = FALSE)

DO_Data$DateTime <- as.POSIXct(DO_Data$DateTime, format = '%m/%d/%Y %H:%M', tz = "GMT")  # Change to proper date format and correct timezone
DO_Data$DateTime <- format(DO_Data$DateTime, tz = "America/New_York", usetz = TRUE)
DO_Data$DateTime <- as.POSIXct(DO_Data$DateTime, tz = 'EST')


DO_Data_wide <- DO_Data %>% 
	as.tbl() %>% 
	filter(DO_conc_mgperL != "NA" | Temp_C != "NA") %>% 
	# filter(Region == 'South Shore and Peconics') %>% 
	select(DateTime:DO_conc_mgperL, SiteName) %>%
	spread(SiteName, DO_conc_mgperL)

DO_Data_trim <- DO_Data %>% 
	as.tbl() %>% 
	filter(DO_conc_mgperL != "NA" | Temp_C != "NA") %>% 
	# filter(Region == 'South Shore and Peconics') %>% 
	select(DateTime:DO_conc_mgperL, SiteName)

DO_Data_xts <- xts(x = DO_Data_wide[,2], order.by = DO_Data_wide$DateTime, unique = TRUE)

DO_xts <- xts(x = DO_Data_trim[,2:3], order.by = DO_Data_trim$DateTime, unique = TRUE)


dygraph(DO_xts) %>%
	dyRangeSelector() %>% 
	dyHighlight(highlightSeriesBackgroundAlpha = 0.2) %>% 
	dyRoller(showRoller = TRUE) %>% 
	dyShading(from = 0, to = 3, axis = "y", color = "red")

# Goblers Data ----
LIMMN_DO_Data_2014 <- read_csv("C:/Users/astarke/Desktop/Box Sync/DO_monitoring_GSB/GoblersData/LIMMN_DO_Data_2014 corrected for salinity-From Gobler.csv")
Locations <- read.csv("C:/Users/astarke/Desktop/Box Sync/WI_PWL_data_Sept2015_submission/Locations.csv", stringsAsFactors=FALSE)
# Site MBC has many issues-- duplicates etc. Not sure which to keep so dropping the whole site for now.
LIMMN_DO_Data_2014$DateTime_GMT <- mdy_hm(LIMMN_DO_Data_2014$DateTime_GMT, tz = "gmt")
ContinuousStations <- Locations %>% 
	filter(Sampling == "Continuous")

# FUnction to make dyPlot for outputs.
DO_dyplot <- function (siteID, siteName) {
  LIMMN_DO_Data_dyplot <- LIMMN_DO_Data_2014 %>%
  	# filter(complete.cases(.)) %>% 
  	filter(Site == siteID) %>% 
  	select(Temp_C, `DOmg/L`, DateTime_GMT) %>% 
  	xts(order.by = .$DateTime_GMT)
  DOdyPlot <- NULL
  DOdyPlot <- LIMMN_DO_Data_dyplot[,1:2] %>% 
  	dygraph(main = paste(siteName, " Dissolved Oxygen and Temperature - Summer 2014", sep = "")) %>%
  	dySeries("DOmg/L", axis = 'y', label = "Dissolved Oxygen Concentration") %>% 
  	dySeries("Temp_C", axis = "y2", label = "Temperature") %>% 
  	dyHighlight(highlightSeriesBackgroundAlpha = 0.9) %>% 
  	dyAxis("y", label = "DO Concentration (mg/L)", valueRange = c(0, 15)) %>%
  	dyAxis("y2", label = "Temperature (C)") %>% 
  	dyShading(from = 0, to = 4.8, axis = "y", color = "#ffe8d1") %>% 
  	dyLegend(width = 400, labelsSeparateLines = TRUE, show = "always", hideOnMouseOut = TRUE) %>% 
  	dyOptions(useDataTimezone = FALSE, colors = RColorBrewer::brewer.pal(12, "Paired")[2:3])
  	
  DOdyPlot
}


DO_dyplot("PR", "Peconic River") # listed for DO
DO_dyplot("MHC", "Meeting House Creek") # listed for DO
DO_dyplot("FR", "Forge River") 
# North Shore Sites
DO_dyplot("CSH", "Cold Spring Harbor")
DO_dyplot("Hunt", "Huntington Harbor")
DO_dyplot("NPH", "Northport Harbor")
DO_dyplot("SBH", "Stony Brook Harbor")
DO_dyplot("PJ", "Port Jeff Harbor")
DO_dyplot("OBH", "Oyster Bay Harbor")
DO_dyplot("MSH", "Mount Sinai Harbor")
DO_dyplot("Matti", "Mattituck")

# South shore bays
DO_dyplot("HB", "Hewlett Bay")
DO_dyplot("BP", "Bellport Bay")
DO_dyplot("PENN", "Penniman Creek")
DO_dyplot("TB", "Tiana Beach")
DO_dyplot("MB", "Middle Bay")
DO_dyplot("NCB", "Great River")


DO_dyplot("STC", "Seatuck Cover")
DO_dyplot("QTK", "Quantuck Bay")
DO_dyplot("3MI", "Three Mile Harbor (3MI)")
DO_dyplot("3MH", "Three Mile Harbor (3MH)")




DO1 %>% dyShading(from = 0, to = 4.8, axis = 'y', color = "#ffe8d1") %>% 
	dyAxis("y", label = "DO Concentration (mg/L)", valueRange = c(0, 15))
DO2 %>% dyShading(from = 0, to = 4.8, axis = 'y', color = "#ffe8d1") %>% 
	dyAxis("y", label = "DO Concentration (mg/L)", valueRange = c(0, 15))
DO3 %>% dyShading(from = 0, to = 4.8, axis = 'y', color = "#ffe8d1") %>% 
	dyAxis("y", label = "DO Concentration (mg/L)", valueRange = c(0, 15))

