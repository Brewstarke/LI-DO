library(devtools)
library("d3heatmap", lib.loc="~/R/win-library/3.1")

cleanDOdata <- alldata %>% 
	select(-starts_with("Start")) %>% 
	select(-starts_with("End")) %>% 
	select(-starts_with("Coupler")) %>% 
	select(-starts_with("Host")) %>% 
	select(-starts_with("Stopped")) %>% 
	select(Date.Time, starts_with("DO")) %>% 
	filter(complete.cases(.))

rankDOdata <- DOdata %>% 
	select(dateTime, DO_0.5m) %>% 
	separate(dateTime, c("Date", "Time"), sep = " ", remove = FALSE) %>% 
	mutate(cumDist = cume_dist(DO_0.5m)) %>% 
	ggvis(~cumDist, ~DO_0.5m) %>% 
	layer_lines(stroke = ~dateTime)

rankDOdata


# From:
# https://govt.westlaw.com/nycrr/Document/I4ed90412cd1711dda432a117e6e0f345?viewType=FullText&originationContext=documenttoc&transitionType=CategoryPageItem&contextData=(sc.Default)

# Dissolved oxygen (DO)	A-Special	
# In rivers and upper waters of lakes, not less than 6.0 mg/L at any time. 
# In hypolimnetic waters, it should not be less than necessary 
# for the support of fishlife, particularly cold water species.
# AA, A, B, C, AA-Special	
# For trout spawning waters (TS) the DO concentration shall 
# not be less than 7.0 mg/L from other than natural conditions. 
# For trout waters (T), the minimum daily average shall not be less 
# than 6.0 mg/L, and at no time shall the concentration be less 
# than 5.0 mg/L. For nontrout waters, the minimum daily average 
# shall not be less than 5.0 mg/L, and at no time shall the 
# DO concentration be less than 4.0 mg/ L.
# D	Shall not be less than 3.0 mg/L at any time.
# SA, SB, SC	Chronic: Shall not be less than a daily average of 4.8 mg/L*
# 	
# Remark: 
# *The DO concentration may fall below 4.8 mg/L for a limited number of days, as defined by the formula:
# 	DOi = 13.0/2.80 + 1.84e-0.1ti
# where DOi = DO concentration in mg/L between 3.0-4.8 mg/L 
# and ti = time in days. 
# This equation is applied by dividing the DO range of 3.0-4.8 mg/L 
# into a number of equal intervals. DOi is the lower bound of 
# each interval (i) and ti is the allowable number of days that 
# the DO concentration can be within that interval. 

# The actual number of days that the measured DO concentration falls 
# within each interval (i) is divided by the allowable number of days 
# that the DO can fall within interval (ti). The sum of the quotients 
# of all intervals (i...n) cannot exceed 1.0: i.e.,
# The DO concentration shall not fall below the acute standard of 3.0  mg/L 
# at any time.
# SA, SB, SC, SD	Acute: Shall not be less than 3.0 mg/L at any time.
# I	Shall not be less than 4.0 mg/L at any time.
# 
# 
# 

# RasterPlots ----

DO_Raster <- function(data, station){
  library(lubridate)
  library(scales)
  data %>% 
	filter(station_nm == station) %>%
	select(dateTimeLocal, DO_0.5m) %>% 
	gather(depth, DO, -dateTimeLocal) %>% 
	separate(dateTimeLocal, into = c("Date", "Time"), sep = " ") %>% 
	mutate(Time = as.POSIXct(.$Time, format = "%H:%M:%S", tz = 'GMT')) %>%
	ggplot(aes(y = Date, x = Time, fill = DO)) +
	ggtitle(station) +
	geom_raster(hjust = 0, vjust = 0) +
	scale_fill_gradient(low = 'red', high = 'green') +
	scale_x_datetime(breaks = date_breaks('1 hour'), labels = date_format("%H"))
}

DO_Raster(DOdata, 'NORTHPORT HARBOR AT NORTHPORT NY')
DO_Raster(DOdata, "HUNTINGTON HARBOR AT HUNTINGTON NY")

d3heatmap(DOdata, scales = 'column', colors = 'X_00300_00011')

# ggplot Raster plot
# "MIDDLE BAY AT BALDWIN NY"                    
# "FIRE ISLAND INLET AT BABYLON NY"             
# "HEWLETT BAY AT EAST ROCKAWAY NY"             
# "GREAT SOUTH BAY 2 of 3 NEAR WEST SAYVILLE NY"
# "GREAT SOUTH BAY 3 of 3 NEAR SAYVILLE NY"     
# "GREAT SOUTH BAY 1 of 3 NEAR GREAT RIVER NY"  
# "NICOLL BAY AT OAKDALE NY"                    
# "BELLPORT BAY AT BELLPORT NY"                 
# "HEMPSTEAD HARBOR AT SEA CLIFF NY"            
#  "COLD SPRING HARBOR AT COLD SPRING HARBOR NY" 
#  "OYSTER BAY HARBOR AT OYSTER BAY NY"          
#  "HUNTINGTON HARBOR AT HUNTINGTON NY"          
#  "NORTHPORT HARBOR AT NORTHPORT NY"  
# 
DOdata %>% 
  filter(station_nm == "BELLPORT BAY AT BELLPORT NY") %>%
  # rename('DO 0.5m' = DO_0.5m) %>%
  select(dateTime, starts_with("DO"), station_nm)   %>% 
	select(dateTime, DO_0.5m) %>% 
	gather(depth, DO, -dateTime) %>% 
	separate(dateTime, into = c("Date", "Time"), sep = " ") %>% 
	mutate(Time = as.POSIXct(.$Time, format = "%H:%M:%S", tz = 'GMT')) 
%>%
	ggplot(aes(y = Date, x = Time, fill = DO)) +
	# ggtitle(station) +
	geom_raster(hjust = 0, vjust = 0) +
	scale_fill_gradient(low = 'red', high = 'green') +
	scale_x_datetime(breaks = date_breaks('1 hour'), labels = date_format("%H"))


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


a <- DOdata %>% 
	select(dateTime,station_nm, DO_0.5m) 
%>% 
	separate(dateTime, into = c("Date", "Time"), sep = " ") %>% 
	mutate(Time = as.POSIXct(.$Time, format = "%H:%M:%S", tz = 'GMT'))


a %>% group_by(station_nm) %>% tally()


c <- DOdata %>% 
	filter(station_nm == "GREAT SOUTH BAY 2 of 3 NEAR WEST SAYVILLE NY") %>%
	# rename('DO 0.5m' = DO_0.5m) %>%
	# select(dateTime, starts_with("DO"), station_nm)   %>% 
	select(dateTime, DO_0.5m) %>% 
	gather(depth, DO, -dateTime) %>% 
	separate(dateTime, into = c("Date", "Time"), sep = " ") %>% 
	mutate(Time = as.POSIXct(.$Time, format = "%H:%M:%S", tz = 'GMT')) 
c %>% 	ggplot(aes(y = Date, x = Time, fill = DO)) +
	# ggtitle(station) +
	geom_raster(hjust = 0, vjust = 0) +
	scale_fill_gradient(low = 'red', high = 'green') +
	scale_x_datetime(breaks = date_breaks('1 hour'), labels = date_format("%H"))



d <- DOdata %>% 
	filter(station_nm == "BELLPORT BAY AT BELLPORT NY") %>%
	# rename('DO 0.5m' = DO_0.5m) %>%
	# select(dateTime, starts_with("DO"), station_nm)   %>% 
	select(dateTime, DO_0.5m) %>% 
	gather(depth, DO, -dateTime) %>% 
	separate(dateTime, into = c("Date", "Time"), sep = " ") %>% 
	mutate(Time = as.POSIXct(.$Time, format = "%H:%M:%S", tz = 'GMT')) 

d %>% 
	ggplot(aes(y = Date, x = Time, fill = DO)) +
	# ggtitle(station) +
	geom_raster(hjust = 0, vjust = 0) +
	scale_fill_gradient(low = 'red', high = 'green') +
	scale_x_datetime(breaks = date_breaks('1 hour'), labels = date_format("%H"))




