


# Station 1

DO1_static <- site1_Rawdata %>% 
	rename(DO_0.5m = X_0.5m.above.seabed_00300_00011) %>% 
	rename(DO_0.1m = X_0.1m.above.seabed_00300_00011) %>% 
	select(dateTime, starts_with("DO")) %>% 
	xts(order.by = .$dateTime)
	    
static_plot1 <- DO1_static[,2:3] %>% 
	dygraph(., main = "Station 1- Mouth of Connetquot River", group = "DO-stations") %>% 
	dyAxis("y", label = "DO mg/L", valueRange = c(0, 11)) %>%  
	# dyRangeSelector(retainDateWindow = FALSE) %>% 
	dyOptions(useDataTimezone = FALSE, colors = RColorBrewer::brewer.pal(12, "Paired")[1:2])
# Station 2

DO2_static <- site2_Rawdata %>% 
	rename(DO_0.5m = X_0.5m.above.seabed_00300_00011) %>% 
	rename(DO_0.1m = X_0.1m.above.seabed_00300_00011) %>% 
	select(dateTime, starts_with("DO")) %>% 
	xts(order.by = .$dateTime)

static_plot2 <- DO2_static[,2:3] %>% 
	dygraph(., main = "Station 2- Central Bluepoints Property", group = "DO-stations") %>% 
	dyAxis("y", label = "DO mg/L", valueRange = c(0, 11)) %>% 
	# dyRangeSelector(retainDateWindow = FALSE) %>% 
	dyOptions(useDataTimezone = FALSE, colors = RColorBrewer::brewer.pal(12, "Paired")[3:4])

static_plot2

# Station 3

DO3_static <- site3_Rawdata %>% 
	rename(DO_0.5m = X_0.5m.above.seabed_00300_00011) %>% 
	rename(DO_0.1m = X_0.1m.above.seabed_00300_00011) %>% 
	select(dateTime, starts_with("DO")) %>%
	xts(order.by = .$dateTime)

static_plot3 <- DO3_static[,2:3] %>% 
	dygraph(., main = "Station 3- Along Eastern Bluepoints Property Line", group = "DO-stations") %>% 
	dyAxis("y", label = "DO mg/L", valueRange = c(0, 11)) %>% 
	# dyRangeSelector(retainDateWindow = FALSE) %>% 
	dyOptions(useDataTimezone = FALSE, colors = RColorBrewer::brewer.pal(12, "Paired")[9:10])

static_plot3 




static_plot1 %>% dyShading(from = 2.3, to = -1, axis = "y", color = "#ffd1d1") %>% 
	dyShading(from = 2.3, to = 4.8, axis = 'y', color = "#ffe8d1")

static_plot2 %>% dyShading(from = 2.3, to = -1, axis = "y", color = "#ffd1d1") %>% 
	dyShading(from = 2.3, to = 4.8, axis = 'y', color = "#ffe8d1")

static_plot3  %>% dyShading(from = 2.3, to = -1, axis = "y", color = "#ffd1d1") %>% 
	dyShading(from = 2.3, to = 4.8, axis = 'y', color = "#ffe8d1")







