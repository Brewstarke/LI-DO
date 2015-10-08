


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



## Cumulative Plots:


DO_CumulativePlot <- function(data, site){
	cumdata <- data %>% 
		rename(DO_0.5m = X_0.5m.above.seabed_00300_00011) %>% 
		select(DO_0.5m) %>% 
		filter(complete.cases(.)) %>% 
		mutate(cumDistDO_0.5m = cume_dist(DO_0.5m)*100) %>% 
		mutate(O2_criteria = ifelse(DO_0.5m <= 2.3, 
					    "Survival in jeopordy", 
					    ifelse(DO_0.5m >= 4.8, "No harm", "Growth hindered"))) %>% 
		mutate(O2_criteria = factor(O2_criteria, levels = c("Survival in jeopordy", "Growth hindered", "No harm"))) %>% 
		ggplot(aes(y = DO_0.5m, x = cumDistDO_0.5m))+
		geom_ribbon(aes(ymin = 0, ymax = DO_0.5m, fill = O2_criteria)) +
		theme_bw() + 
		scale_fill_manual(values = c('#FFB4A2', '#FCDDBC', '#B8D8BA')) +
		labs(x = "% of total time", y = "DO concentration (mg/L)", 
		     fill = "Effects of O2 concentration", title = paste("Impacts of Oxygen Limitations -", site, sep = " ")) 
	cumdata
}

DO_CumulativePlot(site1_Rawdata, "Site 1")
DO_CumulativePlot(site2_Rawdata, "Site 2")
DO_CumulativePlot(site3_Rawdata, "Site 3")






