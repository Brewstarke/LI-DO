library(devtools)
install_github('rCharts', 'ramnathv')
library(rCharts)

cleanDOdata <- alldata %>% 
	select(-starts_with("Start")) %>% 
	select(-starts_with("End")) %>% 
	select(-starts_with("Coupler")) %>% 
	select(-starts_with("Host")) %>% 
	select(-starts_with("Stopped")) %>% 
	select(Date.Time, starts_with("DO")) %>% 
	filter(complete.cases(.))

rankDOdata <- cleanDOdata %>% 
	select(Date.Time, DO.1.lower) %>% 
	separate(Date.Time, c("Date", "Time"), sep = " ") %>% 
	mutate(cumDist = cume_dist(DO.1.lower)) %>% 
	ggvis(~cumDist, ~DO.1.lower) %>% 
	layer_lines(stroke = ~Date.Time)

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
