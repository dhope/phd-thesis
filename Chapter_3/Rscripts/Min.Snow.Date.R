# library(MASS)
library(tidyverse)
library(foreign)
#library(NCStats)
library(vcd)## loading vcd package
library(nortest)
library(lattice)
# require(plyr)
library(dplyr)
require(dichromat)
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Data from http://www.ncdc.noaa.gov/cdo-web/datasets

require(lubridate)
# Import Data
snow.raw <- read_csv("~/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/Snowmelt/wesa.snowmelt.raw.1924.2015.csv",col_types = cols())%>%
        select(STATION, STATION_NAME, LATITUDE, LONGITUDE,  DATE, PRCP, SNWD) %>% #wesa.snowmelt.raw.data.csv")
        mutate(LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE))
snow.raw2016 <- read_csv("~/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/Snowmelt/wesa.snowmelt.raw.2016.csv",col_types = cols()) %>%
      select(STATION, STATION_NAME, LATITUDE, LONGITUDE,  DATE, PRCP, SNWD)
snow.raw2017 <- read_csv("~/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/Snowmelt/wesa.snowmelt.raw.2017.csv",
	col_types = cols()) %>%
  select(STATION, NAME, LATITUDE, LONGITUDE, DATE, PRCP, SNWD) %>% dplyr::rename(STATION_NAME= NAME) %>% 
  filter(SNWD!=-9999) %>% mutate(Year = as.character(year(DATE)),Month=as.character(month(DATE)), 
                                 Day = as.character(day(DATE)), Date=as_date(DATE),
                                 DayofYear = yday(Date)) %>% filter(DayofYear <=200) %>% select(-DATE) %>% 
  mutate(Date=as.character(Date),
         STATION_NAME = ifelse(STATION_NAME=="NOME MUNICIPAL AIRPORT, AK US", "NOME MUNICIPAL AIRPORT AK US", 
                               ifelse(STATION_NAME=="KOTZEBUE RALPH WEIN MEMORIAL AIRPORT, AK US","KOTZEBUE RALPH WEIN MEMORIAL AIRPORT AK",
                                      ifelse(STATION_NAME=="BETHEL AIRPORT, AK US", "BETHEL AIRPORT AK US", "ERROR"))),
         STATION = paste0("GHCND:",STATION))

plot2017 <- ggplot(snow.raw2017, aes(DayofYear, SNWD, colour = STATION))+ geom_point()

included.sites <- unique(snow.raw$STATION_NAME)#[c(1,3)]
# Clean the data
snow <- snow.raw %>%
          bind_rows(snow.raw2016) %>%
        filter(STATION_NAME %in% included.sites) %>%
				filter(SNWD != -9999) %>% # Remove errors
				# select(STATION,	STATION_NAME,	ELEVATION,	LATITUDE,	LONGITUDE,	DATE,	PRCP, SNWD) %>%
				mutate(Year = substr(DATE, 1, 4), # Break down Day, month,year
         				Month = substr(DATE, 5,6),
         				Day = substr(DATE, 7, 8)) %>%
				mutate(Date = paste(Month, "/", Day, "/", Year, sep = ""), # Add day of year
						DayofYear = strptime(Date, "%m/%d/%Y")$yday+1) %>%
				filter(DayofYear <=200) %>% select(-DATE) %>% 
  bind_rows(snow.raw2017)





# ggplot(snow, aes(STATION_NAME, SNWD, colour = Year)) + geom_boxplot()
# 
# 
# 
# stations <- unique(snow$STATION_NAME)
# snow <- subset(snow, DayofYear <= 180)
# 
# 
# require(ggplot2)
# snowmelt.plt <- ggplot(filter(snow, Year == 1998), aes(DayofYear, SNWD))
# snowmelt.plt + geom_point(aes(colour = STATION_NAME)) + geom_smooth() + geom_line(aes(y = 12.5)) + 
#   facet_grid(Year~.) + ylim(0,150)
# 
# 
# #xyplot(SNWD~DayofYear|Year, data = snow, pch = 19, type = c("p", "smooth"), ylim = c(0,100))
# output = list()
# for (i in 1:length(stations)) {
# 	coeffmatrix <- matrix(NA, nrow=500, ncol=4, dimnames = list(c(1:500),
#                                                    c("Station", "YEAR", "LastDayofSnow", "SNWD")))
# 	station.i <- stations[i] 
#   print(paste(station.i, ' - ', i))
# 	site <- filter(snow, STATION_NAME == station.i & DayofYear > 80)
# 	yr <- unique(site$Year)
# 	for (j in 1:length(yr)) {
# 		yr.i <- yr[j]
# 		yr.site <- subset(site, Year == yr.i) #& SNWD <= 1000)
# 		if (min(yr.site$SNWD, na.rm=T) <= 5500 & min(yr.site$DayofYear, na.rm=T) < 90 & max(yr.site$DayofYear, na.rm=T) > 160){
# 		snowfree<- yr.site[yr.site$SNWD >=25.0,]#>= 12.5,] # Subset to days above 125mm depth
# 		mnday <- snowfree[snowfree$DayofYear == max(snowfree$DayofYear, na.rm=T),] # Extract last day above 125mm
# 		#print(mnday$DayofYear); print(mnday$SNWD)
#     coeffmatrix[j,3] <-mnday$DayofYear#[1] # add to output matrix
# 		coeffmatrix[j,4] <- mnday$SNWD#[1]
# 		} else {}# cat('ERROR: ',yr.i, '  ', 'station.i  - ',  min(yr.site$SNWD, na.rm=T) )}
# 		coeffmatrix[j,1] <- paste(station.i)
# 		coeffmatrix[j,2] <- yr.i
# 		#print(min(yr.site$SNWD, na.rm=T))
# 		}
# 		x1 <- data.frame(coeffmatrix)
#         #exportname <- paste(station.i,".csv",sep="")
#         #write.table(x1, file = exportname, sep = ",", append = F)
#         output[[station.i]] <- x1
#         }
# 

snowmelt.by.site <- 
  snow %>%   group_by(STATION_NAME, Year) %>% mutate(n = n(), min.d = min(DayofYear), max.d = max(DayofYear), range = max.d-min.d) %>%
  # filter(SNWD < 25.0 & range == 199& DayofYear > 80) %>%
  filter(SNWD>12.5) %>%
  slice(which.max(DayofYear)) %>%
  group_by(STATION_NAME, Year) %>% mutate(snowmelt = min(DayofYear)) %>% filter(DayofYear == snowmelt) %>%
  select(STATION_NAME, Date, Year, Month, Day, DayofYear, n, min.d, max.d, snowmelt, SNWD)

# 
# 
# 
# all.stations <- ldply(output, data.frame)
# all.stations <- filter(all.stations, !is.na(YEAR))
# all.stations$LastDayofSnow <- as.numeric(as.character(all.stations$LastDayofSnow))
# all.stations$SNWD <- as.numeric(as.character(all.stations$SNWD))
# write.table(all.stations, 'SnowmeltEstimates.txt', sep = '\t', quote=F)

require(ggplot2)
# plt.snowmelt <- ggplot(all.stations, aes(YEAR, LastDayofSnow, group = Station)) + geom_point(aes(colour = Station))
# plt.snowmelt 
# plt.2 <- ggplot(all.stations, aes(YEAR, LastDayofSnow)) + geom_boxplot()
# plt.2
library(dplyr)
station.dates <- snowmelt.by.site %>% filter(!is.na(snowmelt)) %>% select(Year, STATION_NAME, snowmelt) %>% 
  tidyr::spread(STATION_NAME, snowmelt) %>%
ungroup() %>%
  mutate(Year = as.numeric(as.character(Year)))
names(station.dates) <- c('Year', 'Bethel', 'Kotzebue', 'Nome')

mean.snowmelt <- snowmelt.by.site %>%
					group_by(Year) %>%
					dplyr::summarize(mean.snowmelt = mean(snowmelt), sd.snowmelt = sd(snowmelt)) %>%
          ungroup() %>%
					mutate(dev.snowmelt = mean.snowmelt - mean(mean.snowmelt, na.rm=T),
                 Year = as.numeric(as.character(Year)),
                 group = ifelse(Year == 2013, 'a', 
                                ifelse(Year ==2014, 'b', 
                                       ifelse(Year == 2015, 'c', 
                                              ifelse(Year==2016, 'd',
                                                     ifelse(Year==2017, 'e', "f")))))) %>% 
  filter(!is.na(mean.snowmelt)) %>%
  # left_join(station.dates, by = 'Year') %>% 
  mutate(method="snowpack",
         rMean = cummean(mean.snowmelt))

dev.snowmelt_bysite <- snowmelt.by.site %>%
  group_by(STATION_NAME) %>%
  mutate(dev.snowmelt = snowmelt - mean(snowmelt, na.rm=T),
         Year = as.numeric(as.character(Year))
         ) %>% ungroup %>% 
  dplyr::filter(!is.na(snowmelt)) %>% select(STATION_NAME, Year, snowmelt, dev.snowmelt)

# wesaVGmethod <- read.csv(file = "./VanGilsMethod/WESASnowmeltPhenology.csv", stringsAsFactors = F) %>% mutate(method="VG") %>%
#   rename(mean.snowmelt = yday.start, Year = year)
# 
# methodCompare <- bind_rows(mean.snowmelt, wesaVGmethod)
# 
# ggplot(methodCompare, aes(Year, mean.snowmelt, colour = method)) + geom_point() + geom_smooth(method='lm')
# left_join(mean.snowmelt, wesaVGmethod, by = "Year") %>% ggplot(aes(mean.snowmelt.x, mean.snowmelt.y)) +
#   geom_point() + labs(x = "Snowmelt Date from Snowdepth", y = "Snowmelt date from Sat") +
#   geom_abline(slope = 1, intercept = 0)

# require(ggthemes)
# dev.plt <- ggplot(mean.snowmelt, aes(Year, dev.snowmelt, fill = group)) +
#   geom_bar(stat='identity', position='dodge') +
#    ylab('Deviation from Mean Snowmelt Date (Days)')+
#    theme(text = element_text(size = 18), legend.position = 'none')
# ggsave('/home/dhope/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/graphs/colour/snowmelt_original.jpeg', height = 9, width = 12, plot = dev.plt)
# # Extract colours used in plot
# build.info <- ggplot_build(dev.plt)$data[[1]] %>% arrange(desc(x)) %>% filter(x > 2011) %>% select(fill)
# colours.plt <- as.vector(build.info[['fill']])
# # Plot as it would look for colour blind
# dev.plt +
#   scale_fill_manual(values=dichromat(colours.plt))
# ggsave('/home/dhope/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/graphs/colour/snowmelt_as_colourblind.jpeg', height = 9, width = 12)
# 
# # Colourblind friendly
# dev.plt + scale_fill_manual(values =cbbPalette)
# ggsave('/home/dhope/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/graphs/colour/snowmelt_colourblind.jpeg', height = 9, width = 12)
# # ggplot(filter(mean.snowmelt, Year >= 2013), aes(Year, dev.snowmelt)) +
# #   geom_bar(stat='identity') + ylim(min(mean.snowmelt$dev.snowmelt), max(mean.snowmelt$dev.snowmelt)) + 
# #   theme_minimal()+ ylab('Deviation from Mean Snowmelt Date (Days)') +  theme(text = element_text(size = 18)) 
# 
# 
# dev.plt + xlim(c(2012, 2017.5)) + ylim(min(mean.snowmelt$dev.snowmelt), max(mean.snowmelt$dev.snowmelt)) + scale_fill_manual(values =cbbPalette)
# ggsave('/home/dhope/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/graphs/colour/snowmelt_colourblind_2013-2015.jpeg', height = 9, width = 12)
# 
# 
# 
# SnowdatesByStation <- 
# snow %>% filter(Year > 2012) %>%
#   ggplot( aes(DayofYear, SNWD, colour = STATION_NAME))+ geom_line() + facet_grid(Year~.) +
#   geom_vline(data = filter(mean.snowmelt,Year>2012), aes(xintercept = mean.snowmelt, colour = NULL ), colour = 'black') +
#   xlim(75, 160) +
#   geom_vline(data = filter(snowmelt.by.site, Year>2012), aes(xintercept=snowmelt, colour = STATION_NAME))
# ggsave('/home/dhope/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/graphs/colour/SnowByDay.jpeg', height = 9, width = 12)
