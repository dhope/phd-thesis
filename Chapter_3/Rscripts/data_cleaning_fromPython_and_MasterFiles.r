### This script should take files output from compilingdata_fromMasterFiles.py, which 
# has been compiled and cleaned to some degree from the master files.
# the next step is to get the data cleaned and organized for analysis. So need to convert any written comments into numbers
# and remove anything that will throw off the analysis
# Author : David Hope
# Date: September 16, 2016
require(tidyverse)
# setwd('/home/dhope/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/Rscripts')
# rm(list = ls())
## Set file locations because Rstudio is stupid
filesource <- '../Python/output'
inputfiles <- '../Python/input/'
database.loc <- '../'

filenames <- list.files(filesource, pattern="*_13_14_15_16_17.txt", full.names=TRUE)
survey_data <- lapply(filenames, read.csv, sep = '\t', stringsAsFactor = FALSE)

extractdatesiteid <- function(x)
{
  mutate(x, SiteID = substr(RecordID, 1, 4), # Ensure siteid, and year month day arefilled in (they should be already)
         Year = substr(RecordID, 5, 8), 
         Month = substr(RecordID, 9,9),
         Day = substr(RecordID, 10, 11),
         Date = lubridate::mdy(paste(Month, Day, Year, sep="-")),
         doy = yday(Date))
}

# Custom function to extract just the data type from the file names
substrRight <- function(x, n){
  substr(x, nchar(filesource)+2, nchar(x)-19)
}

names(survey_data) <- 
  substrRight(filenames,33)

### 1. Discover counts of WESA that are not numbers
require(dplyr)
require(ggplot2)
require(tidyr)

# Filter out empty rows. Checked below
wesa_counts <- filter(survey_data$wesa_counts, RecordID != "")
siteinfo <- filter(survey_data$siteinfo, RecordID != "")
obsinfo <- filter(survey_data$obsinfo, RecordID != "")
falcon_counts <- filter(survey_data$falcon_counts, RecordID != "")


wesa_counts$countZeros <- wesa_counts$Count # Add new column to add zeros to empty columns
wesa_counts[wesa_counts[["Count"]] == "",][['countZeros']] <- 0 # Add zeros %>% unique()#View()
# wesa_counts %>% filter(is.na(as.numeric(countZeros))) %>% View() # View the NA
nasToFix <- wesa_counts %>% filter(is.na(as.numeric(countZeros))) %>% .[['countZeros']] %>% unique
# filter(survey_data$wesa_counts, RecordID == "BB922014719") %>% View
replacements <- vector("list", length = length(nasToFix))
names(replacements) <- nasToFix


### Manually replace data that was entered by idiots

replacements$Zero <- 0
replacements$`15 at West end of area` <- 15
replacements$`100-150` <- 125
replacements$Unknown <- NA
replacements$`15 at West end` <- 15
replacements$`9 (see notes)` <- 9
replacements$`15+` <- 15
replacements$`600 at East end of area` <- 600
replacements$`600 at East end` <- 600
replacements$`300 at West end` <- 300
replacements$`64 WESA` <- 64
replacements$`280 at West end` <- 280
replacements$`69 WESA` <- 69
replacements$`54 WESA` <- 54
replacements$`78 WESA` <- 78
replacements$`5/10 000` <- 7500
replacements$`5 WESA` <- 5
replacements$`300+` <- 300
replacements$`4 WESA` <- 4
replacements$`6 WESA` <- 6
replacements$`3 WESA` <- 3
replacements$`2 WESA` <- 2
replacements$`6:  4 WESA; 2 LESA` <- 4
replacements$`0 WESA; 0 LESA` <- 0
replacements$`1 LESA` <- 0
replacements$`4: 2 WESA; 2 LESA` <- 2
replacements$`1 WESA` <- 1
replacements$`4: 3 WESA; 1 LESA` <- 3
replacements$`3: 2 WESA; 1 LESA` <- 2
replacements$`0 WESA 0 LESA` <- 0
replacements$`5: 4 WESA; 1 LESA` <- 4
replacements$`6: 4 WESA; 2 LESA` <- 4
replacements$`0 WESA;  0 LESA` <- 0
replacements$`06 WESA` <- 6
replacements$`Estimated 30 peeps` <- 30
replacements$`2 (+2 LESA)` <- 2
replacements$`5 (+1 SESA)` <- 5
replacements$`9 (+1 SESA)` <- 9
replacements$`8 (+2 LESA)` <- 8
replacements$`>5500` <- 5500
replacements$`[285]*  225` <- 285
replacements$`[123]  73` <- 123
replacements$`[165] 105` <- 165
replacements$`[127] 67` <- 127
replacements$`[130]  90` <- 130
replacements$`[690] 260 Near (plus 430 Far)` <- 690
replacements$`[675] 200 near (plus 475 far)` <- 675
replacements$`[786] 186 near (plus 600 far)` <- 786
replacements$`[895] 205 near (plus 690 far)` <- 895
replacements$`[899] 169 near (plus 730 far)` <- 899
replacements$`[862] 152 near (plus 710 far)` <- 862
replacements$`[840] 360 near (plus 480 far` <- 840
replacements$`[1035] 275 near (plus 760 far)` <- 1035
replacements$`[860] 190 near (plus 670 far)` <- 860
replacements$`[1,515] 267 near (plus 1,250 far)` <- 1515
replacements$`[1,510] 300 near plus 1,210 far` <- 1510



# wesa_counts %>%
#   mutate(Year = substr(RecordID, 5, 8)) %>%
# filter(SiteID == "CROC" & Year == "2016") %>% View

replacements$`110 (WESA 43, LESA 67)` <- 43
replacements$`109 (WESA 33, LESA 76)` <- 33
replacements$`235 (WESA 108)` <- 108
replacements$`about 27` <- 27

replacements[sapply(replacements, is.null)]


# Introduce the cleaned counts
wesa_clean_numbers <- 
  wesa_counts %>%
  mutate(CountClean=ifelse(countZeros %in% nasToFix, replacements[countZeros], countZeros)) 
wesa_clean_numbers$CountClean <- as.numeric(wesa_clean_numbers$CountClean)
 

# Check data 
# filter(wesa_clean_numbers, is.na(as.numeric(as.character(CountClean)))) %>% View

# filter(wesa_clean_numbers, RecordID=="")  %>% select(ID) %>% as.vector() %>% unique()
# filter(survey_data$obsinfo, RecordID == "") %>% View
# filter(survey_data$falcon_counts, RecordID == "") %>% select(-Source, -X) %>% distinct %>% View

# Find the Records not in WESA counts
notbrokendownWESAcounts <- siteinfo %>%
  anti_join(wesa_counts, by = "RecordID") %>% filter(RecordID != "")

## Fix wording for idiots who entered wrong values in to did you see any birds?
siteinfo$YES_NO_WESA_Obs %>% unique()


correctUsages <- c("I did not observe any Western Sandpipers during the entire survey", 
                   "I saw at least 1 WESA during the survey" )
error2013 <- filter(siteinfo, YES_NO_WESA_Obs == "I did observe any Western Sandpipers during the entire survey") %>% 
                    select(RecordID) %>% left_join(wesa_clean_numbers)#Vie##w
# filter(siteinfo, YES_NO_WESA_Obs == "") %>% 
#   select(RecordID) %>% left_join(wesa_clean_numbers) %>%View
# Since are all zeros in error2013, can safely change to "I did not observe any WESA"
YN2Fix <- siteinfo %>% filter(!YES_NO_WESA_Obs %in% correctUsages ) %>% .[['YES_NO_WESA_Obs']] %>% unique


replacementsYN <- vector("list", length = length(YN2Fix))
names(replacementsYN) <- YN2Fix
replacementsYN$`I did observe any Western Sandpipers during the entire survey` <- correctUsages[1]
replacementsYN$Yes  <- correctUsages[2]
replacementsYN$`no peeps` <- correctUsages[1]
replacementsYN$No <- correctUsages[1]
# replacementsYN$`` <- NA

# Introduce the cleaned siteinfo
# siteinfo_clean <- 
#   siteinfo %>%
#   mutate(YES_NO_WESA_Obs=ifelse(YES_NO_WESA_Obs %in% YN2Fix, replacementsYN[YES_NO_WESA_Obs], YES_NO_WESA_Obs)) 
# unique(siteinfo_clean$YES_NO_WESA_Obs)
# siteinfo_clean$YES_NO_WESA_Obs <- as.character(siteinfo_clean$YES_NO_WESA_Obs)
# left_join(siteinfo_clean, siteinfo, by ='RecordID') %>% select(YES_NO_WESA_Obs.x, YES_NO_WESA_Obs.y) %>%
#   View

# Repeat for LESA/WESA Ratio
siteinfo$Ratio_LESA %>% unique
siteinfo$Ratio_WESA %>% unique

# siteinfo %>% filter(Ratio_LESA == "65/12") %>% View
# siteinfo_clean$Ratio_LESA
replacementsLESA <- list('undetermined'='', "?" = "", "65/12"="77", "." = "", "unknown" = "")
replacementsWESA <- list("160/65" = "225", "undetermined"="", "?"="","too far to see" = "", "." = "", "Too far to ID."="", "all"=1,
                         "unknown" = "")

ratios2Fix <- filter(siteinfo, Ratio != "") %>% select(RecordID, Ratio)
ratios2Fix$Ratio_WESA <- c(NA, 3, 79,NA,NA, NA,NA, 5000, 4, NA,NA,NA, 32, 33, 4,
                           10, 5,4, 150,8,4, 6, NA, 6, NA,90, NA, NA, 3,  47,29,12,NA,NA, 7, NA, 1, NA,NA,NA,
                           5,5,7,85,25,14,360,2275,NA,NA, 0,0,NA, NA,10,12,3,0, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                           0, NA,NA,NA,NA,NA,NA,NA, 65,8,86,4,37,0,3,2,7,2,NA, 1, 16, 8, 2, NA, 1,NA,NA,NA,NA,NA,56,
                           NA, NA, NA, 13, NA, NA, 6, NA,NA,NA,NA,NA,5, 5, NA, 22,NA, NA, NA,0, 2, NA,  NA, NA, 2,
                           0,NA,NA, NA, NA,NA, NA, NA,NA,  43,87,163,62,
                           NA,NA,NA,NA, NA, 20, 290, 1, 20, 14, NA, NA, NA, NA, 1, 7, NA, 1, 85, 75,85,95 # 2017
                           )

ratios2Fix$Ratio_LESA <- c(NA, 0, 6,NA,NA,  NA,NA,0 , 0,NA,NA, NA,0 ,1
                           , 2, 20, 2, 2,0,0, 1, 1,NA, 0, NA, 10,NA,  NA, 0, 0, 0, 68, NA,NA,0,NA,  2, NA,NA,NA,
                           0,0,36,15,25,110,2,89,NA, NA,9,11,NA, NA,4,3,30,15, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 
                           5, NA, NA,NA,NA,NA,NA,NA,0,14,38,2,9,2,0,0,6,5,NA, 14,8,7,2,NA, 0, NA,NA,NA,NA,NA, 0,
                           NA, NA, NA, 5, NA, NA, 0,NA,NA,NA,NA,NA,5,  6,NA, 3, NA, NA, NA, 12, 5, NA, NA, NA,1, 
                           11, NA, NA, NA, NA, NA,NA, NA, NA,67,36,26,4,
                           NA,NA,NA,NA, NA,2, 10, 0, 0, 9, NA, NA, NA, NA, 3, 0, NA, 0, 15,25,15,5# 2017
                           )


siteinfo_clean <- 
  siteinfo %>%
  mutate(YES_NO_WESA_Obs=ifelse(YES_NO_WESA_Obs %in% YN2Fix, replacementsYN[YES_NO_WESA_Obs], YES_NO_WESA_Obs),
         Ratio_LESA = ifelse(Ratio_LESA %in% names(replacementsLESA), replacementsLESA[Ratio_LESA], Ratio_LESA),
         Ratio_WESA = ifelse(Ratio_WESA %in% names(replacementsWESA), replacementsWESA[Ratio_WESA], Ratio_WESA),
         YES_NO_WESA_Obs = as.character(YES_NO_WESA_Obs), Ratio_LESA = as.numeric(Ratio_LESA),
         Ratio_WESA = as.numeric(Ratio_WESA) ) %>%
  left_join(ratios2Fix, by = 'RecordID') %>%
  mutate(Ratio_LESA = ifelse(Ratio.x == "", Ratio_LESA.x, Ratio_LESA.y),
         Ratio_WESA = ifelse(Ratio.x == "", Ratio_WESA.x, Ratio_WESA.y)) %>%
  select(-Ratio_LESA.x, -Ratio_LESA.y, -Ratio_WESA.x, -Ratio_WESA.y, -Ratio.y) %>%
  rename(Ratio = Ratio.x)

WESA_PEEPSlist <- c("WESA", "Peeps")
wesa_peeps <- filter(wesa_clean_numbers, !WESA_PEEPS %in% WESA_PEEPSlist) %>% .[['WESA_PEEPS']] %>% unique
# Fix WESA/Peeps
replacementsPeeps <- vector("list", length = length(wesa_peeps))
names(replacementsPeeps) <- wesa_peeps
# replacementsPeeps$`` <- NA
replacementsPeeps$`Peeps (Non species specific)` <- "Peeps"
replacementsPeeps$`Western Sandpipers Only` <- "WESA"
replacementsPeeps$PEEPS <- "Peeps"
replacementsPeeps$`WESA (plus 60 PEEPS sleeping)` <- "Peeps"
# filter(wesa_clean_numbers, WESA_PEEPS == "WESA (plus 50 PEEPS)") %>% View
replacementsPeeps$`WESA (plus 10 LESA)` <- "WESA"
replacementsPeeps$`WESA (plus 50 PEEPS)` <- "Peeps"
replacementsPeeps$`WESA (plus 50 peeps sleeping among mud clots)` <- "Peeps"
replacementsPeeps$`WESA (plus 60 PEEPS feeding along west shore)` <- "Peeps"
replacementsPeeps$`PEEPS (plus 40 peeps far shore` <- "Peeps"
replacementsPeeps$`WESA (plus PEEPS)` <- "Peeps"
replacementsPeeps$`Western Sandpipers Only, Peeps (Non species specific)` <- "WESA"

replacementsPeeps[sapply(replacementsPeeps, is.null)]
# filter(wesa_clean_numbers, WESA_PEEPS == "Western Sandpipers Only, Peeps (Non species specific)") %>% View
# filter(wesa_clean_numbers, WESA_PEEPS == "DC") %>% View

wesa_clean_numbers <- 
  wesa_clean_numbers %>%
  mutate(WESA_PEEPS=ifelse(WESA_PEEPS %in% WESA_PEEPSlist, WESA_PEEPS, replacementsPeeps[WESA_PEEPS]),
         WESA_PEEPS = as.character(WESA_PEEPS) )

# unique(wesa_clean_numbers$WESA_PEEPS)
# Check it worked. Make it a new column instead of overwriting to do so.
# wesa_clean_numbers %>% filter(!WESA_PEEPS %in% WESA_PEEPSlist) %>% select(WESA_PEEPS, WESA_PEEPS2) %>% unique





## Import the old data and see how it compares
# Commented out as errors fixed, but uncomment to improve or double-check

site.info_old  <- read.csv(paste(database.loc, 'Rscripts/data/site.info.clean.csv', sep =""))
obs.info_old   <- read.csv(paste(database.loc, 'Rscripts/data/obs.info.clean.csv', sep =""))
falcons_old    <- read.csv(paste(database.loc, 'Rscripts/data/falc.clean.csv', sep =""))
wesa_old       <- read.csv(paste(database.loc, 'Rscripts/data/wesa.clean.csv', sep =""))
site.codes <- read.csv(paste(database.loc, 'AreaCalculations/AreaComparison.csv', sep = ""),
                       stringsAsFactors = FALSE)
# 
# wesa_comparison <- wesa_clean_numbers %>% left_join(wesa_old, by = 'ID') 
# 
# wesa_comparison %>% 
#   ggplot(aes(counts.cleaned, CountClean)) + geom_point()
# 
# filter(wesa_comparison, counts.cleaned != CountClean) %>% 
#   select(counts.cleaned, CountClean, ID, Source) %>%View # This code highlights were the results differ
# from when I manually cleaned the data in excel. All are either correct in the new form or have a second entry 
# in the new version from an independent observer

# Find the Records that are in the old version but not the new
# missingData <- site.info_old %>% anti_join(siteinfo_clean, by = "RecordID") %>% filter(SiteID != "PNMA") %>%
#   select(RecordID, YES_NO_WESA_Obs, Site, SiteID, SiteName, Survey.Site)
# 3 entries 1. Squa was corrected due to wrong date entered
# filter(siteinfo_clean, SiteID == "SQUA" & Month == "8") %>% View
# 2. Crockett Lake
# filter(siteinfo_clean, SiteID == "CROC" ) %>% View
# filter(obs.info_old, RecordID == "CROC2014818")
# This is an error that was not previously corrected. Is actually CHEM
# #. SHPO2013819
# filter(siteinfo_clean, SiteID == "SHPO" ) %>% View
# This was an actual error. Fixed by fixing site name in python script for 2013.

missingOld <- wesa_clean_numbers %>% anti_join(wesa_old, by = 'ID')

####################################################################################
## -----------------Deal with the duplicate counts issue ------------------------ 
############################# DATA MANUPULATION WARNING ##############################
duplicateRecords <- siteinfo_clean %>% group_by(RecordID) %>%
  summarize(nRecords = n()) %>% 
  filter(nRecords > 1)


siteinfo_clean$Collaborative %>% unique
# filter(siteinfo_clean, Collaborative == "I saw at least 1 WESA during the survey")
# filter(siteinfo_clean, Collaborative == "Independent" | Collaborative == "Independent ") %>% View
independentCounts <- grep("a", siteinfo_clean$RecordID, value=T)
independentDuplicatRecords <- independentCounts %>% gsub("a", "", .)
recordsToCombineA <- filter(wesa_clean_numbers, RecordID %in% independentDuplicatRecords) %>%
  # Drop the records that I wish to add as alternative counts # I lose Anna vs Simons Count, but that's ok.
  select(-ALT_D2S, -Alt_Count, -Alt_Density, -Alt_WESA_PEEPS)
recordsToCombineB <- filter(wesa_clean_numbers,  RecordID %in% independentCounts) %>% 
  # Take alternative counts, rename them and then shift counts over to altcounts
  rename(RecordID_full = RecordID, ID_full = ID
         ) %>% 
  mutate(RecordID = gsub("a", "", RecordID_full),
         ID = gsub("a", "", ID_full),
         ALT_D2S = D2S,
         Alt_Count = CountClean,
         Alt_Density = Density, 
         Alt_WESA_PEEPS = WESA_PEEPS) %>%
  select(ID, RecordID, ALT_D2S, Alt_Count, Alt_Density, Alt_WESA_PEEPS)

recordsCombined <- recordsToCombineA %>% left_join(recordsToCombineB, by = c("RecordID", "ID")) %>%
  mutate( Alt_Count = as.character(Alt_Count)) #ALT_D2S = as.integer(ALT_D2S),

wesa_clean_alt_numbers <- wesa_clean_numbers %>% filter(!RecordID %in% independentCounts)%>%
  anti_join(recordsCombined, by = c("RecordID","ID" )) %>% bind_rows(recordsCombined) %>% 
  mutate(Alt_Count = as.numeric(Alt_Count),
         mean_count =  ifelse(!is.na(Alt_Count), (CountClean + Alt_Count)/2, CountClean)) %>% 
  group_by(ID) %>% 
  mutate(
         #mean(Alt_Count, CountClean, na.rm=T),
         maxCleanCount = max(Alt_Count, CountClean, na.rm=T)) %>% ungroup

# filter(wesa_clean_alt_numbers, RecordID %in% independentDuplicatRecords) %>% View
# wesa_clean_numbers %>% filter(!is.na(Alt_Count) & Alt_Count != "") %>% View
# wesa_clean_alt_numbers$Alt_Count %>% unique()

### ----------------- Examine counts with indepenet counts
only_independent_counts <- filter(wesa_clean_alt_numbers, !is.na(Alt_Count)) %>%
  mutate(diff_count = abs(CountClean - Alt_Count),
         # mean_count = (CountClean + Alt_Count)/2,
         max_count = ifelse(CountClean>Alt_Count, CountClean, Alt_Count),
         min_count = ifelse(CountClean<Alt_Count, CountClean, Alt_Count),
         propdiff = diff_count / mean_count,
         relCount = abs(Alt_Count -CountClean) / mean_count,
         sd = sqrt(((Alt_Count - mean_count)**2 +(CountClean - mean_count)**2 )/2),
         cv = sd/mean_count)
ggplot(only_independent_counts, aes(CountClean, Alt_Count)) + 
  geom_point() + #geom_smooth(method="lm", se=F) +
  geom_line(data = tibble(x=seq(0,4000, 1),y=seq(0,4000, 1)),aes(x=x, y = y, colour = 'red' )) +
  # geom_abline(slope = 1, intercept = 0, color = 'red') +
  lims(x=c(0,4000), y=c(0,4000)) + labs(x="Count 1", y = "Count 2") +
  # coord_trans(x=scales::log1p_trans(),y=scales::log1p_trans()) +
  theme(legend.position = 'none')

ggplot(only_independent_counts, aes(mean_count,diff_count, colour = SiteID)) + geom_point() +
    coord_trans(x = scales::log1p_trans()) + geom_smooth(method='lm', aes(group=1))
# filter(only_independent_counts, diff_count > 500) %>% View
ggplot(only_independent_counts %>% filter(mean_count>0),
       aes(mean_count, relCount)) + 
  geom_point(aes(colour = SiteID)) 
# ggplot(only_independent_counts %>% filter(mean_count==0), aes( Alt_Count)) + 
#   geom_histogram(binwidth = 5) + labs(x="2nd observer counts where 1st observer counts zero",
#                                       y="Number of Counts")

# require(lme4)
# lmer_ind <- lmer(CountClean~Alt_Count + (1|RecordID), data=only_independent_counts)
# augind <- broom::augment(lmer_ind, only_independent_counts)
# ggplot(augind, aes(.fitted, .resid)) + geom_point()
# plot(lmer_ind, resid(., type = "deviance") ~ fitted(.), abline = 0)

# ----------------------- Falcons -----------
# Need to remove other species and clean up this data
# Remove non-falcon observations and empty species obs
# filter(falcon_counts, Species == "") %>% View
# falcon_counts$Species %>% unique()
notFalcons <- c( "COHA (Coopers Hawk)","Northern Harrier" ,  "" , "NOHA"  , "BAEA", "sharp shinned hawk", "Osprey",
                 "SSHA","Sharp-shinned Hawk", "Sharp-shinned hawk" )

# filter(falcon_counts, Species == "PEFA, or SSHA or COHA") %>% View
# filter(falcon_counts, Species ==    "UNK" ) %>% View
# filter(falcon_counts, Species ==    "Unknown" ) %>% View
# filter(falcon_counts, Count ==    "0" ) %>% View
## After reading comments from unknow observations, will include all but EIDE2016717 as were
# described as falcons of unknown species
falcon_counts_clean <- filter(falcon_counts, !Species %in% notFalcons & RecordID != "EIDE2016717" &
                                Count != "0")

# Now clean up the counts

falcon_counts_clean$Count %>% unique()
# filter(falcon_counts_clean, Count == "") %>% View # Empty counts are all 1
replacementFalcCount <- c("one juvenile", "1 - seen approx 1.8 km from lagoon", "Unknown", 
                          "1 (the same individual was present on the same perch from 10:22 until observations stopped at 12:00)",
                          "1 (larger bird than first two records, presumed female)" ,
                          "1, smaller merlin" )
# replacementFalcCount$`one juvenile` <- "1"
# replacementFalcCount$`1 - seen approx 1.8 km from lagoon` <- "1"
# replacementFalcCount$`Unknown` <- "1"

falcon_counts_clean <- falcon_counts_clean %>%
  mutate(countClean = ifelse(!Count %in% replacementFalcCount, Count, "1"),
         countClean = ifelse(countClean == "", "1", countClean),
         countClean = as.numeric(countClean) )
# filter(falcon_counts_clean, RecordID %in% independentCounts) %>% View
falcon_counts_clean$countClean[falcon_counts_clean$ID=="RBBP2014816_F3"] <- 1

# Fix Times ---------------------------------------------------------------
require(lubridate)
siteinfo_clean$hour_start <- hour(strptime(siteinfo_clean$Time_Start, format = "%H:%M"))
siteinfo_clean$minute_start <- minute(strptime(siteinfo_clean$Time_Start, format = "%H:%M"))
siteinfo_clean$hour_start[!is.na(as.numeric(siteinfo_clean$Time_Start))]  <- hour(
  strptime(siteinfo_clean$Time_Start[!is.na(as.numeric(siteinfo_clean$Time_Start))], format = "%H%M") )
siteinfo_clean$minute_start[!is.na(as.numeric(siteinfo_clean$Time_Start))]  <- minute(
  strptime(siteinfo_clean$Time_Start[!is.na(as.numeric(siteinfo_clean$Time_Start))], format = "%H%M") )

siteinfo_clean$hour_end <- hour(strptime(siteinfo_clean$Time_End, format = "%H:%M"))
siteinfo_clean$minute_end <- minute(strptime(siteinfo_clean$Time_End, format = "%H:%M"))
siteinfo_clean$hour_end[!is.na(as.numeric(siteinfo_clean$Time_End))]  <- hour(
  strptime(siteinfo_clean$Time_End[!is.na(as.numeric(siteinfo_clean$Time_End))], format = "%H%M") )
siteinfo_clean$minute_end[!is.na(as.numeric(siteinfo_clean$Time_End))]  <- minute(
  strptime(siteinfo_clean$Time_End[!is.na(as.numeric(siteinfo_clean$Time_End))], format = "%H%M") )



startToFix <- siteinfo_clean %>% 
  dplyr::filter(
    is.na(hour_start) | #strptime(Time_Start, format = "%H:%M")) &
      is.na(minute_start) ) %>% #strptime(Time_Start, format = "%H%M"))) %>%
  .[['Time_Start']] %>% unique

endToFix <- siteinfo_clean %>% 
  dplyr::filter(
    is.na(hour_end) | #strptime(Time_End, format = "%H:%M")) &
      is.na(minute_end) ) %>% #strptime(Time_End, format = "%H%M"))) %>%
  .[['Time_End']] %>% unique


# filter(survey_data$wesa_counts, RecordID == "BB922014719") %>% View
replacements_start <- vector("list", length = length(startToFix))
names(replacements_start) <- startToFix
replacements_start$`1000 local` <- "10:00"
replacements_start$`1100 local` <- "11:00"
replacements_start$`700` <- "07:00"
replacements_start$`11am` <- "11:00"
replacements_start$`900` <- "09:00"
replacements_start$`10 am` <- "10:00"
replacements_start$`800` <- "08:00"
replacements_start$`12 noon` <- "12:00"
replacements_start$`730` <- "07:30"
replacements_start$"00:00:00" <-"12:00"
replacements_start$"00:15:00" <-"12:15"
replacements_start$`2016-09-13 13:00:00` <- "13:00"
replacements_start$`2016-09-13 13:30:00` <- "13:30"

replacements_start[sapply(replacements_start, is.null)]

replacements_end <- vector("list", length = length(endToFix))
names(replacements_end) <- endToFix
replacements_end$`930` <- "09:30"
replacements_end$`2pm` <- "14:00"
replacements_end$`934` <- "09:34"
replacements_end$`800` <- "10:00"
# replacements_end$ <- "07:30"
replacements_end$`2016-09-13 15:00:00` <-  "15:00"
replacements_end$`2016-09-13 16:00:00` <-  "16:00"

replacements_end[sapply(replacements_end, is.null)]
# Introoduce the cleaned counts
siteinf_final <- 
  siteinfo_clean %>%
  # rowwise %>%
    mutate(StartClean=ifelse(Time_Start %in% startToFix[startToFix!="UNK"&startToFix!=""], 
                   # cut(Time_Start, labels = replacements_start, right=F),
                             replacements_start[Time_Start],
                   paste0(hour_start,":", minute_start)),
           EndClean= ifelse(Time_End %in% endToFix[endToFix!="UNK"&endToFix!=""], 
                            # cut(Time_Start, labels = replacements_start, right=F),
                            replacements_end[Time_End],
                            paste0(hour_end,":", minute_end))) %>% #unnest %>% 
  mutate(hour_start_clean = ifelse(is.na(hour_start),
                                   hour(strptime(StartClean, format = "%H:%M")),
                                   hour_start),
         minut_start_clean = ifelse(is.na(minute_start),
                                   minute(strptime(StartClean, format = "%H:%M")),
                                   minute_start),
         hour_end_clean = ifelse(is.na(hour_end),
                                   hour(strptime(EndClean, format = "%H:%M")),
                                   hour_end),
         minut_end_clean = ifelse(is.na(minute_end),
                                    minute(strptime(EndClean, format = "%H:%M")),
                                    minute_end),
         hour=ifelse(hour_start_clean<6, hour_start_clean + 12, hour_start_clean),
         end_hour=ifelse(hour_end_clean<=7 | RecordID == "SJEN2016715", hour_end_clean + 12, hour_end_clean)) %>% 
  extractdatesiteid %>% 
  mutate(
         DatetimeStart= mdy_hm(paste(Month, Day, Year, hour, minut_start_clean, sep = "-")),
         DatetimeEnd = mdy_hm(paste(Month, Day, Year, end_hour, minut_end_clean, sep = "-")),
         Datetime_roundedstart = round_date(DatetimeStart, "15 minutes"),
         Datetime_roundedend = round_date(DatetimeEnd, "15 minutes"),
         diff =   difftime(DatetimeEnd,DatetimeStart,units = 'hours')) 

siteinfo_for_counts <- select(siteinf_final, RecordID, Datetime_roundedstart, Datetime_roundedend, SiteID)
# 




# adjust counts for ratios ------------------------------------------------
# Warning! I'm taking the mean count for alternative counts here
wesa.ratios.incounts <- 
  wesa_clean_alt_numbers %>% left_join(select(siteinfo_clean, RecordID, Ratio_WESA, Ratio_LESA), by = 'RecordID') %>% 
  mutate(Ratio_WESA = as.numeric(Ratio_WESA),
         Ratio_LESA = as.numeric(Ratio_LESA),
         propWESA = ifelse(Ratio_WESA + Ratio_LESA == 0, NA, 
                           Ratio_WESA/(Ratio_WESA + Ratio_LESA)),
         WESA.only = ifelse(WESA_PEEPS == "WESA", mean_count,#maxCleanCount, 
                            ifelse(is.na(propWESA),mean_count, #maxCleanCount, 
                                   mean_count * propWESA))) 
# Warning! I'm taking the mean count for alternative counts here

# Warning! I'm taking the max count for alternative counts here
wesa <- wesa.ratios.incounts %>% mutate(counts.cleaned = WESA.only) %>% #wesa_clean_alt_numbers   maxCleanCount) %>% #CountClean)
  filter(counts.cleaned != -Inf) 


site.info <- siteinf_final %>% filter(!RecordID %in% independentCounts)
obs.info <- obsinfo %>% filter(!RecordID %in% independentCounts)
# print((length(allObs[allObs!="" & allObs !="None"]) + 24) * 4 * 15)
falcons <- falcon_counts_clean %>% mutate(Count=countClean) %>% extractdatesiteid


records <- unique(wesa.ratios.incounts$RecordID)

# Include zerocounts ------------------------------------------------------
require(lubridate)
zero.counts <- 
  siteinf_final %>%
  filter(!(RecordID %in% records))# %>%
  # select(-X, -SiteName) #%>%
  # mutate(Year = substr(RecordID, 5, 8), 
  #        Month = substr(RecordID, 9,9),
  #        Day = substr(RecordID, 10, 11),
  #        Date = mdy(paste(Month, Day, Year, sep = "-")))
# zero.counts$Time_Start[zero.counts$Time_Start=="       07:30"] <- "07:30:00"

dat <- 
  zero.counts %>% #filter(Time_Start != "") %>% 
  mutate(
    # pasted=paste(Date, Time_Start),
    # datetime_start = ymd_hms(pasted),
    # datetime_end = ymd_hms(paste(Date, Time_End)) ,
    # duration_survey = datetime_end- datetime_start,
    ncounts =diff / as.duration( minutes(15)) +1
    ) %>% 
  filter(!is.na(ncounts)) %>%
  select(RecordID, Datetime_roundedstart, ncounts) 



makeemptydf <- function(RecordID, Datetime_roundedstart, ncounts){
  ncounts <- round(ncounts)
  dat <- tibble(RecordID = rep(RecordID,ncounts)) %>% 
    mutate(ID = paste(RecordID, "_W",row_number(), sep = ""),
           Time = 
             seq(as_datetime(Datetime_roundedstart), length.out = ncounts, by = "15 mins") , 
           counts.cleaned = 0)
}
zero.datfull <-  pmap_df(dat,makeemptydf)

wesa.final <- wesa  %>% left_join(siteinfo_for_counts) %>% 
  mutate(TimeEst = Datetime_roundedstart + minutes("15") *CountNum) %>% 
  bind_rows(zero.datfull %>% rename(TimeEst = Time)) %>% 
  extractdatesiteid
wesa <- wesa.final
# Export files to Shiny App -----
wesa2Shiny <- wesa.final %>% select(ID, RecordID,SiteID, counts.cleaned) 
siteInfo2shiny <- site.info %>% select(RecordID, SiteID , SiteName)
falcons4shiny <- falcons %>% select(RecordID, SiteID,ID, Count)
write.csv(wesa2Shiny, "../ShinyResults/Shiny_Map/.data/wesa4shiny.csv")
write.csv(siteInfo2shiny, "../ShinyResults/Shiny_Map/.data/siteInfo2shiny.csv")
write.csv(falcons4shiny, "../ShinyResults/Shiny_Map/.data/falcons2shiny.csv")


## 2016 Missing Data
julyRecords <- filter(mutate(siteinfo_clean, 
                             Month = substr(RecordID, 9,9), 
                             Year = substr(RecordID, 5, 8)
                             ), Month == 7 & Year == 2017) %>% 
  .[["SiteID"]] %>% as.vector %>% unique
augRecords <- filter(mutate(siteinfo_clean, 
                            Month = substr(RecordID, 9,9),
                            Year = substr(RecordID, 5, 8)), 
                     Month == 8 & Year == 2017) %>% 
  .[["SiteID"]] %>% as.vector %>% unique

JulyMissing <- 
site.codes %>% 
  filter(!SiteID %in% julyRecords) %>% select(SiteName, SiteID) %>% mutate(Month = "July")
AugustMissing <- 
site.codes %>% 
  filter(!SiteID %in% augRecords) %>% select(SiteName, SiteID) %>% mutate(Month = "August")

Missing2016 <- union(JulyMissing, AugustMissing)



