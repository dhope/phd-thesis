require(tidyverse)
source('data_cleaning_fromPython_and_MasterFiles.r')

boot.fun <- function(data,i, val,danger.est,avg_or_max,site_="Small",
                     removedsites = c("ALHE", "PANM", "KALE", "DEEP", "WINT", "SQUA",
                                                   "VRYL")
                      ) {
  avg_or_max_enq <- enquo(avg_or_max)
  # val_ <- enquo(val)
  if(is.null(i)){d <- data} else{
  d <- data[i,]#ifelse(is.data.frame(data), , data[i])
  }
  # wesa <- d
  if(!exists("joinlargesites")) source("BB_JENB.r")
  largesites <- joinlargesites(d) %>% select(-Year, -Month, -Day)#mutate_at(c("Year", "Month", "Day"),as.numeric)
  dat <- d %>%  
    filter(!grepl("^BB", RecordID)&!grepl("JEN", RecordID)) %>% 
    mutate(n.survey.sites = 1) %>% 
    bind_rows(largesites ) %>% filter(!SiteID %in% removedsites )
  

  site.by.month <-
	  dat %>% # Load data and remove rows without Records or where counts are NA (should have already been filled if correct)
	  filter(RecordID != 'RecordID' & !is.na(counts.cleaned) & counts.cleaned != -Inf) %>%
	  group_by(RecordID) %>% # Generate summary counts across each survey.
	  dplyr::summarize(max.count = max(counts.cleaned, na.rm=T),  # The Maximum Count in a Survey
	                   avg.count = mean(counts.cleaned, na.rm=T), # The average count in a survey
	                   sd.count = sd(counts.cleaned, na.rm = T),  # SD across counts in a survey 
	                   sum.count = sum(counts.cleaned, na.rm = T), # Sum of individual counts in a survey
	                   n.counts = n(),
	                   n.survey.sites = mean(n.survey.sites)) %>% # Number of counts in survey
	  mutate(SiteID = substr(RecordID, 1, 4), # Ensure siteid, and year month day arefilled in (they should be already)
	         Year = substr(RecordID, 5, 8), 
	         Month = substr(RecordID, 9,9),
	         Day = substr(RecordID, 10, 11)) %>%
	  group_by(Month, Year, SiteID) %>%# Summarize across month. Calculations similar to summarize above
	  dplyr::summarize(avg.max = mean(max.count),  # Average of maximum counts from surveys in a month
	  	avg.avg = mean(avg.count), n.days = n(),   # Average count in all surveys in a month
	  	sum.max = sum(max.count), # Sum of maximum count across all surveys in a month
	  	sum.avg=sum(avg.count), # Sum of survey average counts in a month
	  	w.avg.avg = weighted.mean(avg.count, n.survey.sites),
	  	w.avg.max = weighted.mean(max.count, n.survey.sites)
	  	) %>% ungroup %>% 
	  # mutate(presYN=ifelse(sum.max > 0, 1,0)) %>% 
    group_by(Year, Month) %>%  # Calculate monthly values across all sites
	  mutate(monthly.total = sum((!! avg_or_max_enq))) %>% #, # Sum of averages or max, depending on function value
	         #sum.total=sum(avg_or_max)) %>%
    ungroup %>% 
	  mutate(pTotal = (!! avg_or_max_enq)/monthly.total)
# return(site.by.month)
  

 site_mean_proportions_and_counts <- # Calculate the site proportions across all years to use in interpolation
	  site.by.month %>% 
	  filter(!SiteID %in% removedsites) %>% 
	  group_by(SiteID, Month) %>% 
	  summarize(site.mean.count = mean((!! avg_or_max_enq),na.rm=T), # Take the mean average or maximum across all years for each month and site
	            site.n=n(),
	            site.mean.prop = mean(pTotal,na.rm=T)) %>% ungroup # Calculate the mean proportion of total birds counted across all years

overall.relativemonth <- site.by.month %>%
	  filter(!SiteID %in% removedsites) %>% 
	  group_by(Year, Month) %>% 
	   summarize(annual.monthly.mean = mean((!! avg_or_max_enq)),na.rm=T) %>% group_by(Month) %>% # Annual mean of site mean or max counts
	  mutate(overall.month.mean = mean(annual.monthly.mean),na.rm=T) %>% # Overall Mean across all years of max or mean  counts
	           ungroup %>% mutate(
	         overal.rmonth = annual.monthly.mean/overall.month.mean) # Annual deviation in monthly mean from overall mean

  mn.interpolated <- 
	  site.by.month %>% 
	  filter(!SiteID %in% removedsites) %>% 
	  # filter(SiteID %in% filter(site.missing, x>0)$SiteID) %>%
	  select(Month, Year, SiteID, (!! avg_or_max_enq)) %>% spread(SiteID, (!! avg_or_max_enq)) %>% 
	  gather("SiteID", "surveyValue", c(-"Month", -"Year")) %>% 
	  left_join(overall.relativemonth, by=c("Month", "Year")) %>% 
    left_join(site_mean_proportions_and_counts, by = c("Month", "SiteID")) %>% 
	  # group_by(Month, SiteID) %>% mutate(monthly.mean = mean(avg.max, na.rm=T)) %>% ungroup %>% 
	  mutate(interpolated_via_p = ifelse(is.na(surveyValue),
	                                     site.mean.prop*(annual.monthly.mean*27 ), #33
	                                     surveyValue),
	         interpolated_via_site = ifelse(is.na(surveyValue), site.mean.count * overal.rmonth, surveyValue),
	         interpolated_mean = (interpolated_via_p +interpolated_via_site)/2,
	         miss = ifelse(is.na(surveyValue), "Interpolated", "Counts")) %>% 
    # Uncomment to remove interpolation values.
    # mutate(interpolated_mean = surveyValue) %>% filter(!is.na(surveyValue)) %>% 
	  mutate(SiteID.danger = ifelse(grepl("^BB", SiteID), "BBAY",ifelse(grepl("JEN", SiteID), "JENB", SiteID) )) %>%
	  group_by(SiteID) %>% 
	  mutate(sitemean = mean(interpolated_mean,na.rm=T)) %>% 
    group_by(Month, SiteID) %>% 
    mutate(sitemean_monthly = mean(interpolated_mean,na.rm=T)) %>% 
	  group_by(Month, Year) %>% 
	  mutate(totalBirds = sum(interpolated_mean)) %>% 
	  group_by(Month) %>% mutate(mnTotal = mean(totalBirds,na.rm=T)) %>% 
	  group_by(Month, Year, SiteID,totalBirds, SiteID.danger,miss) %>% 
	  summarise(pTotal = interpolated_mean / totalBirds,
	            rel_S = 
	              # ifelse(miss=="Interpolated", 0,
	                     ifelse(sitemean_monthly==0, 0,(interpolated_mean - sitemean_monthly)/sitemean_monthly),
	                     # ),
	            # count = n.survey.sites,
	            relC = (interpolated_mean - sitemean) / (totalBirds-mnTotal)) %>% 
	  group_by(Month) %>% mutate(relT=(totalBirds-mean(totalBirds)) /mean(totalBirds,na.rm=T)) %>%
	  ungroup %>% left_join(danger.est, by = c("SiteID.danger")) %>% mutate(d2=ifelse(Danger.group=='low', 'low', 'high')) %>% ungroup 
  
    if(val=='relC')
      {
      ov <- mn.interpolated %>% group_by(Month, Year, size) %>%
        summarize(rel_S = mean(rel_S,na.rm=T)) %>% ungroup %>% 
        filter(size == !!site_)
      # print(length(i))
      return(ov$rel_S)
    }
  if(val=='relC_all')
  {
    ov <- mn.interpolated %>% group_by(Month, Year, size) %>%
      summarize(rel_S = mean(rel_S)) %>% ungroup 
    # print(length(i))
    return(ov)
  }
	  # if(val=="pTotal"){
	  	outval <- mn.interpolated %>% filter(size=="Large") %>%
	  				group_by(Month, Year, totalBirds, relT) %>%
	  				summarize(pTotal = sum(pTotal)) %>% ungroup
	  	# sum(mn.interpolated$pTotal[mn.interpolated$size=="Large"&
	  						# mn.interpolated$Month==mnth & mn.interpolated$Year==yr], na.rm=T)
	  # }
	  # if(val=="totalBirds"){
	  # 	outval2 <- #unique(
	  # 		mn.interpolated$totalBirds[mn.interpolated$Month==mnth & mn.interpolated$Year==yr]
	  # 	)
	  # }
    if(val=="all"){return(outval)} else  return(outval[[val]])
}

# datruns <- expand.grid(yr = unique(mn.interpolated.summary$Year), 
#                        mnth = unique(mn.interpolated.summary$Month),
#                        val = c("pTotal", "totalBirds")) #%>%
#                            # select(-runs) %>%
#                            group_by(yr, mnth, val) %>% nest() 

#https://www.painblogr.org/2017-10-18-purrring-through-bootstraps.html

source("../Snowmelt/Min.Snow.Date.R", chdir=T)
sitestoremove <- c("ALHE", "PANM", "KALE", "DEEP", "WINT", "SQUA", 
                   "SHPO",#"EIDE",#"JENB","CROC",
                   "VRYL")
wesa <- wesa.final %>% filter(!is.na(counts.cleaned)) %>% 
  filter(!grepl(paste(sitestoremove, collapse = "|"),RecordID )) %>% ungroup
# source('BB_JENB.r')

# wesa_L <- wesa %>%  mutate(SiteID = substr(RecordID, 1, 4), 
#                                       Year = substr(RecordID, 5, 8),
#                                       Month = substr(RecordID, 9,9),
#                                       Day = substr(RecordID, 10, 11),
#                            n.survey.sites = 1) %>% 
#   filter(!grepl("^BB", RecordID)&!grepl("JEN", RecordID)) %>% 
#   bind_rows(largesites)

danger.est <- read_rds("../AreaCalculations/Updated_Danger_largerarea.rds") %>% 
  mutate(Prop.Danger = prop.danger, Danger.group = ifelse(size=="Large", "low", "high"), SiteID.danger= SiteID )

# danger.est %<>% mutate(
#   Danger.group = ifelse(SiteID %in%c("JENB","CROC"), "Large", Danger.group),
#   size = ifelse(SiteID %in%c("JENB","CROC"), "Large", size))
# wesa %<>% filter(SiteID !="EIDE")
# wesar <- wesa %>% mutate(SiteID = substr(RecordID, 1, 4), 
#                          Year = substr(RecordID, 5, 8), 
#                          Month = substr(RecordID, 9,9),
#                          Day = substr(RecordID, 10, 11)) %>% 
#   left_join( dplyr::select(site.codes,SiteID, Region), by="SiteID") %>% mutate(
#     Region = factor(ifelse(SiteID %in% c("MAPL", "SQUA"), "Van", Region),
#                     levels=c("Malc", "EVanIs", "Tofino", "SVanIs", "FRD", "Van", "Puget"))) %>% 
#   filter(!is.na(Region)) %>% left_join(danger.est)
# wesar$s

# wesa_nest <- wesa %>%
# 	  mutate(SiteID = substr(RecordID, 1, 4), 
# 	         Year = substr(RecordID, 5, 8), 
# 	         Month = substr(RecordID, 9,9),
# 	         Day = substr(RecordID, 10, 11)) %>% 
#   mutate(SiteID =ifelse(grepl("JEN", SiteID), "JENB", SiteID)) %>% 
#   # filter(!SiteID %in% c("CROC", "EIDE", "KENN", "ENBO")) %>%
#     # First I group the data by species 
#     dplyr::group_by(Month, Year) %>%
#     # Then I nest the dataframe
#     tidyr::nest() 

# wesa_nest <- wesa %>% mutate(SiteID = substr(RecordID, 1, 4), 
#                              Year = substr(RecordID, 5, 8),
#                              Month = substr(RecordID, 9,9),
#                              Day = substr(RecordID, 10, 11)) %>% 
#   dplyr::group_by(Month, Year) %>% 
#   tidyr::nest() 
# wesa_L %<>%  filter(!SiteID %in% c("JENB", "CROC", "EIDE"))

typelist <- list("avg.max" = "Average  of Survey Maximum", 
                 "avg.avg"="Average of Survey Average", 
                 "w.avg.max"="Weighted Average  of Survey Maximum",
                 "w.avg.avg" = "Weighted Average  of Survey Average")


# CompareTypes ------------------------------------------------------------


comparetypes <- 
bind_rows(list(
  boot.fun(data = wesa, i = NULL, val = "all", danger.est = danger.est, avg_or_max = avg.max,removedsites = sitestoremove) %>% mutate(t = "avg.max"),
boot.fun(wesa, i = NULL, val = "all", danger.est = danger.est, avg_or_max = w.avg.max,removedsites = sitestoremove)%>% mutate(t = "w.avg.max"),
boot.fun(wesa, i = NULL, val = "all", danger.est = danger.est, avg_or_max = w.avg.avg,removedsites = sitestoremove)%>% mutate(t = "w.avg.avg"),
boot.fun(wesa, i = NULL, val = "all", danger.est = danger.est, avg_or_max = avg.avg,removedsites = sitestoremove)%>% mutate(t = "avg.avg") )) %>%
  rowwise() %>% 
  mutate(type = typelist[[t]]) %>% ungroup %>% 
  mutate(month = factor(ifelse(Month=="7", "July", "August"), 
                        levels = c( "July", "August")))

write_rds(comparetypes, "WithInterpolation_comparetypes.rds")
# dkjaf <- 
# comparetypes %>% left_join(surveydates) %>% mutate(date = doy - yday(mdy("6-20-2013"))) %>% 
#   select(Year,date,totalBirds, pTotal,t) %>% group_by(t) %>% nest() #rowid_to_column('id') %>% 
#   # group_by(id) %>% nest %>% 
#  purrr::map2( dkjaf$data,dkjaf$t,write_csv)
require(cowplot)
EstimationMethods <- 
plot_grid(
ggplot(comparetypes, aes(as.numeric(Year), pTotal, colour = type)) + 
  facet_wrap(~month) + geom_line() + 
  ylim(0,1) + labs(x= "", y="Proportion of Birds at the Large Sites", colour = "Estimation Method")+ 
  theme(legend.position = 'none')+
  scale_color_brewer(palette="Set1", type = "qual"),
ggplot(comparetypes, aes(as.numeric(Year), totalBirds, colour = type)) + facet_wrap(~month) + geom_line() + 
  theme(legend.position = 'bottom')+
  labs(x= "Year", y="Total Numbers of Birds", colour = "Estimation Method")+
  scale_color_brewer(palette="Set1", type = "qual") , nrow=2)


NoInterpolation <- 
  read_rds( "NoInterpolation.rds")

EstimationMethods_Nointerpolation <- 
  plot_grid(
    ggplot(NoInterpolation, aes(as.numeric(Year), pTotal, colour = type)) + 
      facet_wrap(~month) + geom_line() + 
      ylim(0,1) + labs(x= "", y="Proportion of Birds at the Large Sites", colour = "Estimation Method")+ 
      theme(legend.position = 'none')+
      scale_color_brewer(palette="Set1", type = "qual"),
    ggplot(comparetypes, aes(as.numeric(Year), totalBirds, colour = type)) + facet_wrap(~month) + geom_line() + 
      theme(legend.position = 'bottom')+
      labs(x= "Year", y="Total Numbers of Birds", colour = "Estimation Method")+
      scale_color_brewer(palette="Set1", type = "qual") , nrow=2)


ggplot(comparetypes, aes(as.numeric(Year), pTotal*totalBirds, colour = type)) + facet_wrap(~Month) + geom_line() + #ylim(0,1) +
  geom_line(aes(y=(1-pTotal)*totalBirds), linetype=2) + scale_color_brewer(palette="Set1", type = "qual")

comparetypes_relC <- 
  bind_rows(list(
    boot.fun(wesa, i = NULL, val = "relC_all", danger.est = danger.est, avg_or_max = avg.max,removedsites = sitestoremove) %>% mutate(t = "avg.max"),
    boot.fun(wesa, i = NULL, val = "relC_all", danger.est = danger.est, avg_or_max = w.avg.max,removedsites = sitestoremove)%>% mutate(t = "w.avg.max"),
    boot.fun(wesa, i = NULL, val = "relC_all", danger.est = danger.est, avg_or_max = w.avg.avg,removedsites = sitestoremove)%>% mutate(t = "w.avg.avg"),
    boot.fun(wesa, i = NULL, val = "relC_all", danger.est = danger.est, avg_or_max = avg.avg,removedsites = sitestoremove)%>% mutate(t = "avg.avg") ))

ggplot(comparetypes_relC, aes(as.numeric(Year), rel_S, colour = t, linetype=size)) + 
  facet_wrap(~Month) + geom_line() + #ylim(0,1)+ 
  scale_color_brewer(palette="Set1", type = "qual")



require(magrittr)
# Add a list column called "booted" containing the object produced by `
# applying `boot::boot` over the data in the "data" list-column.
# wesa_nest %<>%
#     dplyr::mutate(booted = purrr::map(.x = data, # The list-column containing <S3: tibble>
#                                       ~ boot::boot(data = .x, # The <S3 tibble> column being sampled
#                                                    statistic = boot.fun, # The user-defined function
#                                                    R = 1000, # The number of replicates
#                                                    stype = "i",avg_or_max = w.avg.max,
#                                                    danger.est=danger.est, val="pTotal")))


df <- full_join(
  boot.fun(wesa, i = NULL, val = "all", danger.est = danger.est, avg_or_max = w.avg.max,removedsites = sitestoremove),
  boot.fun(wesa, i = NULL, val = "relC_all", danger.est = danger.est, avg_or_max = w.avg.max,removedsites = sitestoremove) %>% 
    spread(size, rel_S),by=c("Month", "Year"))
boot1 <- boot::boot(data = wesa, # The <S3 tibble> column being sampled
                                    statistic = boot.fun, # The user-defined function
                                    R = 1000, # The number of replicates
                                    stype = "i",avg_or_max = w.avg.max,
                                    danger.est=danger.est, val="pTotal")

boot_ci_1 <- map(1:10, ~boot::boot.ci(boot1, conf=0.95, type='perc', index = .x)) 

df$lci_pTotal <- map_dbl(1:10, ~ boot_ci_1[[.x]]$perc[[4]])
df$uci_pTotal <- map_dbl(1:10, ~ boot_ci_1[[.x]]$perc[[5]])

boot2 <- boot::boot(data = wesa, # The <S3 tibble> column being sampled
                                    statistic = boot.fun, # The user-defined function
                                    R = 1000, # The number of replicates
                                    stype = "i",avg_or_max = w.avg.max,
                                    danger.est=danger.est, val="totalBirds")

boot_ci_2 <- map(1:10, ~boot::boot.ci(boot2, conf=0.95, type='perc', index = .x)) 

df$lci_Total <- map_dbl(1:10, ~ boot_ci_2[[.x]]$perc[[4]])
df$uci_Total <- map_dbl(1:10, ~ boot_ci_2[[.x]]$perc[[5]])





boot3 <- boot::boot(data = wesa, # The <S3 tibble> column being sampled
                    statistic = boot.fun, # The user-defined function
                    R = 1000, # The number of replicates
                    stype = "i",avg_or_max = w.avg.max,site_="Small",
                    danger.est=danger.est, val="relC")

boot_ci_3 <- map(1:10, ~boot::boot.ci(boot3, conf=0.95, type='perc', index = .x)) 

df$lci_r0 <- map_dbl(1:10, ~ boot_ci_3[[.x]]$perc[[4]])
df$uci_r0 <- map_dbl(1:10, ~ boot_ci_3[[.x]]$perc[[5]])

boot4 <- boot::boot(data = wesa, # The <S3 tibble> column being sampled
                    statistic = boot.fun, # The user-defined function
                    R = 1000, # The number of replicates
                    stype = "i",avg_or_max = w.avg.max,site_="Large",
                    danger.est=danger.est, val="relC")

boot_ci_4 <- map(1:10, ~boot::boot.ci(boot4, conf=0.95, type='perc', index = .x)) 

df$lci_r1 <- map_dbl(1:10, ~ boot_ci_4[[.x]]$perc[[4]])
df$uci_r1 <- map_dbl(1:10, ~ boot_ci_4[[.x]]$perc[[5]])


boot5 <- boot::boot(data = wesa, # The <S3 tibble> column being sampled
                    statistic = boot.fun, # The user-defined function
                    R = 1000, # The number of replicates
                    stype = "i",avg_or_max = w.avg.max,
                    danger.est=danger.est, val="relT")

boot_ci_5 <- map(1:10, ~boot::boot.ci(boot5, conf=0.95, type='perc', index = .x)) 

df$lci_rT <- map_dbl(1:10, ~ boot_ci_5[[.x]]$perc[[4]])
df$uci_rT <- map_dbl(1:10, ~ boot_ci_5[[.x]]$perc[[5]])
df$rT <- map_dbl(1:10, ~ boot_ci_5[[.x]]$t0)








write_rds(df,"../../../Thesis Project/Hope.wesa/Confrontation_chapter/Results_Bootstrapped_withRelC_wmax.rds")




















# outputbyyear <- pmap(datruns, boot.fun(data=wesa, i=data, danger.est[["i"]], val, yr, mnth)

# Add a list column called "booted_ci" containing the object produced by `
# applying `boot::boot.ci` over the data in the "booted" list-column.
# wesa_nest %<>%
#     dplyr::mutate(booted_ci = purrr::map(.x = booted, # The list-column containing <S3: boot> objects
#                                          ~ boot::boot.ci(.x,
#                                                          conf = 0.95, # Interval width
#                                                          type = "basic")))  # Calculate a BCa interval
# 
#     
# 
# 
# 
# 
# 
# wesa_nest %<>%
#   dplyr::mutate(booted_total = purrr::map(.x = data, # The list-column containing <S3: tibble>
#                                     ~ boot::boot(data = .x, # The <S3 tibble> column being sampled
#                                                  statistic = boot.fun, # The user-defined function
#                                                  R = 1000, # The number of replicates
#                                                  stype = "i",avg_or_max = w.avg.max,
#                                                  danger.est=danger.est, val="totalBirds")))
# 
# 
# wesa_nest %<>%
#   dplyr::mutate(booted_ci_total = purrr::map(.x = booted_total, # The list-column containing <S3: boot> objects
#                                        ~ boot::boot.ci(.x,
#                                                        conf = 0.95, # Interval width
#                                                        type = "basic")))  # Calculate a BCa interval
# 
# # write_rds(wesa_nest, "Bootstrapped_Estimates.rds")
# # wesa_nest <- read_rds("Bootstrapped_Estimates.rds")
# wesa_booted <- wesa_nest %>%
#   # Add columns
#   dplyr::mutate(statistic = purrr::map(.x = booted_ci, # The list-column containing <S3 bootci> objects
#                                        ~ .x$t0), # The point estimate
#                 lower_ci = purrr::map(.x = booted_ci,
#                                       ~ .x$basic[[4]]), # The value of the lower 2.5% limit
#                 upper_ci = purrr::map(.x = booted_ci,
#                                       ~ .x$basic[[5]]),
#                 statisticT = purrr::map(.x = booted_ci_total, # The list-column containing <S3 bootci> objects
#                                        ~ .x$t0), # The point estimate
#                 lower_ciT = purrr::map(.x = booted_ci_total,
#                                       ~ .x$basic[[4]]), # The value of the lower 2.5% limit
#                 upper_ciT = purrr::map(.x = booted_ci_total,
#                                       ~ .x$basic[[5]])) %>% # The value of teh upper 97.5% limit
#   # Drop the list-columns (no longer needed)
#   dplyr::select(-data, -booted, -booted_ci, -booted_total, -booted_ci_total) %>%
#   # Unnest the dataframe
#   tidyr::unnest()

# write_rds(wesa_booted,"../../../Thesis Project/Hope.wesa/Confrontation_chapter/Results_Bootstrapped_BBJEBCombined.rds")
surveydates <- read_rds("../../../Thesis Project/Hope.wesa/Confrontation_chapter/.rds/surveydates.rds")
wesa_booted <- read_rds("../../../Thesis Project/Hope.wesa/Confrontation_chapter/.rds/Results_Bootstrapped_withRelC_wmax.rds")




plot_plarge <- 
  ggplot(wesa_booted, aes(Year, statistic, colour = month(as.numeric(Month), label=T))) + 
  geom_pointrange(aes(ymin=lower_ci, ymax=upper_ci), position = position_dodge(0.2)) +
  labs(y="Proportion of birds\nat large sites", colour = "Month") + ylim(0,1) +
  scale_color_brewer(type = 'qual', palette = "Set1")

plot_T <- 
  ggplot(wesa_booted, aes(Year, statisticT, colour = month(as.numeric(Month), label=T))) + 
  geom_pointrange(aes(ymin=lower_ciT, ymax=upper_ciT), position = position_dodge(0.4)) +
  labs(y="Total number \nof sandpipers counted", colour = "Month", fill="") + #ylim(0,8000) +
  scale_color_brewer(type = 'qual', palette = "Set1") +
  scale_fill_brewer(type = 'qual', palette = "Set1") +
  geom_label(data=surveydates, colour = 'black',
             aes(x=as.character(Year),
                 label=Day, y=15000,
                 fill = month(as.numeric(Month), label=T)),
             position = position_dodge(0.4))
  
require(lubridate)
plot_Small <- 
  ggplot(wesa_booted, aes(Year, Small, colour = month(as.numeric(Month), label=T))) + 
  geom_pointrange(aes(ymin=lci_r0, ymax=uci_r0), position = position_dodge(0.4)) +
  labs(y="Total number \nof sandpipers counted", colour = "Month", fill="") + #ylim(0,8000) +
  scale_color_brewer(type = 'qual', palette = "Set1") +
  scale_fill_brewer(type = 'qual', palette = "Set1") 
plot_Large <- 
  ggplot(wesa_booted, aes(Year, Large, colour = month(as.numeric(Month), label=T))) + 
  geom_pointrange(aes(ymin=lci_r1, ymax=uci_r1), position = position_dodge(0.4)) +
  labs(y="Total number \nof sandpipers counted", colour = "Month", fill="") + #ylim(0,8000) +
  scale_color_brewer(type = 'qual', palette = "Set1") +
  scale_fill_brewer(type = 'qual', palette = "Set1") 


plot_Relative <- 
  ggplot(wesa_booted , aes(Year, Small)) + 
  geom_pointrange(aes(ymin=lci_r0, ymax=uci_r0), position = position_nudge(x=-0.1), colour = "#EE2C2C") +
  geom_pointrange(aes(y=Large, ymin=lci_r1, ymax=uci_r1), position = position_nudge(x=0.1), colour = "#0000FF") +
  facet_wrap(~month(as.numeric(Month), label=T), nrow=2)+
  labs(y="Relative shift from mean maximum site count \n at large(blue) and small(red) sites", colour = "Month", fill="") #+ #ylim(0,8000) +
  # scale_color_brewer(type = 'qual', palette = "Set1") +
  # scale_fill_brewer(type = 'qual', palette = "Set1") 
plot_Aug <- 
  ggplot(wesa_booted, aes(Year, Large, colour = month(as.numeric(Month), label=T))) + 
  geom_pointrange(aes(ymin=lci_r1, ymax=uci_r1), position = position_dodge(0.4)) +
  labs(y="Total number \nof sandpipers counted", colour = "Month", fill="") + #ylim(0,8000) +
  scale_color_brewer(type = 'qual', palette = "Set1") +
  scale_fill_brewer(type = 'qual', palette = "Set1") 


require(cowplot)

plot_grid(plot_Small+theme(legend.position = 'none') ,  plot_Large+ theme(legend.position = 'bottom'),
          nrow = 2, rel_heights = c(0.45,0.55))



wesa_booted %>% left_join(mean.snowmelt %>% mutate(Year = as.character(Year))) %>% 
  ggplot( aes(dev.snowmelt, statistic, colour = month(as.numeric(Month), label=T))) + 
  geom_pointrange(aes(ymin=lower_ci, ymax=upper_ci), position = position_dodge(0.2)) +
  labs(y="Proportion of birds\nat large sites", colour = "Month") + ylim(0,1) +
  scale_color_brewer(type = 'qual', palette = "Set1", direction=-1)
ggsave("snowmelt_finalplot.png", width = 6, height = 6)
wesa_booted %>% #left_join(mean.snowmelt %>% mutate(Year = as.character(Year))) %>% 
  ggplot( aes(statisticT, statistic, colour = month(as.numeric(Month), label=T))) + 
  geom_pointrange(aes(ymin=lower_ci, ymax=upper_ci), position = position_dodge(0.2)) +
  labs(y="Proportion of birds\nat large sites", colour = "Month") + ylim(0,1) +
  scale_color_brewer(type = 'qual', palette = "Set1", direction=-1)
ggsave("population_finalplot.png", width = 6, height = 6)


