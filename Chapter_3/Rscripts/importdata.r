require(tidyverse)
require(lubridate)
if(!exists("which.avg")) which.avg <- "avg.max"

generatedist <- function(x, baseline_maxD_df=baseline_maxD, baselineRes_df=baselineRes){
  x %>% #filter(time %in% analysisdates) %>% 
    mutate(TotalBirds = site1+site0,
           Age = ifelse(time < 40, "Adult", "Juvenile"),
           pSafe = site1/(TotalBirds),
           rS_D = site1/site0) %>% 
    left_join(baseline_maxD_df, by = "time") %>% 
    left_join(baselineRes_df, by = 'time') %>% 
    mutate(d0 = site0 - baseline0C,
           d1 = site1 - baseline1C,
           rel0 = ifelse(baseline0==0, NA, d0/baseline0),
           rel1 = ifelse(baseline1==0, NA, d1/baseline1),
           ratio0 = site0/baseline0,
           ratio1 = site1/baseline1,
           rel0_wmax = d0/(1+baseline0),#ifelse(is.na(rel0), max(rel0, na.rm=T), rel0),
           rel1_wmax = d1/(1+baseline1),#ifelse(is.na(rel1), max(rel1, na.rm=T), rel1)
           relT = (TotalBirds-d_t)/d_t,
           RPD0 = ifelse(site0==0 & baseline0==0,0, 2*(site0-baseline0)/(site0+baseline0)),
           RPD1 = ifelse(site1==0 & baseline1==0, 0, 2*(site1-baseline1)/(site1+baseline1)),
           index0 = (site0 - baseline0) / (TotalBirds-d_t),
           index1 = (site1 - baseline1) / (TotalBirds-d_t),
           index_t = (TotalBirds - d_t ),
           Small = ifelse(baseline0C==0, 0,d0 / baseline0C),
           Large = ifelse(baseline1C==0, 0,d1 /baseline1C)
    )
  # rowwise %>% 
  # mutate(
  #   rel0 =  site0- baseline0[[Age]],
  #   rel1 = site1 -baseline1[[Age]]) %>% ungroup
}


getdensity <- function(d,scen, mod_dat, count.dat, var="pTotal", varmod="pSafe"){
  if(var=="pTotal"){
    dat <- count.dat %>% filter(doy==d) %>% .[[var]]
      #count.dat %>% filter(doy==d & d2=='low') %>% 
      # summarize_(pL = paste("sum(",var,")")) %>% .[['pL']]}
  }
  if(var=="relT")
    {
    dat <- count.dat %>% filter(doy==d) %>% .[[var]]
      #count.dat %>% filter(doy==d & d2=='low') %>% 
      # select(relT) %>% 
      # distinct %>% .[['relT']]}
    }
  if(grepl("index", var))
  {
    dat <- ifelse(d<220, tcounts$mn[tcounts$Month=="7"],tcounts$mn[tcounts$Month=="8"])
  }
  if(var %in% c("Large", "Small"))
  {
      dat <- count.dat %>% filter(doy==d) %>% .[[var]]
  }
  mdat <- mod_dat %>% filter(doy == d& scenario_f==scen) %>% 
    mutate_(y=varmod)
  model_density <-  density(mdat$y,na.rm = T)#, from = 0, to = 1)
  out <- model_density$y[which.min(abs(model_density$x-dat))]
  out_max <- max(model_density$y)
  d_max_p <-  model_density$x[which(model_density$y == out_max)]
  outpred <- model_density$x[which(model_density$y == out)]
  return(out/out_max)
}

filter_scenarios <- function(scenario_group, mod.dat, countdat){
  mdat <- mod.dat %>% 
    filter(doy %in% unique(countdat$doy) & scen_g == scenario_group)
  # print(mdat)
  # cdat <- countdat %>% select(Year, doy) %>% distinct
  tibble_x <- expand.grid(d =  unique(countdat$doy),
                          scen = as.character(unique(mdat$scenario_f)),
                          stringsAsFactors = F
  ) %>% #left_join(cdat, by =c("d"="doy")) %>% 
    mutate(density_prop= 
             # For each scenario in group, run all dates
             pmap_dbl(., getdensity, mod_dat=mdat,count.dat=countdat,var="pTotal", varmod="pSafe"),
           density_total= pmap_dbl(., getdensity, mod_dat=mdat,count.dat=countdat, 
                                   var="relT",
                                   varmod="relT"),
           density_small =pmap_dbl(., getdensity, mod_dat=mdat,count.dat=countdat, 
                                   var="Small",
                                   varmod="Small"),
           density_large =pmap_dbl(., getdensity, mod_dat=mdat,count.dat=countdat, 
                                   var="Large",
                                   varmod="Large")
           #,
           # density_index0 =
           #   pmap_dbl(., getdensity, mod_dat=mdat,count.dat=countdat, 
           #            var="index0",
           #            varmod="index0"),
           # density_index1 =
           #   pmap_dbl(., getdensity, mod_dat=mdat,count.dat=countdat, 
           #            var="index1",
           #            varmod="index1")
           
    ) %>%
    unnest %>% mutate(scenario_group)
  return(tibble_x)
  
}

dbaseline <- function(dat,var) {
  if(nrow(dat[!is.na(dat[[var]]),])<2) return(NA)
  d <- density(dat[[var]], na.rm=T)
  return(d$x[which.max(d$y)])
}

# Count Data --------------------------------------------------------------

# 
# danger.est <- read.csv(paste("../../../SharingFiles_ForWindows/ShorebirdSurveyDatabase/", 'AreaCalculations/GISArea.estimates_updated.csv', sep = ""),
#                        stringsAsFactors = FALSE) %>%
#   rename(Area = Area_Intertidal,
#          Danger = HighDanger_Area,
#          Prop.Danger = PropDanger) %>% 
#   mutate(Danger.group = ifelse(Prop.Danger<0.4, 'low', ifelse(Prop.Danger<0.75, 'mid', 'high')),
#          size = ifelse(Danger.group == 'low', 'Large', ifelse(Danger.group=='mid', "Medium", "Small")))


danger.est <- read_rds("../../../SharingFiles_ForWindows/ShorebirdSurveyDatabase/AreaCalculations/Updated_Danger_largerarea.rds") %>% 
  mutate(Prop.Danger = prop.danger, Danger.group = ifelse(size=="Large", "low", "high"), SiteID.danger= SiteID )


surveydates <- read_rds("~/Documents/SFU/PhD/Thesis_Project/Hope.wesa/Confrontation_chapter/.rds/surveydates.rds")  %>% 
  mutate(Month.name = month(Date, label=T)) %>% mutate_at(vars(Year, Month), as.character)
surveydates_full <- read_rds("~/Documents/SFU/PhD/Thesis_Project/Hope.wesa/Confrontation_chapter/.rds/surveydates_full.rds")
# mn.interpolated <- read_rds("../Confrontation_chapter/mn.interpol.rds") %>%
#   mutate(SiteID.danger = ifelse(grepl("^BB", SiteID), "BBAY",ifelse(grepl("JEN", SiteID), "JENB", SiteID) ))
# 
# mn.interpolated.summary <- mn.interpolated %>% 
#   group_by(SiteID) %>% 
#   mutate(sitemean = mean(interpolated_mean)) %>% 
#   group_by(Month, Year) %>% 
#   mutate(totalBirds = sum(interpolated_mean)) %>% 
#   group_by(Month) %>% mutate(mnTotal = mean(totalBirds)) %>% 
#   group_by(Month, Year, SiteID,totalBirds, SiteID.danger) %>% 
#   summarise(pTotal = interpolated_mean / totalBirds,
#             relC = (interpolated_mean - sitemean) / (totalBirds-mnTotal)) %>% 
#   group_by(Month) %>% mutate(relT=(totalBirds-mean(totalBirds)) /mean(totalBirds)) %>%
#   ungroup %>% left_join(danger.est, by = "SiteID.danger") %>% mutate(d2=ifelse(Danger.group=='low', 'low', 'high')) %>% ungroup %>% 
#   left_join(surveydates, by = c("Year", "Month"))
# tcounts <- 
#   mn.interpolated %>% 
#   group_by(Month, Year) %>% 
#   summarize(t = sum(interpolated_mean), n=n()) %>% 
#   group_by(Month) %>% summarize(mn = mean(t))
# Import scenario data
# source("Scenarios_noPlots.r")
if(which.avg == 'avg.max'){
surveyresults <- read_rds("../Confrontation_chapter/.rds/Results_Bootstrapped_withRelC_wmax.rds") %>% 
  ungroup %>% 
    left_join(surveydates, by = c("Year", "Month")) %>% group_by(Month) %>%  mutate(rTotal=(totalBirds-mean(totalBirds))/mean(totalBirds))
}
if(which.avg == 'avg.avg'){
  surveyresults <- read_rds("../Confrontation_chapter/Results_Bootstrapped_withRelC_wavg.rds") %>% 
    ungroup %>% 
    left_join(surveydates, by = c("Year", "Month")) %>% group_by(Month) %>%  mutate(rTotal=(totalBirds-mean(totalBirds))/mean(totalBirds))
}

# list.of.files <- list.files("../cpp_version/Migration_Model/OrganizedResults/noU/my_scenarios", "*.txt", full.names = T)
# baseline <- read_tsv(list.of.files[grep("PredTime0", list.of.files)]) %>% 
#   mutate(scenario = "Baseline", baselineTotal = site0+site1,
#          baselinePSafe = site1/baselineTotal)
# baselineRes <-  baseline %>% 
#   # filter(time %in% analysisdates) %>% 
#   group_by(time) %>% 
#   summarize(baseline0C=median(site0), baseline1C=median(site1))
# 
# baseline_maxD <- 
#   baseline %>% nest(-time) %>% 
#   mutate(d_t= map(data, dbaseline, var="baselineTotal"),
#          d_p= map(data, dbaseline, var="baselinePSafe"),
#          baseline0=map(data, dbaseline, var="site0"),
#          baseline1=map(data, dbaseline, var="site1")
#   ) %>% select(-data) %>% unnest()




require(magrittr)
# all_scenarios <- map_df(list.of.files, read_tsv)
# all_scenarios %<>% rename(scen_g = scenario, scenario_f = id) %>% 
#   mutate(doy = time + yday(mdy("6-20-2013"))) %>% generatedist







