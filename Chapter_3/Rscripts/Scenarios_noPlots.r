## From Scenarios.R with only data import

require(tidyverse)
require(cowplot)
# Plot out predictions for various scenarios
# August 10, 2017
# David Hope


# Setup -------------------------------------------------------------------
U <- "no" #  "with" #
pk <- "pk"#"no_pk"#
file.locations <- paste("../cpp_version/Migration_Model/OrganizedResults/", U,"U/","scenarios/", pk, "/", sep = "")
lenpk <- ifelse(pk == "pk", 66,69)#56, 59)
# analysisdates <- c(25, 56)


generatedist <- function(x){
  x %>% #filter(time %in% analysisdates) %>% 
    mutate(TotalBirds = site1+site0,
           Age = ifelse(time < 40, "Adult", "Juvenile"),
           pSafe = site1/(TotalBirds),
           rS_D = site1/site0) %>% 
    left_join(baselineRes, by = "time") %>% 
    mutate(d0 = site0 - baseline0,
           d1 = site1 - baseline1,
           rel0 = ifelse(baseline0==0, NA, d0/baseline0),
           rel1 = ifelse(baseline1==0, NA, d1/baseline1),
           ratio0 = site0/baseline0,
           ratio1 = site1/baseline1,
           rel0_wmax = d0/(1+baseline0),#ifelse(is.na(rel0), max(rel0, na.rm=T), rel0),
           rel1_wmax = d1/(1+baseline1),#ifelse(is.na(rel1), max(rel1, na.rm=T), rel1)
           relT = ((site0+site1)-(baseline0+baseline1))/(baseline0+baseline1),
           RPD0 = ifelse(site0==0 & baseline0==0,0, 2*(site0-baseline0)/(site0+baseline0)),
           RPD1 = ifelse(site1==0 & baseline1==0, 0, 2*(site1-baseline1)/(site1+baseline1)),
           index0 = (site0 - baseline0) / ((site0 + site1)-(baseline0 + baseline1)),
           index1 = (site1 - baseline1) / ((site0 + site1)-(baseline0 + baseline1)),
           index_t = (site0 + site1 - (baseline0 + baseline1) )
    )
  # rowwise %>% 
  # mutate(
  #   rel0 =  site0- baseline0[[Age]],
  #   rel1 = site1 -baseline1[[Age]]) %>% ungroup
}


summarizeCounts <- function(x){
  x %>% select(time, Age, scenario_f, site0, site1) %>% 
    gather("Site", "Count", site0:site1) %>% 
    group_by(scenario_f, time, Age, Site) %>% 
    summarize(
      lci = quantile(Count, probs = 0.025, na.rm=T),
      uci = quantile(Count, probs = 0.975, na.rm=T),
      count = mean(Count, na.rm=T)
    ) %>% 
    ungroup %>% mutate(Date = mdy("06-20-2013") + time,
                       s = factor(ifelse(Site=="site0", "Small", "Large"), 
                                  levels=c("Large", "Small")) )
}


# Baseline ----------------------------------------------------------------




baseline <- read_tsv(paste(file.locations, "baselinePred.txt", sep ="")) %>% 
  mutate(scenario = "Baseline")




baselineRes <-  baseline %>% 
  # filter(time %in% analysisdates) %>% 
  group_by(time) %>% 
  summarize(baseline0=median(site0), baseline1=median(site1))
# baseline0 <- list("Adult" = baselineRes$baseline0[[1]], "Juvenile" = baselineRes$baseline0[[2]])
# baseline1 <- list("Adult" = baselineRes$baseline1[[1]], "Juvenile" = baselineRes$baseline1[[2]])










# Predation timing --------------------------------------------------------



early <- read_tsv(paste(file.locations, "EarlyArrival.txt", sep ="")) %>% 
  mutate(scenario = "Early Falcon Arrival")

late <- read_tsv(paste(file.locations, "LateArrival.txt", sep ="")) %>% 
  mutate(scenario = "Late Falcon Arrival")


predationTiming <- bind_rows(baseline, early) %>% bind_rows(late) %>% 
  # filter(time %in% c(25, 56)) %>%
  generatedist %>%ungroup %>%  mutate(
    scenario_f = factor(scenario, levels = c("Early Falcon Arrival", "Baseline", "Late Falcon Arrival"))) 



pred_sum <- summarizeCounts(predationTiming)



# PopulationChange --------------------------------------------------------

decline10 <- read_tsv(paste(file.locations, "tenpercentDecl.txt", sep ="")) %>% 
  mutate(scenario = "10% Decline", popchange = -0.1)
decline20 <- read_tsv(paste(file.locations, "twentypercentDecl.txt", sep ="")) %>% 
  mutate(scenario = "20% Decline", popchange = -0.2)
decline30 <- read_tsv(paste(file.locations, "thirtypercentDecl.txt", sep ="")) %>% 
  mutate(scenario = "30% Decline", popchange = -0.3)
decline10J <- read_tsv(paste(file.locations, "tenpercentInc.txt", sep ="")) %>% 
  mutate(scenario = "10% Increase", popchange = 0.1)
# decline20J <- read_tsv(paste(file.locations, "twentypercentDeclJ.txt", sep ="")) %>% 
# mutate(scenario = "20% Decline in Juvenile", popchange = -0.2)
decline30J <- read_tsv(paste(file.locations, "thirtypercentInc.txt", sep ="")) %>% 
  mutate(scenario = "30% Increase", popchange = 0.3)


popDecline <- baseline %>% mutate(popchange=0) %>% bind_rows(decline10) %>% 
  bind_rows(decline20) %>% bind_rows(decline30) %>% bind_rows(decline10J) %>% 
  # bind_rows(decline20J) %>% 
  bind_rows(decline30J) %>% 
  generatedist %>% 
  mutate(
    scenario_f = factor(scenario, levels = c("30% Increase",
                                             "10% Increase",
                                             "Baseline",
                                             "10% Decline",
                                             "20% Decline",
                                             "30% Decline"))) 


popdec_sum <- summarizeCounts(popDecline)



# Predation Population ----------------------------------------------------

predFiles <- fileNames <- list.files(
  path = substr(file.locations, 1,nchar(file.locations)-1),
  pattern = "predPop",full.names = T)

predpop <- map_df(predFiles, function(x)read_tsv(x) %>% 
                    mutate(fname = substr(x, (nchar(x)-9), (nchar(x)-4)))) %>% 
  separate( fname, c("junk", "predPop"),sep = "_") %>% 
  mutate(predPop = as.numeric(predPop),
         week = time%/% 7) %>% 
  bind_rows(baseline %>% mutate(predPop = 1.0)) %>% generatedist %>% 
  mutate(scenario_f = as.factor(predPop*100) )


# Food adjustment ---------------------------------------------------------

foodFiles <- fileNames <- list.files(
  path = substr(file.locations, 1,nchar(file.locations)-1),
  pattern = "flywayFood",full.names = T)

foodChange <- map_df(foodFiles, function(x)read_tsv(x) %>% 
                       mutate(fname = substr(x, (nchar(x)-8), (nchar(x)-4)))) %>% 
  separate( fname, c("junk", "food"),sep = "_") %>% 
  mutate(food = as.numeric(food)) %>% 
  bind_rows(baseline %>% mutate(food = 1.0)) %>% generatedist %>% 
  mutate(scenario_f = as.factor(food))


# Oil Spill ---------------------------------------------------------------

OilFiles <- fileNames <- list.files(
  path = substr(file.locations, 1,nchar(file.locations)-1),
  pattern = "MontyOil",full.names = T)


OilSpill <- map_df(OilFiles, function(x)read_tsv(x) %>% 
                     mutate(fname = substr(x, (nchar(x)-10), (nchar(x)-4)))) %>% 
  separate( fname, c("Group", "P_Oiled"),sep = "_") %>% 
  mutate(PropOiled = as.numeric(P_Oiled)) %>% 
  bind_rows(baseline %>% mutate(PropOiled = 0.0, Group="Unoiled")) %>% generatedist %>% 
  mutate(scenario_f = as.factor(PropOiled))


# WESA Arrival ------------------------------------------------------------

earlyFiles <- fileNames <- list.files(
  path = substr(file.locations, 1,nchar(file.locations)-1),
  pattern = "Early_",full.names = T)

lateFiles <- fileNames <- list.files(
  path = substr(file.locations, 1,nchar(file.locations)-1),
  pattern = "Late_",full.names = T)


WESA_arrival <- map_df(c(earlyFiles, lateFiles), function(x)read_tsv(x) %>% 
                         mutate(fname = substr(x, lenpk, (nchar(x)-4)))) %>% 
  separate( fname, c("Timing", "Group"),sep = "_") %>% 
  bind_rows(baseline %>% mutate(Timing = "Baseline", Group="both")) %>% generatedist %>% 
  mutate(scenario_f = factor(Timing, levels = c("Early", "Baseline", "Late")))


# Wesa MASS ---------------------------------------------------------------
vheavy<- read_tsv(paste0(file.locations, "ArriveVHeavy.txt")) %>% mutate(scenario = "Very Heavy")
heavy<- read_tsv(paste0(file.locations, "ArriveHeavy.txt")) %>% mutate(scenario = "Heavy")
light<- read_tsv(paste0(file.locations, "ArriveLight.txt")) %>% mutate(scenario = "Light")
vlight <- read_tsv(paste0(file.locations, "ArriveVLight.txt")) %>% mutate(scenario = "Very Light")

mass_scenarios <- bind_rows(list(vheavy, heavy, light, vlight, baseline)) %>% generatedist %>% 
  mutate(scenario_f = scenario)


# Both Falc/WESA Arrival --------------------------------------------------

ComboFiles <- fileNames <- list.files(
  path = substr(file.locations, 1,nchar(file.locations)-1),
  pattern = "Combo",full.names = T)
require(lubridate)

both_arrival <- map_df(ComboFiles, function(x)read_tsv(x) %>% 
                         mutate(fname = substr(x, lenpk, (nchar(x)-4)))) %>% 
  separate( fname, c("Junk", "Falc", "WESA"),sep = "_") %>% 
  mutate(Falc = ifelse(Falc == "EarlyFalc", "Early", "Late"),
         WESA = ifelse(WESA == "EarlyWESA", "Early", "Late")) %>% 
  bind_rows(baseline %>% mutate(Falc = "Baseline", WESA="Baseline")) %>% 
  bind_rows(filter(WESA_arrival, Group == "both"& Timing != "Baseline") %>% 
              mutate(Falc = "Baseline", WESA = Timing) %>% #substr(Timing, 5, nchar(Timing))) %>% 
              select(-(baseline0:RPD1) ) ) %>% 
  bind_rows(filter(predationTiming, scenario!="Baseline") %>% 
              select(-(baseline0:RPD1) ) %>% 
              mutate(Falc=ifelse(grepl("Early",scenario), "Early", "Late"),
                     # substr(scenario, 1, 5), 
                     WESA = "Baseline")) %>% 
  
  generatedist %>% mutate(Falc=ifelse(Falc == "Late ","Late", Falc)) %>% 
  mutate(WESA = factor(WESA, levels=c("Early", "Baseline", "Late")),
         Falc = factor(Falc, levels=c("Early", "Baseline", "Late")), t=170+time,
         date = format(as_date(t), format="%m-%d"),
         scenario_f = paste0(Falc, " Falcons\n", WESA, " WESA\n")
  )#as.factor(time))


sum_both <- summarizeCounts(both_arrival) %>% left_join(distinct(select(both_arrival, scenario_f, Falc, WESA)), by = "scenario_f")



# Fullplot ----------------------------------------------------------------
all_scenarios <- bind_rows(list(predationTiming %>% mutate(scen_g="Predation Timing"),
                                
                                popDecline %>% mutate(scen_g="Population Change"),
                                predpop %>% mutate(scen_g="Predator Population"),
                                foodChange %>% mutate(scen_g="Food abundance"),
                                OilSpill %>% mutate(scen_g="Oil spill"),
                                WESA_arrival %>% mutate(scen_g="WESA Timing"),
                                mass_scenarios %>% mutate(scen_g="Arrival Mass"),
                                both_arrival %>% mutate(scen_g="Falc/WESA Timing") )) %>%
  mutate(doy=time + yday(mdy("6-20-2013")))



# One scenario_example ----------------------------------------------------

wesapop_all <- map_df(list.of.files[grep("WESApop", list.of.files)], read_tsv) %>% generatedist %>% 
  mutate(doy = time + yday(mdy("6-20-2013")) )


pop_summary <- 
  wesapop_all %>% rename(scen_g = scenario, scenario_f = id) %>% 
  # filter(time %in% c(25,56)) %>% 
  group_by(scen_g, scenario_f, doy) %>% nest() %>% 
  mutate(
    Quantiles_plarge = map(data, ~ quantile(.$pSafe,probs = c(0.025,0.1,0.25, 0.5, 0.75,0.9,0.975), na.rm=T)),
    Quantiles_relT = map(data, ~ quantile(.$relT,probs = c(0.025,0.1,0.25, 0.5, 0.75,0.9,0.975)))) %>% 
  unnest( pLarge = map(Quantiles_plarge, broom::tidy),relT=map(Quantiles_relT, broom::tidy)) %>% 
  rename(quant_pLarge = names, pLarge=x, quant_relT = names1, relT=x1)



# ggplot(wesapop_all, aes(id, pSafe, colour =)) + facet_wrap(~doy) + geom_line()
quantiles <- c("25%", "50%","75%" )
ggplot(pop_summary %>% 
         filter(doy > 181 & doy < 244& quant_pLarge %in%quantiles) %>% 
         select(-quant_pLarge, -pLarge) %>%
         spread(quant_relT, relT), 
       aes(scenario_f, `50%`)) + 
  facet_wrap(~doy) +
  geom_ribbon(aes(ymin=`25%`, ymax=`75%`),alpha=0.5, colour='grey') +
  geom_line(aes(y=`50%`))+ 
  labs(x="Parameter adjustment", y="Relative change from baseline\nin total number of birds",
       title="Population Scenarios") 

ggplot(pop_summary %>% 
         filter(doy > 181 & doy < 244& quant_pLarge %in%quantiles)  %>% 
         select(-quant_relT, -relT) %>% 
         spread(quant_pLarge, pLarge), 
       aes(scenario_f, `50%`)) + 
  facet_wrap(~doy, scales='free_x') +
  geom_ribbon(aes(ymin=`25%`, ymax=`75%`),alpha=0.5, colour='grey') +
  geom_line(aes(y=`50%`))+ 
  # stat_summary(geom='ribbon',
  #   fun.ymin = function(z) { quantile(z,0.25) },
  #   fun.ymax = function(z) { quantile(z,0.75) }
  #   ) +
  # stat_summary(fun.y='median', geom='line', na.rm=T) +
  labs(x="Parameter adjustment", y="Proportion of birds at large site")


