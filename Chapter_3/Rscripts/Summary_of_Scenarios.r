require(tidyverse)
require(purrr)
if(!exists('surveydates') )source('importdata.r')
# dist <- 5000
distances <- c(1300,2300,3500,5000,7000,9000)
full_pulltimes <- tibble(time=0:120) %>% filter(time %% 3==0 & time < 70 & time > 10 ) %>% .[["time"]]
# pulltimes <- c(25,56)
all.list.of.files <-
  list.files("../cpp_version/Migration_Model/OrganizedResults/noU/my_scenarios/withDist", "*.txt", full.names = T)
#list.files("../cpp_version/Migration_Model/my_scenarios/", "*.txt", full.names = T)
  # 
### pk 0 == TRUE, 1 === FALSE ###############
generatedist <- function(x, baseline_maxD_df=baseline_maxD, baselineRes_df=baselineRes){
  x %>% ungroup %>% #filter(time %in% analysisdates) %>% 
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
extract_baseline <- function(dist, pk_, pulltimes=c(25,56)){
  dist.list.of.files <-  all.list.of.files[grepl(paste0("dist_", dist, "."), all.list.of.files)]
  baseline <- read_tsv(
    dist.list.of.files[grepl("PredTime0", dist.list.of.files)], col_types = cols()
  ) %>% filter(time %in% pulltimes& pk ==pk_) %>% 
    mutate(scenario = "Baseline", baselineTotal = site0+site1,
           baselinePSafe = site1/baselineTotal)
  baselineRes <-  baseline %>% 
    # filter(time %in% analysisdates) %>% 
    group_by(time) %>% 
    summarize(baseline0C=median(site0), baseline1C=median(site1)) %>% ungroup
  
  
  baseline_maxD <- 
    baseline %>% nest(-time) %>% 
    mutate(d_t= map(data, dbaseline, var="baselineTotal"),
           d_p= map(data, dbaseline, var="baselinePSafe"),
           baseline0=map(data, dbaseline, var="site0"),
           baseline1=map(data, dbaseline, var="site1")
    ) %>% select(-data) %>% unnest()
  return(list(dist = dist, baseline=baseline, baselineRes=baselineRes, baseline_maxD=baseline_maxD))
}

# baseline_all_list_pk <- map(distances, extract_baseline, pk_=0)
# baseline_all_list_nopk <- map(distances, extract_baseline, pk_=1)
# 
# names(baseline_all_list_pk) <- distances
# names(baseline_all_list_nopk) <- distances

summarize_model_dat <- function(dist, pulltimes=c(25,56), baselist, pk_){
  dist.list.of.files <-  all.list.of.files[grepl(paste0("dist_", dist, "."), all.list.of.files)]
  baseline <- baselist[[paste0(dist)]]$baseline
  baselineRes <- baselist[[paste0(dist)]]$baselineRes
  baseline_maxD <- baselist[[paste0(dist)]]$baseline_maxD
  cat(baselist[[paste0(dist)]]$dist," ----  ", ifelse(pk_==0, "with pk", "nopk"), "\n")
  
  run_sum <- function(fname, baseline=baseline, baselineRes=baselineRes, baseline_maxD=baseline_maxD, pk_){
      require(magrittr)
      dat <- read_tsv(fname, col_types = cols()) %>%
        filter(time %in% pulltimes & pk==pk_) %>% 
        rename(scen_g = scenario, scenario_f = id) %>%
        mutate(doy = time + yday(mdy("6-20-2013"))) %>% 
        generatedist(x = ., baseline_maxD_df = baseline_maxD, baselineRes_df = baselineRes)
      # print(head(dat))
      
      summary_full <- 
        dat %>%
        # filter(time %in% c(25,56)) %>% 
        group_by(scen_g, scenario_f, time) %>% nest() %>% 
        mutate(
          Quantiles_plarge = map(data, ~ quantile(.$pSafe,probs = c(0.025,0.1,0.25, 0.5, 0.75,0.9,0.975), na.rm=T)),
          Quantiles_relT = map(data, ~ quantile(.$relT,probs = c(0.025,0.1,0.25, 0.5, 0.75,0.9,0.975), na.rm=T)),
          Quantiles_r0 = map(data, ~ quantile(.$rel0,probs = c(0.025,0.1,0.25, 0.5, 0.75,0.9,0.975), na.rm=T)),
          Quantiles_r1 = map(data, ~ quantile(.$rel1,prob = c(0.025,0.1,0.25, 0.5, 0.75,0.9,0.975), na.rm=T)),
          Quantiles_T = map(data, ~ quantile(.$TotalBirds,prob = c(0.025,0.1,0.25, 0.5, 0.75,0.9,0.975), na.rm=T))) %>% 
        unnest( pLarge = map(Quantiles_plarge, broom::tidy),relT=map(Quantiles_relT, broom::tidy),
                r0 = map(Quantiles_r0, broom::tidy), r1 = map(Quantiles_r1, broom::tidy), TotalBirds = map(Quantiles_T, broom::tidy)) %>% 
        rename(quant_pLarge = names, pLarge=x, 
               quant_relT = names1, relT=x1, 
               quant_r0 = names2, r0=x2, 
               quant_r1=names3, 
               r1 = x3, Q_Total = names4, TotalBirds = x4) %>% 
        mutate(dist=dist)
      rm(dat)
      # scen_means_full <- dat %>%
      #   # filter(time %in% c(25,56)) %>% 
      #   group_by(scen_g, scenario_f, time) %>% nest() %>% 
      #   mutate(mean_pLarge =  map(data, ~ mean(.$pSafe, na.rm = T)),
      #          mean_relT = map(data, ~ mean(.$relT, na.rm=T))) %>% unnest(mean_pLarge, mean_relT) %>% select(-data) %>% mutate(dist=dist)
      return(summary_full)#list(quantiles = summary_full, means = scen_means_full))
    }
  outlist <- map_df(dist.list.of.files,run_sum,baseline_maxD=baseline_maxD,baselineRes=baselineRes, pk_=pk_) 
  return(outlist)
}
# # test <- summarize_model_dat(5000, baselist = baseline_all_list_nopk, pk = 1)
# alloutpk <- map_df(distances, summarize_model_dat, baselist = baseline_all_list_pk, pk = 0)
# write_rds(alloutpk, "../Confrontation_chapter/Scenarios_summary_withDistance_pk2.rds")
# alloutnopk <- map_df(distances, summarize_model_dat, baselist = baseline_all_list_nopk, pk = 1)
# write_rds(alloutnopk, "../Confrontation_chapter/Scenarios_summary_withDistance_nopk2.rds")

# test <- read_tsv(file = all.list.of.files[grepl("PredTime-1.", all.list.of.files)][[4]])
# ggplot(test, aes(time, site0)) + facet_wrap(~pk) + stat_summary(fun.y='mean', geom='line')+
#   stat_summary(fun.y='mean', geom='line', aes(y=site1), colour = 'red')


predictandplot <- function(dat, mod_dat, returnType, bringyourowndat=F, relAbundace=F, sepPlots=F){
  if(isTRUE(bringyourowndat)) scen_dat <- dat
  else{
  if(isTRUE(relAbundace)){
      scen_dat <- dat %>% left_join(mod_dat) %>% 
        filter(Q_Total %in% c("2.5%", "50%", "97.5%")) %>% 
        select(Year, Date,doy, scenario_f,  quant_r0,quant_r1, r0, r1) %>% 
        unite('small',quant_r0, r0 ) %>% 
        unite('large', quant_r1, r1) %>% 
        gather(key, value, small:large) %>% 
        separate(value, c("Quant",  "relEst"), sep="_") %>% 
        mutate(relEst = as.numeric(relEst)) %>% 
        spread(Quant, relEst)
   }  else{
  scen_dat <- dat %>% left_join(mod_dat, by = c("Year", "scenario_f")) %>% filter(Q_Total %in% c("2.5%", "50%", "97.5%"))
  # cat(nrow(dat),"-- ", nrow(mod_dat),"-- ", nrow(scen_dat), "\n")
  scen_dat%<>% 
    
    select(Year, Date,doy, scenario_f,  Q_Total, pLarge, TotalBirds) %>% 
    gather(key = VarType, value = estimate, pLarge:TotalBirds) %>% 
    unite("t1", c("Q_Total", "VarType"),sep = "_") %>% 
    spread(key = t1, value = estimate) %>% 
    rename(uci_pTotal = `97.5%_pLarge`,
           uci_Total = `97.5%_TotalBirds`,
           lci_pTotal = `2.5%_pLarge`,
           lci_Total = `2.5%_TotalBirds`,
           pTotal = `50%_pLarge`,
           totalBirds = `50%_TotalBirds`)} 
  }
  if(isTRUE(relAbundace)){
    if(isTRUE(bringyourowndat)) {
      colsr <- RColorBrewer::brewer.pal(9, name = "Set1")
      scenplot <- ggplot(scen_dat, 
                         aes(Year, Large)) + 
        facet_grid(month(Date, label = T)~.)+
        geom_ribbon(aes(ymin=lci_r1, ymax=uci_r1),alpha=0.25,fill='grey', colour = 'grey' ) +
        geom_ribbon(aes(ymin=lci_r0, ymax=uci_r0),alpha=0.25,fill='grey', colour = 'grey' ) +
        geom_line(colour = colsr[[2]])+ 
        geom_line(aes(y=Small), colour = colsr[[1]], linetype='dashed')+ 
        theme( axis.line = element_blank(), 
               legend.position = 'bottom') +
        labs(x="Parameter adjustment", y="Relative change from baseline", colour="Site") 
    } else{
      
    scenplot <- ggplot(scen_dat, 
           aes(Year, `50%`, colour =key)) + 
      facet_grid(~month(Date, label = T)~.)+
      geom_ribbon(data= filter(scen_dat,!is.na(`2.5%`)|!is.na(`97.5%`)),aes(ymin=`2.5%`, ymax=`97.5%`, group = key),alpha=0.25,fill='grey', colour = 'grey' ) +
      geom_line(aes(y=`50%`))+ 

      theme( axis.line = element_blank(), 
             legend.position = 'bottom') +
      labs(x="Year", y="Relative change from baseline", colour="Site") + 
      scale_color_brewer(type='qual', palette="Set1", direction=-1)
    }
    
    
  } else{
    if(isTRUE(sepPlots)){
      scenplot_prop <-   ggplot(scen_dat, aes(x=Year)) +
        geom_ribbon(aes(ymax = uci_pTotal, ymin=lci_pTotal), fill = 'grey', alpha =0.75) +
        geom_line(aes(y=pTotal)) + facet_grid(.~month(Date, label = T,abbr=F))+
        ylim(0,1)+
        labs(x = "", y="") 
      if(!isTRUE(bringyourowndat)) scenplot_prop <- scenplot_prop + theme(strip.text = element_blank())
        # labs(x = "Year", y="Proportion of Birds at Large Site") 
    scenplot_Total <- ggplot(scen_dat, aes(x=Year)) + 
      geom_ribbon(aes(ymax = uci_Total, ymin=lci_Total), fill = 'grey', alpha =0.75) +
      geom_line(aes(y= totalBirds), linetype='dashed') +
      facet_wrap(~month(Date, label = T,abbr=F))+
      labs(x = "", y="") 
      # labs(x = "Year", y="Number of Birds") 
    if(!isTRUE(bringyourowndat)) scenplot_Total <- scenplot_Total + theme(strip.text = element_blank())
    scenplot <- tibble(pLarge = scenplot_prop, TotalBirds = scenplot_Total)
      
    } else{
  max_c <- max(scen_dat$uci_Total)
  scenplot <-   ggplot(scen_dat, aes(x=Year)) + geom_ribbon(aes(ymax = uci_Total, ymin=lci_Total), fill = 'grey', alpha =0.25) +
    geom_ribbon(aes(ymax = uci_pTotal*max_c, ymin=lci_pTotal*max_c), colour = 'grey', alpha =0.25) +
    geom_line(aes(y= totalBirds), linetype='dashed') +
    geom_line(aes(y=pTotal*max_c)) + facet_grid(month(Date, label = T)~.)+
    scale_y_continuous(sec.axis = sec_axis(~./max_c,name = "Proportion of Birds at Large Site")) +
    labs(x = "Year", y="Number of Birds", ec.axis = "Cats") }
  }
  if(returnType=='data') return(scen_dat)
  if(returnType=='plot') return(scenplot)
  if(returnType %in% c("both", "all")) return(list(plot = scenplot, data=scen_dat))
  
}







