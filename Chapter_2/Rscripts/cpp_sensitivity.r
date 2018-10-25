require(tidyverse)

guides <- readODS::read_ods("../cpp_version/Migration_Model/OrganizedResults/SensitivityGuide.ods", sheet = "Sheet1",
                            col_names = F, skip = 1)
  #read_csv("../cpp_version/Migration_Model/OrganizedResults/SensitivityGuide.csv")
names(guides) <- c("run", "var", "delta", "d", "f")
# 
# fileNames <- list.files(
#   path = "../cpp_version/Migration_Model/output/SenAnal/noU", 
#   pattern = "SenRes",full.names = T)
# 
# results <- map_df(fileNames, read_tsv, col_names=T) %>% 
#   left_join(results, guides, by = 'run')
# # names(results) <- c("run","time","site0","site1")
# No U - No PK
results_all <- read_tsv("../cpp_version/Migration_Model/OrganizedResults/noU/sensitivity/Sensitivity_noU.txt",#output/SenAnal/noU/noPriorK.txt", 
                    col_names = c("pk", "run","time","site0","site1")) %>% 
  left_join( guides, by = 'run') %>% mutate(type = ifelse(pk==0, "PK\nNo u", "No PK\nNo u") )

results_all_u <- read_tsv("../cpp_version/Migration_Model/OrganizedResults/withU/sensitivity/Sensitivity_withU.txt",#output/SenAnal/noU/noPriorK.txt", 
                          col_names = c("pk", "run","time","site0","site1")) %>% 
  left_join( guides, by = 'run') %>% mutate(type = ifelse(pk==0, "PK\nWith u", "No PK\nWith u") )

# results <- read_tsv("../cpp_version/Migration_Model/OrganizedResults/noU/sensitivity/no_pk/noPriorK.txt",#output/SenAnal/noU/noPriorK.txt", 
#                     col_names = c("run","time","site0","site1")) %>% 
#   left_join( guides, by = 'run')
# 
# # No U - PK
# results_pk <- read_tsv("../cpp_version/Migration_Model/OrganizedResults/noU/sensitivity/pk/PriorK.txt", 
#                        col_names = c("run","time","site0","site1"))%>% 
#   left_join( guides, by = 'run')
# U - No PK
# results_u <- read_tsv("../cpp_version/Migration_Model/OrganizedResults/withU/sensitivity/no_pk/noPriorK.txt",
#                       col_names = c("run","time","site0","site1"))%>% 
#   left_join( guides, by = 'run')
# # U - PK
# results_u_pk <- read_tsv("../cpp_version/Migration_Model/OrganizedResults/withU/sensitivity/pk/PriorK.txt", 
#                          col_names = c("run","time","site0","site1"))%>% 
#   left_join( guides, by = 'run')
# results <- filter(results, time %in% c(25,56))

# sen_analysis <- left_join(results, guides, by = 'run') # %>% 
  # mutate(pSafe = site1 / (site0+site1), ratioSafe = site1/site0) %>% filter(!is.na(pSafe))


# ggplot(sen_analysis, aes(var, site0/site1, group = delta, 
#                          colour = as.factor(delta)) ) + 
#   stat_summary(fun.data = "mean_cl_boot") +
#   facet_wrap(~time, scales='free') +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# mean_delta <-sen_analysis %>% # filter(sen_analysis, var== "Baseline") %>% 
#   group_by(run, var,delta, time) %>% 
#   summarise_at(.funs = mean, .vars=vars(site0, site1), na.rm=T) %>% ungroup #%>% #pSafe, ratioSafe
#   # filter(!is.na(pSafe))


calculate_elasticities  <- function(dat,  dates_ = c(25,56)){
  data_i <- filter(dat, time %in% dates_) %>% 
      group_by(var,delta, time,pk,type) %>% 
      summarise_at(.funs = mean, .vars=vars(site0, site1), na.rm=T) %>% ungroup 
  baseline <- data_i %>% filter(var== "Baseline") %>% 
    select(time, site0, site1) %>% #pSafe
    rename( baseline0= site0, baseline1=site1)#baseline_p = pSafe,
  
  elast_tab <- data_i %>% filter(var!= "Baseline") %>% 
    left_join(baseline) %>% ungroup %>% group_by(pk,type) %>% 
    mutate(
      # baseline_p = ifelse(time == 25, baseline[["pSafe"]][[1]], baseline[["pSafe"]][[2]]), 
      # delta_from_baseline = pSafe - baseline_p,
      delta0 = site0 - baseline0,
      delta1 = site1 - baseline1,
      # rdelta_from_baseline = delta_from_baseline / baseline_p,
      # elast = delta_from_baseline/delta,
      pchange0 = (delta0 / baseline0),
      pchange1 = delta1 / baseline1,
      elast0= ifelse(abs(delta)==0.05,pchange0 / delta,pchange0),
      elast1 = ifelse(abs(delta)==0.05,pchange1 / delta,pchange1) )  %>% 
    filter(!is.na(elast0) | !is.na(elast1)) %>% 
    mutate(age = ifelse(time<40, "Adult", "Juvenile")) %>% ungroup
  
  return(elast_tab 
           )
  
}


# dats <- list(results, results_u, results_pk, results_u_pk)
# mods <- c("No PK\nNo u", "No PK\nWith u", "PK\nNo u", "PK\nWith u")
# 
# tst <- calculate_elasticities(results_all)
output_raw <- map_df(list(results_all, results_all_u),calculate_elasticities) 
output <- output_raw %>% 
  mutate(ab_delta=abs(delta)) %>% #map_df(1:4, function(x) calculate_elasticities(dat = dats[[x]], type = mods[[x]])) %>% 
  # map_df(list(results_all, results_all_u),calculate_elasticities) %>% 
  select(age, var, ab_delta , elast0, elast1, pk,type) %>% 
  rename("model" = type) %>% 
  group_by(age, var, model,ab_delta) %>% 
  summarize(elast0 =mean(elast0, na.rm=T), elast1=mean(elast1,na.rm=T)) %>% 
  ungroup %>% 
  unite(id, age, var, model,ab_delta, sep = ">") %>%  
  gather("Site", "E", elast0:elast1) %>% 
  separate(id, c("Age", "var", "Model", "delta"), sep = ">") %>% 
  mutate(Site = ifelse(Site == "elast0", "Small", "Large")) %>% 
  unite(model, Model, Site, sep = "\n") %>% 
  ungroup %>% 
  spread( key = model, E,sep = NULL ) 












# 
# baseline <- mean_delta %>% filter(var== "Baseline") %>% 
#   select(time, site0, site1) %>% #pSafe
#   rename( baseline0= site0, baseline1=site1)#baseline_p = pSafe,
# 
# sen_res <- mean_delta %>% 
#   left_join(baseline) %>% ungroup %>% 
#   filter(baseline0+baseline1 > 10) %>% 
#   # filter(delta <=0.1 & delta >=-0.1) %>% 
#   rowwise %>%
#   mutate(
#   # baseline_p = ifelse(time == 25, baseline[["pSafe"]][[1]], baseline[["pSafe"]][[2]]), 
#   # delta_from_baseline = pSafe - baseline_p,
#   delta0 = site0 - baseline0,
#   delta1 = site1 - baseline1,
#   # rdelta_from_baseline = delta_from_baseline / baseline_p,
#   # elast = delta_from_baseline/delta,
#   pchange0 = (delta0 / baseline0),
#   pchange1 = delta1 / baseline1,
#   elast0= pchange0 / delta,
#   elast1 = pchange1 /delta  ) %>% 
#   ungroup %>% #arrange(desc(abs(elast)))
#   filter(!is.na(elast0) | !is.na(elast1)) %>% 
#   mutate(age = ifelse(time<40, "Adults", "Juveniles")) %>% 
#   filter( time %in% c(25, 56))
# hist(sen_res$pchange1, breaks = 20)
# 
# ggplot(sen_res, aes(var,elast0, colour = as.factor(time) )) +
#   geom_point()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   facet_wrap(~delta)
# 
# ggplot(sen_res, aes(var,elast1 )) +
# theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
# stat_summary(fun.data = 'mean_cl_boot',
#              position = position_nudge( x= -0.1)) +
#   stat_summary(fun.data = 'mean_cl_boot', aes(y=elast0), colour = 'red',
#                  position = position_nudge( x= 0.1)) +facet_wrap(~age)
#   
#   require(lubridate)
# ggplot(filter(sen_res, grepl("pred", var)),
#               aes(mdy("06-20-2013")+ time, elast1, colour = var)) + geom_point() 
# 
# # sen_res %>% filter(!is.na(delta)) %>% 
# #   mutate(ID = paste0(time,"_", delta)) %>% #.[["ID"]]
# #   select(var, ID,   elast) %>% 
# #   spread(key = ID, value = elast) %>% 
# #    write_csv("../VariableSources/ElasticitywithPK_noU.csv")
#   
# 
# sen_res %>% 
#   filter(!is.na(elast1) & !is.na(elast0) & abs(elast0) != Inf & abs(elast1) != Inf) %>% 
#   group_by(age, var) %>% 
#   summarize(e0 = median(elast0, na.rm=T),
#             e1 = median(elast1, na.rm=T),
#             lci0 = quantile(elast0,probs=0.025, na.rm=T),
#             lci1 = quantile(elast1,probs=0.025, na.rm=T),
#             uci0 = quantile(elast0,probs=0.975, na.rm=T),
#             uci1 = quantile(elast1,probs=0.975, na.rm=T),
#             mn.e0 = mean(elast0, na.rm=T),
#             mn.e1 = mean(elast1, na.rm=T)
#             ) %>% 
#   write_csv("../VariableSources/ElasticitywithPK_noU_medci.csv")
#   
#   
# sen_res %>% group_by(var) %>% summarize(v_E=var(elast)) %>%# .[['var(elast)']]%>% hist(, breaks = 30)
#   arrange(desc(v_E))

