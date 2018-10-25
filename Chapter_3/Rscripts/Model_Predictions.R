require(tidyverse)
summary_predictions <- read_rds("../Confrontation_chapter/.rds/Scenarios_summary_withDistance_pk2.rds")

quantiles <- c("50%", "2.5%","97.5%" )
plotpredictions <- function(dat, t_, dist_, bydist=F){
  quantiles <- c("50%", "2.5%","97.5%" )
  
if(isTRUE(bydist))
  {
  pLarge  <- dat %>% 
    filter(time%in%t_ & quant_pLarge %in%quantiles & scen_g == dist_) %>% 
    dplyr::select(dist, scenario_f, time, quant_pLarge, pLarge, dist) %>% 
    group_by(dist, scenario_f) %>% 
    spread(key = quant_pLarge,value=pLarge) %>% mutate(Age = ifelse(time<42,"Adults", "Juveniles"))
  relT  <- dat %>% 
    filter(time%in%t_& quant_pLarge %in%quantiles & scen_g == dist_) %>% 
    dplyr::select(dist, scenario_f, time, quant_relT, relT, dist) %>% 
    group_by(dist, scenario_f) %>% 
    spread(key = quant_relT,value=relT)%>% mutate(Age = ifelse(time<42,"Adults", "Juveniles"))
  
  plot_plarge <- 
    ggplot(pLarge, 
           aes(scenario_f, `50%`)) + 
    facet_wrap(~monthdate,nrow=2 ,scales='free_x') +
    geom_ribbon(data= filter(pLarge,!is.na(`2.5%`)|!is.na(`97.5%`)),aes(ymin=`2.5%`, ymax=`97.5%`),alpha=0.25,fill='grey', colour = 'grey' ) +
    # geom_ribbon(data= filter(d,!is.na(`25%_r1`)|!is.na(`75%_r1`)),aes(ymin=`25%_r1`, ymax=`75%_r1`),alpha=0.5, colour='grey') +
    geom_line(aes(y=`50%`))+ 
    facet_grid(Age~dist) +
    ylim(0,1)+
    # geom_line(aes(y=`50%_r1`), colour = 'blue')+ 
    # geom_line(aes(linetype = Quant, colour = key)) +
    theme( axis.line = element_blank(), 
           legend.position = 'bottom') +
    labs(x="Parameter adjustment", 
         y="Proportion of Birds at Large Site", 
         colour="Site", 
         title=paste0("Scenario - ", dist_)) + 
    scale_color_brewer(type='qual', palette="Set1", direction=-1)
  
  
  plot_relT <- 
    ggplot(relT, 
           aes(scenario_f, `50%`)) + 
    facet_wrap(~monthdate,nrow=2 ,scales='free_x') +
    geom_ribbon(data= filter(relT,!is.na(`2.5%`)|!is.na(`97.5%`)),aes(ymin=`2.5%`, ymax=`97.5%`),alpha=0.25,fill='grey', colour = 'grey' ) +
    # geom_ribbon(data= filter(d,!is.na(`25%_r1`)|!is.na(`75%_r1`)),aes(ymin=`25%_r1`, ymax=`75%_r1`),alpha=0.5, colour='grey') +
    geom_line(aes(y=`50%`))+ 
    facet_grid(Age~dist) +
    # geom_line(aes(y=`50%_r1`), colour = 'blue')+ 
    # geom_line(aes(linetype = Quant, colour = key)) +
    theme( axis.line = element_blank(), 
           legend.position = 'bottom') +
    labs(x="Parameter adjustment", y="Relative change in total numbers from baseline",
         colour="Site", 
         title=paste0("Scenario - ", dist_)) + 
    scale_color_brewer(type='qual', palette="Set1", direction=-1)
  return(list(relT=plot_relT, pLarge=plot_plarge))
  
  }
  
pLarge  <- dat %>% 
  filter(time==t_ & quant_pLarge %in%quantiles & dist == dist_) %>% 
  dplyr::select(scen_g, scenario_f, time, quant_pLarge, pLarge, dist) %>% 
  group_by(scen_g, scenario_f) %>% 
  spread(key = quant_pLarge,value=pLarge)

rel_SL  <- dat %>% 
  filter(time==t_& quant_pLarge %in%quantiles & dist == dist_) %>% 
  dplyr::select(-quant_relT, -relT, -quant_pLarge, -pLarge) %>%
  unite('small',quant_r0, r0 ) %>%
  unite('large', quant_r1, r1) %>%
  gather(key, value, small:large) %>% separate(value, c("Quant",  "relEst"), sep="_") %>% mutate(relEst = as.numeric(relEst)) %>%
  spread(Quant, relEst)
plot_SL <- ggplot(rel_SL, 
       aes(scenario_f, `50%`, colour =key)) + 
  # facet_wrap(~monthdate,nrow=2 ,scales='free_x') +
  geom_ribbon(data= filter(rel_SL,!is.na(`2.5%`)|!is.na(`97.5%`)),aes(ymin=`2.5%`, ymax=`97.5%`, group = key),alpha=0.25,fill='grey', colour = 'grey' ) +
  # geom_ribbon(data= filter(d,!is.na(`25%_r1`)|!is.na(`75%_r1`)),aes(ymin=`25%_r1`, ymax=`75%_r1`),alpha=0.5, colour='grey') +
  geom_line(aes(y=`50%`))+ 
  facet_wrap(~scen_g, scales='free') +
  # geom_line(aes(y=`50%_r1`), colour = 'blue')+ 
  # geom_line(aes(linetype = Quant, colour = key)) +
  theme( axis.line = element_blank(), 
         legend.position = 'bottom') +
  labs(x="Parameter adjustment", y="Relative change from baseline ", colour="Site", title=paste0("Distance - ", dist_, "km; Date - ", t_)) + scale_color_brewer(type='qual', palette="Set1", direction=-1)


plot_plarge <- 
ggplot(pLarge, 
       aes(scenario_f, `50%`)) + 
  facet_wrap(~monthdate,nrow=2 ,scales='free_x') +
  geom_ribbon(data= filter(pLarge,!is.na(`2.5%`)|!is.na(`97.5%`)),aes(ymin=`2.5%`, ymax=`97.5%`),alpha=0.25,fill='grey', colour = 'grey' ) +
  # geom_ribbon(data= filter(d,!is.na(`25%_r1`)|!is.na(`75%_r1`)),aes(ymin=`25%_r1`, ymax=`75%_r1`),alpha=0.5, colour='grey') +
  geom_line(aes(y=`50%`))+ 
  facet_wrap(~scen_g, scales='free') +
  # geom_line(aes(y=`50%_r1`), colour = 'blue')+ 
  # geom_line(aes(linetype = Quant, colour = key)) +
  theme( axis.line = element_blank(), 
         legend.position = 'bottom') +
  labs(x="Parameter adjustment", y="Proportion of Birds at Large Site", colour="Site", title=paste0("Distance - ", dist_, "km; Date - ", t_)) + scale_color_brewer(type='qual', palette="Set1", direction=-1)


relT  <- dat %>% 
  filter(time==t_& quant_pLarge %in%quantiles & dist == dist_) %>% 
  dplyr::select(scen_g, scenario_f, time, quant_relT, relT, dist) %>% 
  # dplyr::select(-quant_relT, -relT, -quant_pLarge, -pLarge) %>% 
  # unite('small',quant_r0, r0 ) %>% 
  # unite('large', quant_r1, r1) %>% 
  # gather(key, value, small:large) %>% separate(value, c("Quant",  "relEst"), sep="_") %>% mutate(relEst = as.numeric(relEst)) %>% 
  # spread(Quant, relEst)
  group_by(scen_g, scenario_f) %>% 
  spread(key = quant_relT,value=relT)

plot_relT <- 
ggplot(relT, 
       aes(scenario_f, `50%`)) + 
  facet_wrap(~monthdate,nrow=2 ,scales='free_x') +
  geom_ribbon(data= filter(relT,!is.na(`2.5%`)|!is.na(`97.5%`)),aes(ymin=`2.5%`, ymax=`97.5%`),alpha=0.25,fill='grey', colour = 'grey' ) +
  # geom_ribbon(data= filter(d,!is.na(`25%_r1`)|!is.na(`75%_r1`)),aes(ymin=`25%_r1`, ymax=`75%_r1`),alpha=0.5, colour='grey') +
  geom_line(aes(y=`50%`))+ 
  facet_wrap(~scen_g, scales='free') +
  # geom_line(aes(y=`50%_r1`), colour = 'blue')+ 
  # geom_line(aes(linetype = Quant, colour = key)) +
  theme( axis.line = element_blank(), 
         legend.position = 'bottom') +
  labs(x="Parameter adjustment", y="Relative change in total numbers from baseline", colour="Site", title=paste0("Distance - ", dist_, "km; Date - ", t_)) + 
  scale_color_brewer(type='qual', palette="Set1", direction=-1)

return(list(relT=plot_relT, pLarge=plot_plarge, SL=plot_SL))
}

# a <- plotpredictions(summary_predictions,t_ = 56,dist_ = 1300)

allplots_pk <- pmap(expand.grid(t_=c(25,56), dist_=distances), .f = plotpredictions, dat=summary_predictions) %>% 
  transpose

allplots_pk_scenario_by_dist <- pmap(list(t_= rep(list(c(25,56)),6), dist_=unique(summary_predictions$scen_g)), .f = plotpredictions, dat=summary_predictions,bydist=T) %>% 
  transpose


no_pk_summary_predictions <- read_rds("../Confrontation_chapter/.rds/Scenarios_summary_withDistance_nopk2.rds")

allplots_nopk <- pmap(expand.grid(t_=c(25,56), dist_=distances), .f = plotpredictions, dat=no_pk_summary_predictions) %>% 
  transpose

# pdf("plotspk.pdf")
# for(i in allplots_pk$relT) print(i)
# for(j in allplots_pk$pLarge) print(j)
# for(k in allplots_pk$SL) print(k)
# dev.off()
# 
# pdf("plots_nopk.pdf")
# for(i in allplots_nopk$relT) print(i)
# for(j in allplots_nopk$pLarge) print(j)
# for(k in allplots_nopk$SL) print(k)
# dev.off()

