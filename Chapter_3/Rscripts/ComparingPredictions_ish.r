# require(conflicted)
require(tidyverse)
require(lubridate)
require(magrittr)
select <- dplyr::select
filter <- dplyr::filter
source("Summary_of_Scenarios.r")
# dat <- read_rds(".rds/scenario_summary_full_noDraw_july.rds")
if (!exists("mean.snowmelt")) source("../../../SharingFiles_ForWindows/ShorebirdSurveyDatabase/Snowmelt/Min.Snow.Date.R", chdir = T)
surveydates %<>% mutate_at(vars(Year, Month), as.numeric) %>% # <- read_rds("../Confrontation_chapter/surveydates.rds") %>%
  left_join(mean.snowmelt %>% mutate(
    baseline_arrival = 220,
    arrival_after_snowmelt = 100.2,
    ci_arrival = 5.3,
    delta_from_base_arrival_est = mean.snowmelt + arrival_after_snowmelt - baseline_arrival,
    arrival_est = 1.09 * delta_from_base_arrival_est + baseline_arrival,
    lci_arrival_est = 1.09 * delta_from_base_arrival_est - ci_arrival + baseline_arrival,
    uci_arrival_est = 1.09 * delta_from_base_arrival_est + ci_arrival + baseline_arrival
  ) %>% select(
    Year, lci_arrival_est, arrival_est, uci_arrival_est,
    dev.snowmelt
  )) %>%
  mutate(headstart = doy - arrival_est, time = doy - yday(ymd(paste0(Year, "-6-20"))))

if (exists("includethiscrap")) {
  distances <- c(5000)

  baseline_all_list_pk <- map(distances, extract_baseline, pk_ = 0, pulltimes = surveydates$time)

  names(baseline_all_list_pk) <- distances

  modelPredictions_all <- summarize_model_dat(5000, baselist = baseline_all_list_pk, pk = 0, pulltimes = surveydates$time)
  mod_pred_backup <- modelPredictions_all
  require(magrittr)
  modelPredictions_all %<>% right_join(surveydates, by = "time")
  # modelPredictions_all$scen_g %>% unique

  # Population Change -------------------------------------------------------

  # 1. Population Decline
  wesa_pop <- modelPredictions_all %>% filter(scen_g == "WESA Numbers")
  # a. 5% Per Year
  decline <- tibble(Year = 2013:2017, scenario_f = seq(1., by = -0.05, length.out = 5))

  dec_plot <- predictandplot(decline, wesa_pop, "plot")
  dec_plot_rEL <- predictandplot(decline, wesa_pop, "plot", relAbundace = T)
  dec_plot_sep <- predictandplot(decline, wesa_pop, "plot", relAbundace = F, sepPlots = T)


  # 2. Stable Population
  stable <- tibble(Year = 2013:2017, scenario_f = rep(1, 5))

  st_plot <- predictandplot(stable, wesa_pop, "plot")
  st_plot_sep <- predictandplot(stable, wesa_pop, "plot", sepPlots = T)
  st_plot_r <- predictandplot(stable, wesa_pop, "plot", relAbundace = T)

  # 3. Increasing Population 5% of year

  inc <- tibble(Year = 2013:2017, scenario_f = seq(1.2, by = 0.05, length.out = 5))
  inc_plot <- predictandplot(inc, wesa_pop, "plot")
  inc_plot_r <- predictandplot(inc, wesa_pop, "plot", relAbundace = T)

  require(cowplot)




  # Fueling Change ----------------------------------------------------------

  # 1. Global decline
  wesa_food <- modelPredictions_all %>% filter(scen_g == "Food abundance")
  decline_f <- tibble(Year = 2013:2017, scenario_f = round(seq(1.2, by = -0.1, length.out = 5), 1))
  food_dec_ <- predictandplot(decline_f, wesa_food, "plot")
  food_dec_r <- predictandplot(decline_f, wesa_food, "plot", relAbundace = T)


  # Mass decline ------------------------------------------------------------
  # 1. Global decline
  wesa_mass <- modelPredictions_all %>% filter(scen_g == "Mass")
  mass_decline <- predictandplot(decline_f, wesa_mass, "plot")
  mass_decline_r <- predictandplot(decline_f, wesa_mass, "plot", relAbundace = T)



  # Falcon population increase ----------------------------------------------
  falc_pop <- modelPredictions_all %>% filter(scen_g == "Predator Numbers")
  falc_inc <- tibble(Year = 2013:2017, scenario_f = seq(1, by = 0.1, length.out = 5))
  falcon_pop_inc <- predictandplot(falc_inc, falc_pop, "plot")
  falcon_pop_inc_r <- predictandplot(falc_inc, falc_pop, "plot", relAbundace = T)



  # Falcon arrival timing ---------------------------------------------------
  falc_arrival <- modelPredictions_all %>% filter(scen_g == "Predator Timing")
  falcon_arrival <- surveydates %>% select(Year, dev.snowmelt) %>% distinct() %>% mutate(scenario_f = round(-1. * dev.snowmelt, 0)) %>% select(-dev.snowmelt)
  falcon_arrival_plot <- predictandplot(falcon_arrival, falc_arrival, "plot")
  falcon_arrival_plot_r <- predictandplot(falcon_arrival, falc_arrival, "plot", relAbundace = T)

  # pdf("ModelPredictions_nopk.pdf",paper = "USr",width = 12)
  # plot_grid(dec_plot, st_plot, inc_plot, nrow=1, labels=c("Population Decline", "Stable Population", "Increasing Population"),label_y = .95, label_x = .05)
  # plot_grid(food_dec_, mass_decline, falcon_pop_inc, nrow=1, labels=c("Food Decline", "Mass Decline", "Falcon Population"),label_y = .95, label_x = .1)
}
# Survey Results ----------------------------------------------------------

ressurvey <- read_rds("../Confrontation_chapter/.rds/Results_Bootstrapped_withRelC_wmax.rds") %>% # Results_Bootstrapped_BBJEBCombined.rds
  left_join(surveydates %>% mutate_at(vars(Year, Month), as.character)) %>%
  mutate(Year = as.numeric(Year))


resultsplot <- predictandplot(ressurvey, NA, "plot", T)
resultsplot_r <- predictandplot(ressurvey, NA, "plot", bringyourowndat = T, relAbundace = T)

# plot_grid(falcon_arrival_plot, resultsplot, labels = c("Falcon Arrival", "Survey Results"),label_y = .95, label_x = .1)
# dev.off()

# pdf("ModelPredictions_relabundance.pdf",paper = "USr",width = 12)
# plot_grid(dec_plot_rEL, st_plot_r, inc_plot_r, nrow=1, labels=c("Population Decline", "Stable Population", "Increasing Population"),label_y = .95, label_x = .05)
# plot_grid(food_dec_r, mass_decline_r, falcon_pop_inc_r, nrow=1, labels=c("Food Decline", "Mass Decline", "Falcon Population"),label_y = .95, label_x = .1)
# plot_grid(falcon_arrival_plot_r, resultsplot_r, labels = c("Falcon Arrival", "Survey Results"),label_y = .95, label_x = .1)
# dev.off()


# predictandplot(returnType = ,bringyourowndat = ,relAbundace = ,sepPlots = )
if (exists("includethiscrap")) {
  modelRuns <- tibble(
    dat = map_df(list(ressurvey, falcon_arrival, falc_inc, decline_f, decline_f, decline, stable, inc), nest)$data,
    mod_dat = map_df(list(falc_arrival, falc_arrival, falc_pop, wesa_mass, wesa_food, wesa_pop, wesa_pop, wesa_pop), nest)$data,
    bringyourowndat = c(T, rep(F, 7))
  ) %>%
    mutate(propPlots = pmap(., predictandplot, returnType = "plot", sepPlots = T, relAbundace = F)) %>%
    mutate(names = c(
      "Survey Results",
      "Predator Timing",
      "Predator Numbers",
      "Mass",
      "Food abundance",
      "Population\nDecline",
      "Population\nStable",
      "Population\nIncrease"
    )) %>%
    select(names, propPlots) %>%
    mutate(
      pLarge = .$propPlots %>% transpose() %>% .[["pLarge"]],
      TotalBirds = .$propPlots %>% transpose() %>% .[["TotalBirds"]]
    )
  require(cowplot)

  finalPlot_props <-
    plot_grid(
      plot_grid(plotlist = modelRuns$pLarge, ncol = 1, rel_heights = c(0.25, rep(0.125, 7))),
      plot_grid(plotlist = map(modelRuns$names, textGrob), ncol = 1),
      ncol = 2, rel_widths = c(0.8, 0.2)
    )


  finalPlot_total <-
    plot_grid(
      plot_grid(plotlist = modelRuns$TotalBirds, ncol = 1),
      plot_grid(plotlist = map(modelRuns$names, textGrob), ncol = 1),
      ncol = 2, rel_widths = c(0.8, 0.2)
    )


  # pdf("ModelPredictions2.pdf",paper = "US", height = 20, width = 8)
  #
  # finalPlot_props
  # finalPlot_total
  # dev.off()









  # Arrival Change ----------------------------------------------------------

  wesa_arrival <- modelPredictions_all %>% filter(scen_g == "WESA Timing")
  wesa_var_arrival <- tibble(Year = 2013:2017, scenario_f = c(-2, -1, 0, 1, 2))
  wesa_arrival_plot <- predictandplot(wesa_var_arrival, wesa_arrival, "plot")
}

falcons <- read_rds("../Confrontation_chapter/.rds/falcons.rds") %>%
  group_by(Month, Year) %>%
  summarize(
    falcons = mean(max.count.w.zeros),
    sum_falcons = sum(max.count.w.zeros)
  ) %>%
  group_by(Month) %>%
  mutate(
    f_mean = (falcons - mean(falcons)) / mean(falcons),
    f_sum = (sum_falcons - mean(sum_falcons)) / mean(sum_falcons)
  ) %>%
  ungroup() %>%
  mutate_at(vars(Month, Year), as.numeric) %>%
  left_join(surveydates)



# pred_predict <- dat$quants%>% filter(quant_pLarge=="50%") %>%
#   mutate(doy = yday(mdy("6-20-2013") + time)) %>%
#   dplyr::filter(doy %in% surveydates$doy&scen_g == "Predator Timing") %>%
#   mutate(estarrival = 224 - scenario_f, headstart_model = doy-estarrival) #%>%
#   # left_join(surveydates, by = 'doy')
#
#
#
# # rel_p <- pred_predict %>% filter(scenario_f==0) %>% .[['pLarge']] %>% mean
#
#
# pred_predict %<>% right_join(ressurvey %>% select( Year,doy, headstart) %>%
#                                 mutate_at(vars(Year), as.numeric) %>%
#                                 mutate(headstart_data = round(headstart,0)), by = c("headstart_model"="headstart_data")) %>%
#    filter(doy.x==doy.y)
# #   filter(scenario_f == round(headstart,0)) %>%  mutate(pLarge_r = pLarge - rel_p)
#
# # pred_predict %<>% filter(doy==round(headstart.y,0))
#
# # plot_predictions_snowmelt <- pred_predict %>%
# #   ggplot(aes(scenario_f, pLarge, colour = as.factor(Year))) +
# #   stat_summary(fun.y='mean', geom='line') + ylim(0,1) +facet_wrap(~Month) +
# #   scale_x_reverse()+
# #   scale_color_brewer(type='qual', palette='Set1')+
# #   geom_pointrange(data=ressurvey, aes(-1*dev.snowmelt, statistic,ymin= lower_ci, ymax = upper_ci, colour = Year)) +
# #   labs(x="Expected Falcon Arrival from Early (left) to Late (right)",
# #        y="Proportion of birds found at the large sites", colour = "Year")
# #
# # ggsave(plot_predictions_snowmelt, filename = "M", width=8, height=6)
#
Headstartplot <- ggplot(ressurvey, aes(headstart, pTotal, shape = Month)) +
  geom_errorbarh(aes(xmin = headstart - 5.3, xmax = headstart + 5.3, colour = as.factor(Year)), alpha = 0.5) +
  geom_pointrange(aes(
    ymin = lci_pTotal, ymax = uci_pTotal,
    colour = as.factor(Year)
  )) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  geom_smooth(aes(group = 1), span = .9, se = F) +
  ylim(0, 1) +
  labs(
    x = "Headstart in front of falcon front",
    y = "Proportion of birds found at the large sites", colour = "Year"
  )
# geom_point(data=pred_predict, aes(headstart_model, pLarge-.65, colour = as.factor(Year)),  shape = 19)
#
# Headstartplot_r <- ggplot(ressurvey, aes(headstart, Large, shape=Month))+
#   # geom_errorbarh(aes( xmin=headstart-5.3, xmax=headstart+5.3,colour = Year), alpha =0.5)+
#   # geom_pointrange(aes(ymin= lci_pTotal, ymax = uci_pTotal,
#   #                     colour = Year)) +
#   # geom_smooth(aes(group = 1), span = .9, se=F)+
#   # ylim(0,1) +
#   geom_point() + geom_point(aes(y=Small), colour = "red")+
#   labs(x="Headstart in front of falcon front",
#        y="Proportion of birds found at the large sites", colour = "Year")
#
#
#   # geom_point(data=pred_predict, aes(headstart_model, pLarge))
#
#
# ggplot() +   geom_point(data=pred_predict, aes(headstart_model, (pLarge-.5)*2, colour = as.factor(Year)),  shape = 19)
#
#
#
#
# plot_predictions_snowmelt <- ressurvey %>%
#   ggplot(aes(arrival_est, Large)) +
#   geom_line(colour = 'Blue') +
#   geom_line(aes(y=Small), colour = 'Red') +
#   facet_wrap(~Month)
#
#
# plot_predictions_head_rel <- ressurvey %>%
#   ggplot(aes(headstart, Large)) +
#   geom_pointrange(aes(ymax=uci_r1, ymin=lci_r1),colour = 'Blue', position = position_nudge(x=-0.2)) +
#   geom_pointrange(aes(ymax=uci_r0, ymin=lci_r0,y=Small), colour = 'Red', position = position_nudge(x=0.2)) +
#   geom_smooth(aes(group = 1), span = .9, se=F)+
#   geom_smooth(aes(group = 1,y=Small), colour = 'Red', span = .9, se=F)
#
# outsideres <- ressurvey[ressurvey$rT< -0.5,] %>% mutate(rT = -0.5)
# mround <- function(x,base){
#   base*round(x/base)
# }
#
# outjoiner_res <- ressurvey %>%
#   bind_rows(outsideres) %>%
#   #select( Year,doy, rT) %>%
#   mutate_at(vars(Year), as.numeric) %>%
#   mutate(joiner = paste(doy, format(mround(rT,5),nsmall=2), sep = "_"))



# pop_pred <- dat$quants%>% filter(quant_relT=="50%") %>%
#   mutate(doy = yday(mdy("6-20-2013") + time)) %>%
#   dplyr::filter(doy %in% surveydates$doy&scen_g == "WESA Numbers") %>%
#   mutate(rT = scenario_f-1, joiner = paste(doy, format(mround(rT, 5),nsmall=2), sep = "_")) %>%
#   right_join(outjoiner_res, by = c("joiner", "doy")) %>%
#   group_by(doy) %>%
#   slice(which.min(abs(rT.x-rT.y))) %>% ungroup
#
# ci_mod <-
# dat$quants%>% filter(quant_pLarge%in%c("2.5%","97.5%")) %>%
#   mutate(doy = yday(mdy("6-20-2013") + time)) %>%
#   dplyr::filter(doy %in% surveydates$doy&scen_g == "WESA Numbers") %>%
#   mutate(rT = scenario_f-1, joiner = paste(doy, format(mround(rT,5),nsmall = 2), sep = "_")) %>%
#   filter(joiner %in% pop_pred$joiner) %>%
#   # select(rT, quant_pLarge, pLarge, doy) %>%
#   select(rT, quant_r0, r0,r1, doy) %>%
#   # group_by(doy, rT, quant_pLarge) %>%
#   group_by(doy, rT, quant_r0) %>%
#   nest %>% ungroup %>%
#   spread(quant_r0, data) %>%
#   # spread(quant_pLarge, data) %>%
#   unnest() %>%
#   left_join(surveydates)



# predictions_vs_results_pop <-
#   ggplot(ressurvey, aes(Year, Small)) +
#   geom_pointrange(position = position_nudge(x=0.01),aes(ymin=lci_r0, ymax=uci_r0), colour = 'red') +
#   geom_pointrange(position = position_nudge(x=-0.01),aes(ymin=lci_r1, ymax=uci_r1,y=Large), colour = 'blue') +
#   # geom_pointrange(position = position_nudge(x=-0.01),aes(ymin=lci_pTotal, ymax=uci_pTotal), colour = 'black') +
#   # geom_smooth(method='lm',se=T, colour = 'red') +
#   # geom_smooth(method='lm',se=T, colour = 'blue', aes(y=Large)) +
#   facet_grid(lubridate::month(as.numeric(Month), label=T)~.)  +
#   geom_ribbon(data=ci_mod, aes(x=rT, ymin=r0, ymax=r01, y=NULL),fill = 'red', alpha=0.25)+
#   geom_ribbon(data=ci_mod, aes(x=rT, ymin=r1, ymax=r11, y=NULL),fill = 'blue', alpha=0.25)+
#   # geom_ribbon(data=ci_mod, aes(x=rT, ymin=pLarge, ymax=pLarge1, y=NULL),fill = 'blue', alpha=0.25)+
#   geom_line(data = pop_pred, aes(x=rT.y, y=r0), linetype=2,colour = 'red')+
#   geom_line(data = pop_pred, aes(x=rT.y, y=r1), linetype = 2,colour = 'blue')+
#   # geom_line(data = pop_pred, aes(x=rT.y, y=pLarge), linetype = 2,colour = 'blue')+
#   labs(x="Relative WESA numbers", y='Relative change from baseline at small (Red) and large (Blue) sites')

# predictions_vs_results_pop


# predictions_vs_results_pop <-
#   ggplot(ressurvey, aes(Year, pTotal)) +
#   geom_pointrange(aes(ymin=lci_pTotal, ymax=uci_pTotal), colour = 'red') +
#   # geom_pointrange(position = position_nudge(x=-0.01),aes(ymin=lci_r1, ymax=uci_r1,y=Large), colour = 'blue') +
#   geom_smooth(method='lm',se=T, colour = 'red') +
#   # geom_smooth(method='lm',se=T, colour = 'blue', aes(y=Large)) +
#   facet_grid(lubridate::month(as.numeric(Month), label=T)~.)  +
#   geom_ribbon(data=ci_mod, aes(x=rT, ymin=pLarge, ymax=pLarge1, y=NULL),fill = 'red', alpha=0.25)+
#   # geom_ribbon(data=ci_mod, aes(x=rT, ymin=r1, ymax=r11, y=NULL),fill = 'blue', alpha=0.25)+
#   geom_line(data = pop_pred, aes(x=rT.y, y=pLarge), linetype=2,colour = 'red')+
#   # geom_line(data = pop_pred, aes(x=rT.y, y=r1), linetype = 2,colour = 'blue')+
#   labs(x="Relative WESA numbers", y='Relative change from baseline at small (Red) and large (Blue) sites')



# Predator timing ---------------------------------------------------------



# predator_pred <- dat$quants%>% filter(quant_relT=="50%") %>%
#   mutate(doy = yday(mdy("6-20-2013") + time)) %>%
#   dplyr::filter(doy %in% surveydates$doy&scen_g == "Predator Timing") %>%
#   mutate(joiner = paste(doy, -scenario_f, sep = "_")) %>%
#   right_join(ressurvey %>%
#                # bind_rows(outsideres) %>%
#                #select( Year,doy, rT) %>%
#                mutate_at(vars(Year), as.numeric) %>%
#                mutate(joiner = paste(doy, round(dev.snowmelt,0), sep = "_")), by = c("joiner", "doy")) %>%
#   group_by(doy)# %>%
#   # slice(which.min(abs(rT.x-rT.y))) %>% ungroup

# ci_mod_pred_T <-
#   dat$quants%>% filter(quant_pLarge%in%c("2.5%","97.5%")) %>%
#   mutate(doy = yday(mdy("6-20-2013") + time)) %>%
#   dplyr::filter(doy %in% surveydates$doy&scen_g == "Predator Timing") %>%
#   mutate(rT = scenario_f-1, joiner = paste(doy, -1*scenario_f, sep = "_")) %>%
#   filter(joiner %in% predator_pred$joiner) %>%
#   select(scenario_f, quant_r0, r0,r1, doy) %>% group_by(doy, scenario_f, quant_r0) %>%
#   nest %>% ungroup %>% spread(quant_r0, data) %>% unnest() %>% left_join(surveydates)
# ci_mod_pred_T <-
#   dat$quants%>% filter(quant_pLarge%in%c("2.5%","97.5%")) %>%
#   mutate(doy = yday(mdy("6-20-2013") + time)) %>%
#   dplyr::filter(doy %in% surveydates$doy&scen_g == "Predator Timing") %>%
#   mutate(rT = scenario_f-1, joiner = paste(doy, -1*scenario_f, sep = "_")) %>%
#   filter(joiner %in% predator_pred$joiner) %>%
#   select(scenario_f, quant_pLarge, pLarge, doy) %>% group_by(doy, scenario_f, quant_pLarge) %>%
#   nest %>% ungroup %>% spread(quant_pLarge, data) %>% unnest() %>% left_join(surveydates)
#


# predictions_vs_results_pred_time <-
#   ggplot(ressurvey, aes(dev.snowmelt, Small)) +
#   geom_pointrange(position = position_nudge(x=0.5),aes(ymin=lci_r0, ymax=uci_r0), colour = 'red') +
#   geom_pointrange(position = position_nudge(x=-0.5),aes(ymin=lci_r1, ymax=uci_r1,y=Large), colour = 'blue') +
#   # geom_smooth(method='lm',se=T, colour = 'red') +
#   # geom_smooth(method='lm',se=T, colour = 'blue', aes(y=Large)) +
#   facet_grid(lubridate::month(as.numeric(Month), label=T)~.)  +
#   geom_ribbon(data=ci_mod_pred_T, aes(x=-1*scenario_f, ymin=r0, ymax=r01, y=NULL),fill = 'red', alpha=0.25)+
#   geom_ribbon(data=ci_mod_pred_T, aes(x=-1*scenario_f, ymin=r1, ymax=r11, y=NULL),fill = 'blue', alpha=0.25)+
#   geom_line(data = predator_pred, aes(x=-1*scenario_f, y=r0), linetype=2,colour = 'red')+
#   geom_line(data = predator_pred, aes(x=-1*scenario_f, y=r1), linetype = 2,colour = 'blue') +
#   labs(x="Relative falcon timing", y='Relative change from baseline at small (Red) and large (Blue) sites')
# predictions_vs_results_pred_time <-
#   ggplot(ressurvey, aes(dev.snowmelt, pTotal)) +
#   geom_pointrange(aes(ymin=lci_pTotal, ymax=uci_pTotal), colour = 'black') +
#   # geom_pointrange(position = position_nudge(x=-0.01),aes(ymin=lci_r1, ymax=uci_r1,y=Large), colour = 'blue') +
#   geom_smooth(method='lm',se=T, colour = 'black') +
#   # geom_smooth(method='lm',se=T, colour = 'blue', aes(y=Large)) +
#   facet_grid(lubridate::month(as.numeric(Month), label=T)~.)  +
#   geom_ribbon(data=ci_mod_pred_T, aes(x=-1*scenario_f, ymin=pLarge, ymax=pLarge1, y=NULL),fill = 'red', alpha=0.25)+
#   # geom_ribbon(data=ci_mod, aes(x=rT, ymin=r1, ymax=r11, y=NULL),fill = 'blue', alpha=0.25)+
#   geom_line(data = predator_pred, aes(x=-1*scenario_f, y=pLarge), linetype=2,colour = 'red')+
#   # geom_line(data = pop_pred, aes(x=rT.y, y=r1), linetype = 2,colour = 'blue')+
#   labs(x="Relative falcon timing", y='Relative change from baseline at small (Red) and large (Blue) sites')

# predictions_vs_results_pred_time  +ylim(0,1)
# ppap <- cowplot::plot_grid(
# dat$means %>% filter(scen_g=="Predator Timing"&time<90) %>%
#   ggplot(aes(time-(53-scenario_f), mean_pLarge, colour = as.factor(scenario_f)))  +
#   theme(legend.position = 'none')+
#   geom_line() + geom_vline(xintercept = -20),
#
# pred_plot_notinc <-
# dat$means %>% filter(scen_g=="Predator Timing"&time<90) %>%
#   ggplot(aes(time, mean_pLarge, colour = as.factor(scenario_f)))  +
#   theme(legend.position = 'none')+
#   geom_line() , nrow=1)


# falcon numbers ----------------------------------------------------------


# falcon_pred <- dat$quants%>% filter(quant_relT=="50%") %>%
#   mutate(doy = yday(mdy("6-20-2013") + time)) %>%
#   dplyr::filter(doy %in% surveydates$doy&scen_g == "Predator Numbers") %>%
#   mutate(joiner = paste(doy, scenario_f-1, sep = "_")) %>%
#   right_join(falcons %>%
#                # bind_rows(outsideres) %>%
#                #select( Year,doy, rT) %>%
#                mutate_at(vars(Year), as.numeric) %>%
#                mutate(joiner = paste(doy, round(f_mean,1), sep = "_")), by = c("joiner", "doy")) %>%
#   group_by(doy)# %>%
# slice(which.min(abs(rT.x-rT.y))) %>% ungroup

# ci_mod_falc <-
#   dat$quants%>% filter(quant_pLarge%in%c("2.5%","97.5%")) %>%
#   mutate(doy = yday(mdy("6-20-2013") + time)) %>%
#   dplyr::filter(doy %in% surveydates$doy&scen_g == "Predator Numbers") %>%
#   mutate(joiner = paste(doy, scenario_f-1, sep = "_")) %>%
#   filter(joiner %in% falcon_pred$joiner) %>%
#   select(scenario_f, quant_r0, r0,r1, doy) %>% group_by(doy, scenario_f, quant_r0) %>%
#   nest %>% ungroup %>% spread(quant_r0, data) %>% unnest() %>% left_join(surveydates)
#


# predictions_vs_results_falcons <-
#   ggplot(falcons %>% left_join(ressurvey), aes(f_mean, Small)) +
#   geom_pointrange(position = position_nudge(x=0.005),aes(ymin=lci_r0, ymax=uci_r0), colour = 'red') +
#   geom_pointrange(position = position_nudge(x=-0.005),aes(ymin=lci_r1, ymax=uci_r1,y=Large), colour = 'blue') +
#   # geom_smooth(method='lm',se=T, colour = 'red') +
#   # geom_smooth(method='lm',se=T, colour = 'blue', aes(y=Large)) +
#   facet_grid(lubridate::month(as.numeric(Month), label=T)~.)  +
#   geom_ribbon(data=ci_mod_falc, aes(x=(scenario_f-1), ymin=r0, ymax=r01, y=NULL, group=1),fill = 'red', alpha=0.25)+
#   geom_ribbon(data=ci_mod_falc, aes(x=scenario_f-1, ymin=r1, ymax=r11, y=NULL),fill = 'blue', alpha=0.25)+
#   geom_line(data = falcon_pred, aes(x=scenario_f-1, y=r0), linetype=2,colour = 'red')+
#   geom_line(data = falcon_pred, aes(x=scenario_f-1, y=r1), linetype = 2,colour = 'blue')+
#   labs(x="Relative falcon numbers", y='Relative change from baseline at small (Red) and large (Blue) sites')

# predictions_vs_results_falcons +theme(strip.text.y = element_blank())



# cowplot::plot_grid(predictions_vs_results_pop +theme(strip.text.y = element_blank()), predictions_vs_results_pred_time+ylab("")+theme(strip.text.y = element_blank()), predictions_vs_results_falcons+ylab(""), nrow=1)

# mod_best_fit <-
# modelPredictions_all  %>%
#   filter(quant_pLarge == "50%") %>%
#   select(doy, Year, scen_g, scenario_f, pLarge, TotalBirds)  %>%
#   mutate_at(.vars = "Year", as.character) %>%
#   left_join(surveyresults) %>%
#   mutate(Month = ifelse(doy<213, "July", "August")) %>%
#   group_by(Year,Month, scen_g) %>%
#   slice(which.min(abs(pLarge-pTotal))) %>%
#   select(Year, scen_g, scenario_f, pLarge, pTotal) %>%
#   arrange(scen_g, Month, Year)

# mod_best_fit[mod_best_fit$scen_g=="Food abundance",]


lof <- list.files("../Confrontation_chapter/.dat", pattern = "[ValueResults_full, optimized]", full.names = T)

newdat <- map_df(lof[grepl("optimized", lof)], read_tsv)

dat <- map_df(lof[!grepl("arrival", lof) & grepl("ValueResults_full", lof) & !grepl("alt", lof)], read_tsv,
  col_names = c("var", "date", "optVar", "optN", "ncycles_pLarge", "ncycles_N", "pLarge", "totalBirds")
) %>%
  filter(!var %in% c("arrival", "mass")) %>%
  bind_rows(newdat %>% rename(optVar = optvar, ncycles_pLarge = p, ncycles_N = k)) %>%
  bind_rows(read_tsv("../Confrontation_chapter/.dat/ValueResults_full_mass_arrival.txt",
    col_names = c("var", "date", "optVar", "optN", "ncycles_pLarge", "ncycles_N", "pLarge", "totalBirds")
  )) %>%
  left_join(surveydates %>% mutate(date = doy - yday(mdy("6-20-2013")))) %>%
  arrange(var, Month, Year) %>%
  rowid_to_column("id")

Scenarionames <- tibble(
  var = sort(unique(dat$var)),
  Scenario = c(
    "WESA Timing",
    "Predator Timing",
    "Food abundance",
    "Predator Numbers",
    "Mass", "WESA Numbers"
  )
)

dat %<>% left_join(Scenarionames, by = "var")

# ggplot(dat, aes(pLarge, ncycles_pLarge)) + facet_wrap(~var) + geom_point()

# zz=gzfile('../Confrontation_chapter/.dat/optvar_w_avg_avg.tar.gz','rt')
# dat_w_avg_avg <-  map_df( list.files("../Confrontation_chapter/.dat/wavgavg/",pattern = "optimized", full.names = T),read_tsv,
#                           col_types=cols(), skip=1,
#                           col_names = c("var", "date", "optVar", "optN", "ncycles_pLarge", "ncycles_N", "pLarge", "totalBirds", "run")) %>%
#   left_join(surveydates %>% mutate(date = doy - yday(mdy("6-20-2013")))) %>%
#   arrange(var, Month, Year) %>% rowid_to_column("id") %>%  left_join(Scenarionames, by='var')
# dat <- dat_w_avg_avg

dat_w_skippers <- map_df(list.files("../Confrontation_chapter/.dat/res_w_skippers/", pattern = "optimized", full.names = T), read_tsv,
  col_types = cols(), skip = 1,
  col_names = c(
    "var", "date", "optVar", "optN",
    "ncycles_pLarge", "ncycles_N",
    "pLarge", "totalBirds", "skippers", "run"
  )
) %>%
  left_join(surveydates %>% mutate(date = doy - yday(mdy("6-20-2013")))) %>%
  arrange(var, Month, Year) %>%
  rowid_to_column("id") %>%
  left_join(Scenarionames, by = "var") %>%
  mutate(optN_all = optN, optN = optN - skippers)

dat_nopk <- map_df(list.files("../Confrontation_chapter/.dat/nopk/", pattern = "optimized", full.names = T), read_tsv,
  col_types = cols(), skip = 1,
  col_names = c(
    "var", "date", "optVar", "optN",
    "ncycles_pLarge", "ncycles_N",
    "pLarge", "totalBirds", "skippers", "run"
  )
) %>%
  left_join(surveydates %>% mutate(date = doy - yday(mdy("6-20-2013")))) %>%
  arrange(var, Month, Year) %>%
  rowid_to_column("id") %>%
  left_join(Scenarionames, by = "var") %>%
  mutate(optN_all = optN, optN = optN - skippers)
# dat <- dat_nopk

ucidat <- map_df(list.files("../Confrontation_chapter/.dat/uci", pattern = ".txt", full.names = T), read_tsv,
  col_types = cols(), skip = 1,
  col_names = c(
    "var", "date", "optVar", "optN",
    "ncycles_pLarge", "ncycles_N",
    "pLarge", "totalBirds", "skippers", "run"
  )
) %>%
  left_join(surveydates %>% mutate(date = doy - yday(mdy("6-20-2013")))) %>%
  arrange(var, Month, Year) %>%
  rowid_to_column("id") %>%
  left_join(Scenarionames, by = "var") %>%
  mutate(optN_all = optN, optN = optN - skippers)

lcidat <- map_df(list.files("../Confrontation_chapter/.dat/lci", pattern = ".txt", full.names = T), read_tsv,
  col_types = cols(), skip = 1,
  col_names = c(
    "var", "date", "optVar", "optN",
    "ncycles_pLarge", "ncycles_N",
    "pLarge", "totalBirds", "skippers", "run"
  )
) %>%
  left_join(surveydates %>% mutate(date = doy - yday(mdy("6-20-2013")))) %>%
  arrange(var, Month, Year) %>%
  rowid_to_column("id") %>%
  left_join(Scenarionames, by = "var") %>%
  mutate(optN_all = optN, optN = optN - skippers)

folders <- c("lci", "uci", "res_w_skippers",'food')

importdatfingerprints <- function(f)
{
  out_data <- map_df(list.files(paste("../Confrontation_chapter/.dat/",f,sep = ""), pattern = "*.txt", full.names = T), read_tsv,
                           col_types = cols(), skip = 1,
                           col_names = c(
                             "var", "date", "optVar", "optN",
                             "ncycles_pLarge", "ncycles_N",
                             "pLarge", "totalBirds", "skippers", "run"
                           )
  ) %>% mutate(type = f) #%>% 
    # left_join(surveydates %>% mutate(date = doy - yday(mdy("6-20-2013")))) %>%
    # arrange(var, Month, Year) %>%
    # rowid_to_column("id") %>%
    # left_join(Scenarionames, by = "var") %>%
    # mutate(optN_all = optN, optN = optN - skippers)
  return(out_data)
}


finaldat <- map_df(folders, importdatfingerprints) %>% 
  group_by(var, date,type) %>% 
  summarize_at(.vars = vars(optN, optVar, pLarge, totalBirds,  skippers), mean) %>% 
  tidyr::unite("vars",optN, optVar, pLarge, totalBirds,  skippers) %>%
  group_by(var, date) %>% select(type,vars) %>% 
  spread(key = type, value = vars) %>% 
  separate(lci, 
           into = c("lci_optN", "lci_optVar", "lci_pLarge", "lci_totalBirds",  "lci_skippers"),
           convert = T,
            sep = "_") %>%
  separate(uci, 
           into = c("uci_optN", "uci_optVar", "uci_pLarge", "uci_totalBirds",  "uci_skippers"),
           convert = T,
            sep = "_") %>%
  separate(res_w_skippers, 
           into = c("optN", "optVar", "pLarge", "totalBirds",  "skippers"),
           convert = T,
            sep = "_") %>%
  separate(food, 
           into = c("f_optN", "f_optVar", "f_pLarge", "f_totalBirds",  "f_skippers"),
           convert = T,
           sep = "_") %>% 
  left_join(surveydates %>% mutate(date = doy - yday(mdy("6-20-2013")))) %>%
  arrange(var, Month, Year) %>%
  rowid_to_column("id") %>%
  left_join(Scenarionames, by = "var") %>%
  mutate(optN_all = optN, optN = optN - skippers)
  
food_dat <- importdatfingerprints("food")



dat <- dat_w_skippers
dat <- finaldat

# pdf("ModelPredictions_optimized2.pdf",paper = "US", height = 20, width = 8)
# Pop_Size_Estimates <-
#   ggplot(dat %>% filter(!var %in% c("arrival", "mass")), aes(Year, optN, colour = month(Date, label = T))) +
#   facet_wrap(~Scenario) +
#   geom_point(position = position_dodge(0.2), aes(shape = Month.name)) +
#   scale_color_brewer(type = "qual", palette = "Set1", direction = -1) +
#   stat_summary(fun.y = "mean", geom = "line", aes(linetype = Month.name)) +
#   # geom_smooth(method = 'glm', method.args=list(family=gaussian(link='log')), aes(linetype=Month.name)) +
#   labs(colour = "Month", shape = "", linetype = "") +
#   theme(legend.position = "none") +
#   stat_summary(data = ucidat, fun.y = "mean", geom = "line", aes(linetype = Month.name), alpha = 0.5) +
#   stat_summary(data = lcidat, fun.y = "mean", geom = "line", aes(linetype = Month.name), alpha = 0.5) +
#   labs(y = "Model predicted number of birds stopping in region", x = "")

Pop_Size_Estimates <-
  ggplot(dat_w_skippers %>% filter(!var %in% c("arrival", "mass")), 
         aes(Year, optN, colour = month(Date, label = T))) +
  facet_wrap(~Scenario) +
  geom_point(position = position_dodge(0.2), aes(shape = Month.name), alpha = 0.2) +
  scale_color_brewer(type = "qual", palette = "Set1", direction = -1) +
  # geom_line(data = dat,
  #           alpha = 0.75,  linetype = "dotted",
  #           aes(y = lci_optN,
  #               group = month(Date, label = T))) + 
  # geom_line(data = dat,
  #           alpha = 0.75,   linetype = "dotted",
  #           aes(y = uci_optN,
  #               group = month(Date, label = T))) +
  stat_summary(fun.y = "mean", geom = "line", aes(linetype = Month.name)) +
  # geom_smooth(method = 'glm', method.args=list(family=gaussian(link='log')), aes(linetype=Month.name)) +
  labs(colour = "Month", shape = "", linetype = "") +
  theme(legend.position = "none") +
  labs(y = "Model predicted number of birds stopping in region", x = "") +
  scale_y_log10()


skippers_plot <-
  ggplot(dat %>% filter(!var %in% c("arrival", "mass")), aes(Year, skippers / optN_all * 100, colour = month(Date, label = T))) +
  facet_wrap(~Scenario) +
  geom_point(position = position_dodge(0.2), aes(shape = Month.name)) +
  scale_color_brewer(type = "qual", palette = "Set1", direction = -1) +
  stat_summary(fun.y = "mean", geom = "line", aes(linetype = Month.name)) +
  # geom_smooth(method = 'glm', method.args=list(family=gaussian(link='log')), aes(linetype=Month.name)) +
  labs(colour = "Month", shape = "", linetype = "") +
  theme(legend.position = "none") + ylim(0, 100) +
  labs(y = "Model predicted percentage of birds skipping region", x = "")

# resultplots$TotalBirds + geom_point(data=dat %>% filter(!var %in% c("arrival", "mass")),
#                                     position = position_dodge(0.2),
#                                     aes(y=totalBirds, colour = Scenario)) +
#   geom_smooth(method = 'glm', method.args=list(family=gaussian(link='log')), aes(y=totalBirds))+
#   scale_color_brewer(type='qual', palette = 'Set1', direction=-1)



# ggplot(dat , aes(Year, optVar, colour = month(Date, label = T))) + facet_wrap(~Scenario, scales='free_y') +
#   geom_point(position = position_dodge(0.2)) +
#   stat_summary(fun.y='mean', geom='line') +
#   scale_color_brewer(type='qual', palette = 'Set1', direction=-1)
#
#
# ggplot(dat, aes(Year, pLarge, colour = month(Date, label = T))) + facet_wrap(~Scenario) + geom_point() +
#   scale_color_brewer(type='qual', palette = 'Set1', direction=-1) +ylim(0,1)



# resultplots <- predictandplot(ressurvey, NA, 'plot', T,sepPlots = T)
# resultplots$pLarge + geom_point(data=dat %>% filter(!var %in% c("arrival", "mass")), aes(y=pLarge, colour = Scenario)) +
#   scale_color_brewer(type='qual', palette = 'Set1', direction=1)
# resultplots$TotalBirds + geom_point(data=dat %>% filter(!var %in% c("arrival", "mass")),
#                                     position = position_dodge(0.2),
#                                     aes(y=totalBirds, colour = Scenario)) +
#   geom_smooth(method = 'glm', method.args=list(family=gaussian(link='log')), aes(y=totalBirds))
joined_res <-
  dat_w_skippers %>%
  filter(!var %in% c("arrival", "mass")) %>%
  left_join(
    ressurvey %>% mutate(Month = as.numeric(Month)),
    c("Year", "Month", "doy")
  )

jrnoN <- joined_res %>%
  filter(var != "popsize") %>%
  mutate(cor = cor(pTotal, pLarge, method = "spearman")) %>%
  group_by(Month.name.x, cor) %>%
  summarize(cor.month = cor(pTotal, pLarge, method = "spearman")) %>%
  ungroup()


jrnoScen <- joined_res %>% # filter(var!="popsize") %>%
  # mutate(cor = cor(pTotal, pLarge, method = 'spearman')) %>%
  mutate(
    diff = pTotal - pLarge,
    ss = diff**2
  ) %>%
  group_by(Year, Month.name.x, Scenario) %>%
  mutate(n_runs = n()) %>%
  group_by(Month.name.x, Scenario) %>%
  summarize(
    cor.month = cor(pTotal, pLarge, method = "spearman"),
    mean_diff = mean(diff),
    sd_dff = sd(diff),
    sse = sum(ss), n = n(),
    nruns = mean(n_runs),
    mean_cycles_pLarge = mean(ncycles_pLarge),
    mean_cycles_N = mean(ncycles_N)
  ) %>%
  ungroup()

# jrnoScen %>% unite("var", mean_diff, sd_dff, cor.month, sep="_") %>%
#   spread(Month.name.x, var) %>%
#   separate(Jul, c("Mean Difference\nAdults","SD Difference\nAdults", "Correlation\nAdults"),sep = "_") %>%
#   separate(Aug, c("Mean Difference\nJuveniles","SD Difference\nJuveniles", "Correlation\nJuveniles"),sep = "_") %>%
#   select(1,2,5,3,6,4,7) %>% xtable::xtable()


plarge_fit <-
  ggplot(
    joined_res %>% filter(!var %in% c("arrival", "mass")),
    aes(pTotal, pLarge, colour = Scenario)
  ) +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_brewer(type = "qual", palette = "Set1", direction = 1) + # ylim(0,1)+xlim(0,1) +
  labs(x = "Survey Data (proportion of birds at large sites)", y = "Model Data (proportion of birds at large sites)") +
  # geom_smooth(method='lm', linetype=2,se=F, alpha=0.25) +
  geom_point(position = position_dodge(0.001), aes(shape = Scenario), size = 2)
# stat_summary(fun.data='mean_cl_boot',position = position_dodge(0.001), aes(shape = Scenario))
# ggplot(joined_res,aes(totalBirds.x, totalBirds.x-totalBirds.y, colour =Scenario )) + geom_point() +
#   geom_smooth(method='lm')


f_numbers_compare <-
  ggplot(dat_w_skippers %>% filter(var == "flywayPredation"), aes(Year, optVar, colour = Month.name)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "ribbon", colour = "grey", alpha = 1, aes(group = Month.name)) +
  stat_summary(fun.y = "mean", geom = "line") + 
  # geom_line(data = dat %>% filter(var == "flywayPredation"), 
  #           alpha = 0.75,  linetype = "dotted",
  #           aes(y = lci_optVar,
  #               group = month(Date, label = T))) + 
  # geom_line(data = dat %>% filter(var == "flywayPredation"),
  #           alpha = 0.75,   linetype = "dotted",
  #           aes(y = uci_optVar,
  #               group = month(Date, label = T))) +
  facet_wrap(~Month.name) +
  geom_line(data = falcons, aes(x = Year, y = f_mean + 1), linetype = "longdash") +
  geom_hline(yintercept = 1.0, linetype = 1, colour = "grey") +
  scale_color_brewer(type = "qual", palette = "Set1", direction = -1) +
  labs(
    y = "Annual deviation in falcon numbers from\nmodel (solid lines) or survey obvservations (dashed line)\n (Below one is low, above one is high)",
    colour = ""
  ) + theme(legend.position = "bottom")
# geom_line(data=falcons, aes(x=as.numeric(Year),y=f_sum), linetype=2)

f_arrival_compare <-
  ggplot(dat_w_skippers %>% filter(var == "f_arrival_mod"), aes(Year, optVar, colour = Month.name)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "ribbon", colour = "grey", alpha = 0.25, aes(group = Month.name)) +
  geom_line(
    data = dev.snowmelt_bysite %>% filter(Year > 2012),
    aes(x = as.numeric(Year), y = -1 * dev.snowmelt, group = STATION_NAME), linetype = "longdash", colour = "grey"
  ) +

  # geom_line(data = dat %>% filter(var == "f_arrival_mod"), 
  #           alpha = 0.75,  linetype = "dotted",
  #             aes(y = lci_optVar,
  #                 group = month(Date, label = T))) + 
  # geom_line(data = dat %>% filter(var == "f_arrival_mod"),
  #           alpha = 0.75,   linetype = "dotted",
  #             aes(y = uci_optVar,
  #                 group = month(Date, label = T))) +
  # geom_line(data = dat %>% filter(var == "f_arrival_mod"),
  #           alpha = 1, size=2,
  #           aes(y = f_optVar,
  #               group = month(Date, label = T))) +
  # geom_line(data = dat %>% filter(var == "f_arrival_mod")) + 
  scale_color_brewer(type = "qual", palette = "Set1", direction = -1) +
  geom_line(
    data = mean.snowmelt %>% filter(Year > 2012),
    aes(x = as.numeric(Year), y = -1 * dev.snowmelt), colour = "black", linetype ='longdash', 
  ) +
  labs(
    y = "Deviation in falcon arrival from\nmodel (solid lines) or snowmelt date (dashed line)\n (Negative is later arrival, positive is earlier arrival)",
    colour = ""
  ) + theme(legend.position = "bottom")

food_estimates_plot_confront <- 
  ggplot(dat_w_skippers %>% filter(var == "flywayFood"), 
                    aes(Year, optVar, colour = Month.name)) +
  # geom_line(data = dat %>% filter(var == "flywayFood"), 
  #           alpha = 0.75,  linetype = "dotted",
  #           aes(y = lci_optVar,
  #               group = month(Date, label = T))) + 
  # geom_line(data = dat %>% filter(var == "flywayFood"),
  #           alpha = 0.75,   linetype = "dotted",
  #           aes(y = uci_optVar,
  #               group = month(Date, label = T))) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "ribbon", colour = "grey", alpha = 1, aes(group = Month.name)) +
  stat_summary(fun.y = "mean", geom = "line") + # facet_wrap(~Month.name)+
  # stat_summary(data = ucidat %>% filter(var == "flywayFood"), fun.y = "mean", geom = "line", alpha = 0.5) +
  # stat_summary(data = lcidat %>% filter(var == "flywayFood"), fun.y = "mean", geom = "line", alpha = 0.5) +

  # geom_line(data=falcons, aes(x=Year,y=f_mean+1), linetype=3) +
  geom_hline(yintercept = 0.5, linetype = 'longdash') +
  scale_color_brewer(type = "qual", palette = "Set1", direction = -1) +
  labs(
    y = expression(paste("Model estimated minimum local daily refuelling rate (g ", day^-1, ")")),
    colour = ""
  ) + theme(legend.position = "bottom") + 
  coord_cartesian(ylim = c(0.35, 0.62))



plarge_N <-
  ggplot(
    joined_res %>% filter(!var %in% c("arrival", "mass")),
    aes(optN, pTotal, colour = Scenario)
  ) +
  facet_wrap(~Month) +
  scale_color_brewer(type = "qual", palette = "Set1", direction = 1) + # ylim(0,1)+xlim(0,1) +
  # labs(x="Survey Data (proportion of birds at large sites)", y="Model Data (proportion of birds at large sites)")  +
  # geom_smooth(method='lm', linetype=2,se=F, alpha=0.25) +
  # geom_point(position = position_dodge(0.001), aes(shape = Scenario), size=2)
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(0.001), aes(shape = Scenario, group = interaction(Month, Scenario, Year)))
# ggplot(joined_res,aes(totalBirds.x, totalBirds.x-totalBirds.y, colour =Scenario )) + geom_point() +
#   geom_smooth(method='lm')


# f_arrival_cor <- dat %>% filter(var=="f_arrival_mod") %>%
#   filter(Year!=2014 | Month !=7) %>%
#   group_by(Month.name) %>% summarize(cor= cor(optVar, -1*dev.snowmelt, method = 'spearman'))
#
# f_cor <- dat %>% filter(var=="flywayPredation") %>%
#   # filter(Year!=2014 | Month !=7) %>%
#   select(Year, optVar, Month.name) %>%
#   left_join(falcons) %>%
#   group_by(Month.name) %>% summarize(cor= cor(optVar, f_mean+1,method = 'spearman'))

# dev.off()


# dat %>% filter(var=="f_arrival_mod") %>%
#   ggplot( aes(-1*dev.snowmelt, optVar, colour=Month.name)) +
#   geom_jitter(width = 0.2, height=0, alpha=0.5)+
#   stat_summary(fun.data = 'mean_cl_boot', geom='ribbon',colour = 'grey',alpha=0.25,aes(group=Month.name) ) +
#   # geom_line(data=dev.snowmelt_bysite %>% filter(Year>2012),
#   #           aes(x=as.numeric(Year),y=-1*dev.snowmelt, group=STATION_NAME), linetype='dashed', size=1, colour = 'grey')   +
#   # geom_ribbon(data=mean.snowmelt %>% filter(Year>2012),
#   #             aes(x=as.numeric(Year),y=NULL,
#   #                 ymin=-1*dev.snowmelt+sd.snowmelt,
#   #                 max=-1*dev.snowmelt-sd.snowmelt),
#   #             colour = 'grey',alpha=0.25,
#   #             linetype=3)  +
#     stat_summary(fun.y='mean', geom='line') +
#   scale_color_brewer(type='qual', palette = 'Set1', direction=-1)+
#   # geom_line(data=mean.snowmelt %>% filter(Year>2012),
#   #           aes(x=as.numeric(Year),y=-1*dev.snowmelt),colour = 'black', linetype=2, size=1.1)   +
#   labs(y="Deviation in falcon arrival from\nmodel (solid lines) or snowmelt date (dashed line)\n (Negative is later arrival, positive is earlier arrival)",
#        colour="") + theme(legend.position = 'bottom')





dat %>%
  group_by(Scenario, Year, Month) %>%
  summarize(optVar = mean(optVar), optN = mean(optN)) %>%
  filter(Scenario == "Predator Timing") %>%
  arrange(Month) %>%
  group_by(Month) %>%
  summarize(mn = mean(optVar), sd = sd(optVar), mn2 = mean(optN), sd2 = sd(optN))


dat %>%
  group_by(Scenario, Year, Month) %>%
  summarize(optVar = mean(optVar), optN = mean(optN)) %>%
  group_by(Scenario, Year) %>%
  mutate(dif = diff(optVar)) %>%
  filter(Scenario == "Food abundance") %>%
  arrange(Month) %>%
  group_by(Month) %>%
  summarize(mn = mean(optVar), sd = sd(optVar), mn2 = mean(optN), sd2 = sd(optN), dif = mean(dif))
