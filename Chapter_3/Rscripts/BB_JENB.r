# Time counts at boundary bay
# bb <- 
#   wesa %>%
#   filter(grepl(pattern = '^BB', SiteID )) %>%
#   select(ID, RecordID, CountNum,TimeEst, SiteID, counts.cleaned) %>%
#   mutate(SiteID = substr(RecordID, 1, 4), 
#          Year = substr(RecordID, 5, 8), 
#          Month = substr(RecordID, 9,9),
#          Day = substr(RecordID, 10, 11),
#          datecode = paste(Year, Month, Day, sep = "")) %>%
#   arrange(datecode, CountNum, SiteID) %>% group_by(datecode) %>% 
#   mutate(tot = sum(counts.cleaned)) %>% ungroup %>% group_by(datecode, CountNum) %>% 
#   mutate(counttot = sum(counts.cleaned), n=n()) %>% ungroup
# 
# 
# nsitesbydate <- bb %>% select(SiteID, datecode,tot) %>% distinct %>% group_by(datecode, tot) %>% summarize(n=n()) %>% 
#   arrange(desc(n)) %>% ungroup %>% filter(tot>0)
# 
# bb_combined <- bb %>% group_by(datecode, TimeEst) %>% 
#   summarize(sum.count = sum(counts.cleaned), n.survey.sites = n() ) %>% 
#  mutate(SiteID = "BBAY", RecordID = paste(SiteID, datecode, sep=""),
#              Year = substr(RecordID, 5, 8), 
#              Month = substr(RecordID, 9,9),
#              Day = substr(RecordID, 10, 11) )
# 
# 
# 
# bb_combined_date <- bb_combined %>%
#   group_by(datecode) %>% 
#   summarize(avgcount = mean(sum.count), 
#             n.survey.sites = mean(n.survey.sites), 
#             maxcount = max(sum.count))
# 
# 
# # bb_combined_date %>% group_by(Year, Month) %>% 
# #   summarize(avg.avg = mean(avgcount),
# #             avg.max = mean(maxcount),
# #             w.avg.avg = weighted.mean(avgcount, n.survey.sites),
# #             w.avg.max = weighted.mean(maxcount, n.survey.sites)) %>% 
# #   gather("type", "est", -Year, -Month) %>% ggplot(aes(as.numeric(Year), est, colour = type)) + facet_wrap(~Month) + geom_line()
# 
# 
# Jensens.Bay <- 
#   wesa %>%
#   filter(grepl(pattern = 'JEN', RecordID )) %>%
#   select(ID, RecordID, CountNum,TimeEst, SiteID, counts.cleaned) %>%
#   mutate(SiteID = substr(RecordID, 1, 4), 
#          Year = substr(RecordID, 5, 8), 
#          Month = substr(RecordID, 9,9),
#          Day = substr(RecordID, 10, 11),
#          datecode = paste(Year, Month, Day, sep = "")) %>%
#   arrange(datecode, CountNum, SiteID) %>% group_by(datecode) %>% 
#   mutate(tot = sum(counts.cleaned)) %>% ungroup %>% group_by(datecode, CountNum) %>% 
#   mutate(counttot = sum(counts.cleaned), n=n()) %>% ungroup
# 
# nsitesbydate_JJ <- Jensens.Bay %>% 
#   select(SiteID, datecode,tot) %>% 
#   distinct %>% group_by(datecode, tot) %>% summarize(n=n()) %>% 
#   arrange(desc(n)) %>% ungroup %>% filter(tot>0)
# 
# 
# 
# # alldates <- 
# # bb %>% 
# #   dplyr::filter(datecode %in% nsitesbydate$datecode[nsitesbydate$n>1]&tot>0) %>% 
# # ggplot( aes(TimeEst, counts.cleaned,colour = SiteID)) + geom_line() + facet_wrap(~datecode, scales='free') +
# #   scale_color_brewer(type='qual', palette = "Set1")
# # colors <- ggplot_build(alldates)$data[[1]]$colour %>% unique
# # names(colors) <- unique(bb$SiteID)
# 
# # 
# # 
# # pdf("BoundaryBay_RBBP.pdf")
# # for(d_i in nsitesbydate$datecode[nsitesbydate$n>1]){
# #   p <- bb %>% 
# #     dplyr::filter(datecode ==d_i) %>% 
# #     ggplot( aes(CountNum, counts.cleaned,colour = SiteID)) + geom_line() + facet_wrap(~datecode, scales='free_y') +
# #     scale_color_manual(values=colors) +
# #     stat_summary(fun.y='mean', aes(y=counttot),geom='line', linetype=2, colour = 'black')
# #   print(p)
# #   
# # }
# 
# # dev.off()
# 
# # 
# # pdf("JENB.pdf")
# # for(d_i in nsitesbydate_JJ$datecode[nsitesbydate_JJ$n>1]){
# #   p <- Jensens.Bay %>% 
# #     dplyr::filter(datecode ==d_i) %>% 
# #     ggplot( aes(CountNum, counts.cleaned,colour = SiteID)) + geom_line() + facet_wrap(~datecode, scales='free_y') +
# #     scale_color_brewer(type='qual', palette = "Set1") +
# #     stat_summary(fun.y='mean', aes(y=counttot),geom='line', linetype=2, colour = 'black')
# #   print(p)
# #   
# # }
# # 
# # dev.off()
# 
# 
# JEN_combined <- Jensens.Bay %>% group_by(datecode, TimeEst) %>% 
#   summarize(sum.count = sum(counts.cleaned), n.survey.sites = n() ) %>% 
#   mutate(SiteID = "JENB", RecordID = paste(SiteID, datecode, sep=""),
#          Year = substr(RecordID, 5, 8), 
#          Month = substr(RecordID, 9,9),
#          Day = substr(RecordID, 10, 11) )
# JEN_combined_date <- JEN_combined %>%
#   group_by(datecode) %>% 
#   summarize(avgcount = mean(sum.count), 
#             n.survey.sites = mean(n.survey.sites), 
#             maxcount = max(sum.count))
# 
# JEN_month <- 
# JEN_combined_date %>% mutate(SiteID = "JENB", RecordID = paste(SiteID, datecode, sep=""),
#                             Year = substr(RecordID, 5, 8), 
#                             Month = substr(RecordID, 9,9),
#                             Day = substr(RecordID, 10, 11)
# ) %>% group_by(Year, Month) %>% 
#   summarize(avg.avg = mean(avgcount),
#             avg.max = mean(maxcount),
#             w.avg.avg = weighted.mean(avgcount, n.survey.sites),
#             w.avg.max = weighted.mean(maxcount, n.survey.sites)) 
# 
# # JEN_month%>% 
# #   gather("type", "est", -Year, -Month) %>% ggplot(aes(as.numeric(Year), est, colour = type)) + facet_wrap(~Month) + geom_line()
# 
# 
# largesites <- bind_rows(JEN_combined, bb_combined) %>% rename(counts.cleaned= sum.count)
# 
# 

# Create function ---------------------------------------------------------

joinlargesites <- function(x){
  bb_combined <-  x %>%
    filter(grepl(pattern = '^BB', SiteID )) %>%
    select(ID, RecordID, CountNum,TimeEst, SiteID, counts.cleaned) %>%
    mutate(SiteID = substr(RecordID, 1, 4), 
           Year = substr(RecordID, 5, 8), 
           Month = substr(RecordID, 9,9),
           Day = substr(RecordID, 10, 11),
           datecode = paste(Year, Month, Day, sep = "")) %>%
    arrange(datecode, CountNum, SiteID) %>% group_by(datecode) %>% 
    mutate(tot = sum(counts.cleaned)) %>% ungroup %>% group_by(datecode, CountNum) %>% 
    mutate(counttot = sum(counts.cleaned), n=n()) %>% ungroup %>% group_by(datecode, TimeEst) %>% 
    summarize(sum.count = sum(counts.cleaned), n.survey.sites = n() ) %>% 
    mutate(SiteID = "BBAY", RecordID = paste(SiteID, datecode, sep=""),
           Year = substr(RecordID, 5, 8), 
           Month = substr(RecordID, 9,9),
           Day = substr(RecordID, 10, 11) )
  
  JEN_combined <- x %>%
    filter(grepl(pattern = 'JEN', RecordID )) %>%
    select(ID, RecordID, CountNum,TimeEst, SiteID, counts.cleaned) %>%
    mutate(SiteID = substr(RecordID, 1, 4), 
           Year = substr(RecordID, 5, 8), 
           Month = substr(RecordID, 9,9),
           Day = substr(RecordID, 10, 11),
           datecode = paste(Year, Month, Day, sep = "")) %>%
    arrange(datecode, CountNum, SiteID) %>% group_by(datecode) %>% 
    mutate(tot = sum(counts.cleaned)) %>% ungroup %>% group_by(datecode, CountNum) %>% 
    mutate(counttot = sum(counts.cleaned), n=n()) %>% ungroup %>% group_by(datecode, TimeEst) %>% 
    summarize(sum.count = sum(counts.cleaned), n.survey.sites = n() ) %>% 
    mutate(SiteID = "JENB", RecordID = paste(SiteID, datecode, sep=""),
           Year = substr(RecordID, 5, 8), 
           Month = substr(RecordID, 9,9),
           Day = substr(RecordID, 10, 11) )
  largesites <- bind_rows(JEN_combined, bb_combined) %>% rename(counts.cleaned= sum.count)
  return(largesites)
}

