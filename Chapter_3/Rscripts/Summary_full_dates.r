#  Full Dates
source('Summary_of_Scenarios.r')
baseline_all_list_pk <- map(distances, extract_baseline, pk_=0, pulltimes=full_pulltimes)
baseline_all_list_nopk <- map(distances, extract_baseline, pk_=1, pulltimes=full_pulltimes)

names(baseline_all_list_pk) <- distances
names(baseline_all_list_nopk) <- distances


# # test <- summarize_model_dat(5000, baselist = baseline_all_list_nopk, pk = 1, pulltimes=full_pulltimes)
alloutpk <- map_df(distances, summarize_model_dat, baselist = baseline_all_list_pk, pk = 0, pulltimes=full_pulltimes)
write_rds(alloutpk, "../Confrontation_chapter/.rds/Scenarios_summary_FullDates_withDistance_pk2.rds")
alloutnopk <- map_df(distances, summarize_model_dat, baselist = baseline_all_list_nopk, pk = 1, pulltimes=full_pulltimes)
write_rds(alloutnopk, "../Confrontation_chapter/.rds/Scenarios_summary_FullDates_withDistance_nopk2.rds")
