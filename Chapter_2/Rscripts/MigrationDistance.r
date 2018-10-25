# MC Distance Plot

peak_data <- map_df(
  list.files("../cpp_version/Migration_Model/OrganizedResults/noU/distancemod/mc/", 
             full.names = T), 
  read_tsv) %>% filter(time %in% c(25,56)) %>% 
  mutate(danger = dist, food = 1)
source('../Rscripts/model_data_import.r')
peak_sum <- dataimport(input_data = peak_data, dat_type = 'loop', 'nl')

pk_dist_plot <- plotProp(peak_sum %>% filter(pk==0), var=1) +
  xlab("Migratory Distance from Salish Sea (km)")
# pk_dist_plot

nopk_dist_plot <- plotProp(peak_sum %>% filter(pk==1), var=1)+
  xlab("Migratory Distance from Salish Sea (km)")
# nopk_dist_plot
