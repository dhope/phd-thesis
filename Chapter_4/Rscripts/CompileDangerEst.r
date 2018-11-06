require(tidyverse)
require(purrr)
require(readr)

temp_guides_MC <- list.files(path = "./DangerEstimatesUpdated", pattern = "polygonEst", full.names = T)

Danger_dat_files <- map_df(temp_guides_MC, read_csv, col_names = T) %>% mutate(ID = paste0(Locality, Lat, Lon, sep = "_"))

saveRDS(Danger_dat_files, "./DangerEstimatesUpdated/Danger_Estimates_all.rds")
