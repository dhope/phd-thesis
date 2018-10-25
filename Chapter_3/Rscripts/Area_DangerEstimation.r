## Calculating Area and Danger for each site and survey location from shape files
## May 2018
## David Hope
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)
require(tidyverse)
utm.proj <- paste0("+proj=utm +zone=", 10,  " +north +ellps=GRS80 +units=m +no_defs ", sep ="")
Area_polygons <- readOGR(dsn = "../AreaCalculations/shp/", layer = "SiteArea",stringsAsFactors = "F") %>% 
  spTransform(., CRS(utm.proj)) 

Danger_polygons <- readOGR(dsn = "../AreaCalculations/shp/", layer = "SiteDanger",stringsAsFactors = "F") %>% 
  spTransform(., CRS(utm.proj)) 

Area_polygons@data$area_m2 <- area(Area_polygons)
Area_polygons@data$area_km2 <- Area_polygons@data$area_m2 / (1000^2)
Area_polygons@data$SiteID <- Area_polygons@data$SitePoly_4


Danger_polygons@data$danger_area_m2 <- area(Danger_polygons)
Danger_polygons@data$danger_area_km2 <- Danger_polygons@data$danger_area_m2 / (1000^2)
Danger_polygons@data$SiteID <- Danger_polygons@data$SitePoly_4


Larger_Site_Area <- dplyr::select(Area_polygons@data, SitePolygo, SiteID, area_m2, area_km2) %>%
  left_join(dplyr::select(Danger_polygons@data, SitePolygo, SiteID, danger_area_m2, danger_area_km2)) %>% 
  filter(!is.na(SiteID)) %>% dplyr::select(-SitePolygo) %>% 
  group_by(SiteID) %>% 
    summarise_all('sum') %>% ungroup %>% 
  mutate(prop.danger = danger_area_m2/area_m2,
         rel.dang = prop.danger/min(prop.danger),
         size=ifelse(area_km2>10, "Large", "Small"),
         size_3 = ifelse(prop.danger < 0.2, "Large", ifelse(prop.danger>0.75, "Small","Medium")))

write_rds(Larger_Site_Area, "../AreaCalculations/Updated_Danger_largerarea.rds")

ggplot(Larger_Site_Area, aes(prop.danger, fill=area_km2)) + 
  geom_histogram(binwidth = 0.05) + scale_fill_brewer(type="seq")

Larger_Site_Area %>% arrange(prop.danger) %>%  dplyr::select(SiteID, prop.danger)

Larger_Site_Area %>% arrange(desc(rel.dang)) %>% dplyr::select(SiteID, rel.dang)


ggplot(Larger_Site_Area, aes(rel.dang, fill=as.factor(round(area_km2,0)))) + 
  geom_histogram(binwidth = 0.5) + scale_fill_brewer(palette = "YlOrRd")


Larger_Site_Area %>% group_by(size) %>% 
  summarize(d = mean(prop.danger), 
            sd_d = sd(prop.danger),
            rd = mean(rel.dang), sd_rd = sd(rel.dang),
         safe_a= mean(area_km2), sd_a=sd(area_km2))

Larger_Site_Area %>% group_by(size_3) %>% 
  summarize(d = mean(prop.danger), 
            sd_d = sd(prop.danger),
            rd = mean(rel.dang), sd_rd = sd(rel.dang),
            safe_a= mean(area_km2), sd_a=sd(area_km2))

