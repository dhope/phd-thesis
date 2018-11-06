utm <- 20
# rm(list=ls())
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)
require(tidyverse)
setwd("~/Documents/SFU/PhD/SharingFiles_ForWindows/EasternAnalysis/Rscripts")
source("prepareNewSiteData.r")
# line.layer <- readOGR("../GIS/",layer = "Tideline")
points.layer <- dat
fixR <- function(x, d = 150) ifelse(is.na(x), x, ifelse(x > d, 0, 1))
fixA <- function(x) ifelse(!is.na(x), 1, x)
utm.proj <- paste0("+proj=utm +zone=", utm, " +north +ellps=GRS80 +units=m +no_defs ", sep = "")
P.utm.utm <- readOGR("../GIS/", layer = paste0("UTM", utm, "Intertidal_dist", sep = "")) %>%
  spTransform(., CRS(utm.proj))
P.utm.utm$distanceD150 <- fixR(P.utm.utm$distance)
P.utm.utm$distanceA <- fixA(P.utm.utm$distance)
pointsPolygon <- spTransform(points.layer, CRS = CRS(utm.proj))
print(gArea(P.utm.utm))
extent_r <- extent(P.utm.utm)
# class       : Extent
# xmin        : 367012.3
# xmax        : 373081.6
# ymin        : 5061473
# ymax        : 5069023

points.layer.points <- data.frame(pointsPolygon) %>% filter(Locality == "Mary's Point")
# points.layer.points = data.frame(pointsPolygon) %>% filter(Locality=="Crescent Beach")

extent_r <- extent(matrix(c(367012.3, 5061473, 373081.6, 5069023), nrow = 2))
# extent_r <- extent(matrix(c(388255.-3700,4898740-3700,388255.+3700 ,4898740+3700 ), nrow=2))
pointsPolygon@data <- as.data.frame(pointsPolygon@data)
buffg <- gBuffer(pointsPolygon, byid = T, width = 2500)
buffg@data$id <- rownames(buffg@data)
buffg.points <- fortify(buffg, region = "id")
buffg.points.df <- plyr::join(buffg.points, buffg@data, by = "id") %>% filter(Locality == "Mary's Point")
# buffg.points.df = plyr::join(buffg.points, buffg@data, by="id")%>% filter(Locality=="Crescent Beach")

P.utm.utm.crop <- crop(P.utm.utm, extent_r)
# plot(P.utm.utm.crop[["distanceD150"]])
P.utm.utm.crop@data$id <- rownames(P.utm.utm.crop@data)
P.utm.utm.crop.points <- fortify(P.utm.utm.crop, region = "id")
P.utm.utm.crop.df <- plyr::join(P.utm.utm.crop.points, P.utm.utm.crop@data, by = "id") %>%
  mutate(d150 = ifelse(distance == 150, "High", "Low"))
require(cowplot)
mapt_plt <-
  ggplot(P.utm.utm.crop.df) +
  aes(long, lat, group = group, fill = as.factor(d150)) +
  geom_polygon() +
  geom_polygon(data = buffg.points.df, aes(fill = NA), colour = "black") +
  geom_point(
    data = points.layer.points, colour = "black",
    aes(x = x, y = y, group = NULL, fill = NULL), size = 3
  ) +
  # geom_path(color="white") +
  coord_equal() +
  scale_fill_brewer(type = "qual", palette = "Set1", direction = 1, "Predation Danger") +
  theme(
    legend.position = "None", axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank()
  ) + labs(x = "", y = "") +
  # annotate("text",388000, 4900000, label = "Crescent Beach, NS" )
  annotate("text", 372000, 5063000, label = "Mary's Point, NB")
# ggsave("../IOC_Poster/images/SMALL_example.png",mapt_plt, width =6, height=6, dpi=600,bg='transparent' )


CA_MAP <- ggplot2::map_data(map = "world2", c("Canada", "USA")) # %>% filter(long>230&long<238 &lat<51 & lat>47)

mappointdat <- dat@data %>%
  mutate(
    Site.Code = paste(round(Lat, 2), round(Lon, 2), sep = "_"),
    indataset = ifelse(Site.Code %in% ACSS_South_Danger_forAnalysis$Site.Code, TRUE, FALSE)
  )

overviewPlot <-
  ggplot(CA_MAP) + geom_polygon(aes(x = long, y = lat, group = group),
    fill = NA,
    alpha = 0.8, color = "black"
  ) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  coord_fixed(1.1, xlim = c(292, 300), ylim = c(43.5, 48.5)) +
  guides(fill = FALSE) + # do this to leave off the color legend
  geom_point(
    data = mappointdat, aes(
      x = Lon + 360, y = Lat,
      size = n.counts, colour = indataset
    ),
    alpha = 0.5
  ) +
  scale_size_area() +
  labs(x = "", y = "", size = "Number of Surveys", colour = "Included in Analysis") +
  theme(
    axis.text = element_blank(), axis.line = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    axis.ticks = element_blank(),
    legend.position = "right"
  ) +
  scale_color_brewer(type = "qual", palette = "Set1", direction = 1) #+
# guides(colour = guide_legend(nrow = 2,
#                            byrow = TRUE))
