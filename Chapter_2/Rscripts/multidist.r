require(tidyverse)
require(lubridate)
all_back <- list.files("../cpp_version/Migration_Model/OrganizedResults/noU/distancemod/decisionmatrix/", "full_back_dist_", full.names = T)
distances_ <- all_back %>% substr(nchar(all_back)-7, nchar(all_back)-4)

all_backfiles <- map_df(all_back, function(x){ read_tsv(x) %>% mutate( dist = substr(x = x, nchar(x)-7, nchar(x)-4)) } )

# all_backfiles %>% 
#   filter(`d(s,t,x)`!=-9&s!=2)%>%
#   group_by(t,fuel,s) %>% summarize(d=mean(`d(s,t,x)`)) %>% 
#   ggplot(aes(t,fuel, fill=d)) + geom_raster() + facet_wrap(~s) +
#   scale_fill_distiller()

cols_danger <- c("Small" = "Red", #"#F8766D", 
                 "Large" = "Black",#"#00BA38",
                 "Depart" ="#619CFF" ,
                 "Medium" = "Purple")#

distance_backwards_plots <- 
all_backfiles %>% 
  filter(`d(s,t,x)`!=-9&s!=2)%>%
  mutate(
  DecName = factor(ifelse(`d(s,t,x)` == 0, "Small", ifelse(`d(s,t,x)`==1, "Large", "Depart")),
                   levels=c("Small", "Large", "Depart")),
SiteName = factor(ifelse(s == 0, "Small", ifelse(s==1, "Large", "Depart")),
                  levels=c("Small", "Large", "Depart"))) %>% 
  ggplot(aes(mdy("6-20-2013")+t,fuel/max(all_backfiles$fuel), fill=DecName)) + geom_raster() + 
  facet_grid(SiteName~dist) +
  scale_fill_manual(values=cols_danger) + 
  coord_cartesian(ylim=c(0,.50), xlim = c(mdy("6-20-2013")+0,mdy("6-20-2013")+90)) +
  labs(x= "", y="Relative Fuel Load",title="Migratory distance (km)")+
  theme(legend.position = 'none')


all_for_pk <- list.files("../cpp_version/Migration_Model/OrganizedResults/noU/distancemod/pk/", "full_forward_dist_", full.names = T)
all_for_nopk <- list.files("../cpp_version/Migration_Model/OrganizedResults/noU/distancemod/nopk/", "full_forward_dist_", full.names = T)
distances_for <- all_back %>% substr(nchar(all_back)-7, nchar(all_back)-4)
require(magrittr)
all_for_files <- bind_rows(map_df(all_for_pk, function(x){ read_tsv(x) %>% mutate( dist = substr(x = x, nchar(x)-7, nchar(x)-4)) } ) %>% 
  mutate(pk = "Prior Knowledge"),
  map_df(all_for_nopk, function(x){ read_tsv(x) %>% mutate( dist = substr(x = x, nchar(x)-7, nchar(x)-4)) } ) %>% 
    mutate(pk = "No Prior Knowledge")
  )
all_for_files %<>% mutate(Date = mdy("6-20-2013")+t,
                          range = 14000 * (1-1/sqrt(1 + fuel)),
                          speed = range / ((0.5 + fuel/0.033471538)))


# distlist <- tibble(dist=c("1300", "2300", "3500","5000", "7000", "9000"), 
#                    lab =  c("a", "b", "c", "d","e","f")) 
# 
# 
# prop_df <- expand.grid("a" = seq(0,1, by = 0.1),
#                        "b" =seq(0,1, by = 0.1) ,
#                        "c" = seq(0,1, by = 0.1),
#                        "d" = seq(0,1, by = 0.1),
#                        "e" = seq(0,1, by = 0.1),
#                        "f" = seq(0,1, by = 0.1)) %>% 
#   mutate(total=a+b+c+d+e+f) %>% filter(total==1.0) %>% select(-total) %>% 
#   rownames_to_column("group") %>% #mutate(id=as.numeric(id)) %>% 
#   gather(key = "lab",value =  "proportion", -group) %>% group_by(group) %>% 
#   nest
# 
# test1 <- prop_df[prop_df$group==1023,]
# 
# 
# sampleids <- function(x, df2) x %>% 
#   full_join(df2, by = "lab") %>% 
#   group_by(lab, group, proportion) %>% nest %>% 
#   mutate(samp = map2(data, proportion, sample_frac)) %>% 
#   select(-data) %>% unn est %>% left_join(all_for_files)
# 
# 
# a_df <- expand.grid(lab = c("a", "b", "c", "d","e","f"),
#                id =seq(0,9999)) 
# 
# j_df <-  expand.grid(lab = c("a", "b", "c", "d","e","f"),
#                      id=seq(10000,19999)) 
# 
# 
# 
# 
# 
# mod.dat <- bind_rows(a_df, j_df) %>% 
#   arrange(pk, id, t) %>% 
#   mutate(Age = ifelse(Age==0, "Adult", "Juvenile"),
#          Site = ifelse(s == 0, "Small", "Large"),
#          md= mdy("6-20-2001")+t,
#          date_i=floor_date(md, unit='week'),
#          week = t %/% 7)
# 
# test_df <- 
# all_for_files %>% group_by(id, pk, dist) %>% nest %>% left_join(distlist) %>% left_join(test1) %>% 
#   select(-id, -lab) %>%
#   mutate(samp = map2(data, proportion, sample_frac))

# pLarge <- all_for_files %>% 
#   mutate(f=round(fuel,2)) %>% 
#   group_by(t, s, dist) %>% 
#   summarise(n=n()) %>% 
#   group_by(t,dist) %>% 
#   spread(s,n) %>% 
#   replace_na(list(`0`=0 ,`1`=0)) %>% 
#   mutate(totalBirds = (`0` + `1`),
#     pLarge = `1`/totalBirds) %>% 
#   
#   ungroup %>% mutate(dist=as.numeric(dist))
#   
# ggplot(pLarge %>% filter(t>12&t<70) , aes(dist, pLarge, group=t, colour = t)) +
#   geom_line() + scale_colour_distiller(type='div')
# 
# ggplot(pLarge %>% filter(t==25|t==56) , aes(dist, totalBirds, group=t, colour = t)) +
#   geom_line() + scale_colour_distiller(type='div')
# 
# 
# 
# ggplot(all_for_files %>% filter(s!=2), 
#        aes(as.numeric(dist), fuel*22.7+22.7,fill =as.factor(s))) + stat_summary(geom='ribbon', fun.data='mean_cl_boot') + 
#   coord_cartesian(ylim=c(23,30))

flighrange_on_depart <- 
  all_for_files %>% 
  filter(s==2&pk=="No Prior Knowledge") %>% 
  ggplot(aes(Date, range,group=interaction(Age, pk))) + 
  stat_summary(fun.data='mean_cl_boot', geom='ribbon', alpha=0.25, colour = 'grey') +
  stat_summary(fun.y='mean', geom='line') +
  facet_grid(.~dist) + 
  labs(x = "", y="Flight Range on Departure (km)", title="Migratory distance (km)")
  
  

speed_on_depart <- 
  all_for_files %>% 
  filter(s==2&pk=="No Prior Knowledge") %>% 
  ggplot(aes(Date, speed,group=interaction(Age, pk))) + 
  stat_summary(fun.data='mean_cl_boot', geom='ribbon', alpha=0.25, colour = 'grey') +
  stat_summary(fun.y='mean', geom='line') +
  facet_grid(.~dist)+
  geom_hline(yintercept = 172, linetype=2)+
  labs(x = "", y="Instantaneous Speed of Migration (km/day)", title="Migratory distance (km)")




fuel_on_depart <- 
  all_for_files %>% 
  filter(s==2) %>% 
  ggplot(aes(Date, fuel, colour = Age)) + 
  stat_summary(fun.data='mean_cl_boot', geom='ribbon', alpha=0.25, colour = 'grey') +
  stat_summary(fun.y='mean', geom='line') +
  facet_grid(.~dist)





masses <- 
  all_for_files %>% 
  filter(s!=2) %>% 
  ggplot(aes(Date, 22.7*fuel+22.7, speed,group=interaction(Age, s), colour = as.factor(s))) + 
  stat_summary(fun.data='mean_cl_boot', geom='ribbon', alpha=0.25, colour = 'grey') +
  stat_summary(fun.y='mean', geom='line') +
  facet_grid(pk~dist) + 
  labs(x = "", y="Flight Range on Departure (km)", title="Migratory distance (km)")






