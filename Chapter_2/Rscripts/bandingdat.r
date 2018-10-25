## ---- chunkA ----
# modver <-  'deltaf'#"mut" #
# PloteVerything <-T

if(!exists("modver")){modver <- "changed_x"}
require(tidyverse)
require(lubridate)
require(cowplot)
select <- dplyr::select
u_val <- "noU"
if(!exists("nboot")){nboot <- 1000}
# Banding data from Dov.
# January 9, 2018
## ---- captureMasses ----
dat <- read.csv("../../Banding_Data/BB_SI_from_dov.csv", stringsAsFactors = F) 
banding_dat <-dat %>% filter(!is.na(Year)& Species == "WESA"&#!is.na(Age)&
                               Month %in% c(6,7,8)) %>%
  mutate(Date = mdy(paste(Month, Day, Year, sep = "-")),
         DoY = yday(Date),
         w = week(Date)
        )
fdat <- filter(banding_dat, Year > 1994 & Year <2002) %>% 
  mutate(df = mdy(paste(Month, Day, "2013", sep = "-")),
         age = ifelse(Age=="J", "Juvenile", "Adult"),
         Site = ifelse(site =="BB", "Boundary Bay", "Sidney Island"))
bandingsum <- fdat %>% group_by(site, age) %>% 
  summarize(Mass = mean(Mass, na.rm=T)) #%>% group_by(age) %>% 
  # mutate(deltaM = abs(diff(Mass))) %>% ungroup
all_years <- 
ggplot(banding_dat, aes(DoY, Mass, colour = site)) + 
  stat_summary(fun.data = 'mean_cl_boot') 

banding_dat_filt <- banding_dat %>% filter(Month !=6 &# Age %in% c("A", "J", "U") &
                        ((month(Date) ==7 & day(Date) < 25) |
                           (month(Date) ==8 & day(Date) > 5) ) &
                        site %in% 
                        c("BB", "SI")) %>% 
  mutate(MonthName = factor(ifelse(month(Date)==7, "July", ifelse(month(Date)==8, "August",NA)),
                            levels=c("July", "August"))) 
capturedat <- 
banding_dat_filt %>% group_by(year(Date), site, MonthName) %>% 
  # summarize(mnday=mean(yday(Date))) %>% 
  ggplot(aes(`year(Date)`, yday(Date), colour = site)) + 
    stat_summary(fun.data = 'mean_cl_boot') +
  facet_wrap(~MonthName, scales='free') +
  geom_smooth(method='lm')


FRPlot <- ggplot(banding_dat_filt , 
       aes(Date, Mass, colour = site)) + #(Mass-22.7)/22.7
  geom_smooth(method='lm') + facet_wrap(~MonthName) + 
  # stat_summary(geom='linerange',aes(x=mdy(paste(Month,15, Year, sep="-"))),
  #                fun.ymin = function(z) { quantile(z,0.025) },
  #                fun.ymax = function(z) { quantile(z,0.975) }
  #                ) +
  stat_summary(fun.data = 'mean_cl_boot',aes(x=mdy(paste(Month,15, Year, sep="-")))) +
  # stat_summary(fun.y = 'mean', geom='point',aes(x=mdy(paste(Month,15, Year, sep="-")))) +
  scale_colour_manual(values=c("BB"="Black", "SI"="Red")) + ylab("Mass at capture (g)")
# 
# ggplot(banding_dat_filt , 
#        aes(year(Date), Mass, colour = site)) + 
#   geom_smooth() + facet_wrap(~MonthName) + 
#   stat_summary(fun.data = 'mean_cl_boot') +
#   scale_colour_manual(values=c("BB"="Black", "SI"="Red")) + ylab("Mass at capture (g)")


sexratio <- banding_dat %>% group_by(site,DoY,Age, Sex) %>% summarise(n=n()) %>% ungroup() %>% spread(Sex, n) %>% mutate(p_F = F/(F+M))

# ggplot(sexratio %>% filter(site %in% c("BB", "SI")), aes(DoY, p_F, colour = site)) + geom_boxplot() + facet_wrap(~Age)

Wing_FRPlot <- ggplot(banding_dat_filt %>% filter(Sex!="U") , 
                 aes(Date, Wing, colour = site, shape=Sex, linetype=Sex)) + 
  geom_smooth(method='lm') + facet_wrap(~MonthName) + 
  stat_summary(fun.data = 'mean_cl_boot',aes(x=mdy(paste(Month,15, Year, sep="-")))) +
  scale_colour_manual(values=c("BB"="Black", "SI"="Red")) + ylab("Wing Length (mm)")


mean_site_diff <- banding_dat %>% 
  filter(Month !=6 &site %in% 
           c("BB", "SI")) %>% 
  group_by(Month, site) %>% summarize(Mass=mean(Mass,na.rm = T)) %>% 
  ungroup %>% 
  spread(key = site, value = Mass) %>% 
  mutate(L_minus_S=BB-SI,
         Age = ifelse(Month == 7, "Adult", "Juvenile"),
         ver="Banding") 
 

Mass_plot <- fdat %>% filter(Age == "J" | (Month !="8")) %>% #& Day <28)
   filter(Age!="J"| (Month !="7")) %>%#& Day > 3)
  filter(Year!=1997| site!="BB") %>%
  ggplot( aes(df, Mass, colour = interaction(age, Site,sep = "; ") )) + 
  # geom_point(alpha = 0.5, position = position_dodge(0.2))+
  stat_summary(fun.data = 'mean_cl_boot', position = position_dodge(0.4))  +
  # geom_hline(data = bandingsum, aes(yintercept = Mass, colour = Site, group = interaction(Site, Month))) +
  # facet_grid(~Year) +
  geom_hline(yintercept = 22.7, linetype=2) + 
  geom_smooth(aes(group = interaction(age, Site)))+
  scale_color_brewer(type='qual', palette='Set1', direction = -1)+
  # scale_colour_manual(values = c("BB" = '#619CFF', "SI" = "#F8766D")) +
  labs(colour = "Site", x = "") +#+ coord_cartesian(ylim=c(15, 35))
  stat_summary(shape = 2,
    data = filter(fdat,Year ==1997&site=="BB"), alpha = 0.5,
    fun.data = 'mean_cl_boot', position = position_dodge(0.4)) 


# banding_dat_filt %>% 
#   filter(Sex!="U") %>% 
#   ggplot(aes(Mass, fill=interaction(Sex,site)) )+ 
#   geom_density(alpha=0.25) + scale_fill_brewer(type='qual', palette = 'Set1', direction = -1) +
#   facet_grid((Year-Year%%5)~Month) +
#   geom_vline(aes(xintercept = 22.7))
# banding_dat_filt %>% 
#   # filter(Sex!="U") %>% 
#   ggplot(aes(DoY, fill=interaction(Sex,site)) )+ 
#   geom_histogram(alpha=0.5, binwidth = 1) + scale_fill_brewer(type='qual', palette = 'Set1', direction = -1) +
#   facet_grid((Year-Year%%5)~Month, scales = 'free_x') 

# Mass_plot
## ---- mass-notneeded ----
# banding_dat %>% filter(Year ==1997) %>% 
#   ggplot(aes(DoY, Mass, colour = site, shape = Age)) + 
#   geom_point(position=position_dodge(0.3))

# fdat %>% filter(Age == "J" | (Month !="8"& Day <28)) %>% 
#   filter(Age!="J"| (Month !="7"& Day > 3)) %>% group_by(Site, age) %>% 
#   summarize(Mass = mean(Mass, na.rm=T)) %>% group_by(age) %>% 
#   mutate(deltaM = diff(Mass)) %>% ungroup

# banding_dat %>% filter(Year < 1984) %>% 
#   mutate(age = ifelse(DoY < 208, "Adult", "Juvenile")) %>% 
#   ggplot(aes(DoY, Mass, colour = interaction(age, site) ) )  + 
#   # geom_point(alpha=0.5)+
#   stat_summary(fun.data = 'mean_cl_boot', position = position_dodge(0.2)) +
#   geom_smooth(formula = "y~1", method='lm')+
#    theme(legend.position = 'none')


# ggsave(filename = "../ThesisChapter/figures/Mass_Birds.png", 
#        Mass_plot,
#       width = 8, height = 5)


# fdat %>% 
# ggplot( aes(DoY, fill = as.factor(Year))) + geom_bar(position='stack', stat = 'count') +
#   facet_wrap(~site)

# curve(1/(1+exp(-2*(x-0.5))), 0, 3, ylim=c(0,1))


bandingsum_week <- fdat %>% group_by(site,w) %>% 
  summarize(Mass = mean(Mass, na.rm=T)) %>% group_by(w) %>% 
  # slice(nrow(.)>1) %>% 
  summarize(deltaM = abs(diff(Mass))) %>% ungroup 

## ---- mass-model-match ----
source("../Rscripts/model_data_import.r")
if(modver == 'deltaf'){
      mod.dat_nopk <- read_tsv(paste0("../cpp_version/Migration_Model/OrganizedResults/",u_val,"/MassCorrection/noPK/deltaf/baseline/forwards_sim.txt"),col_types=cols()) %>% 
        mutate(Mass = fuel*22.7+22.7) %>% mutate(priorK = "No Prior Knowledge") 
      
      mod.dat_pk <- read_tsv(paste0("../cpp_version/Migration_Model/OrganizedResults/",u_val,"/MassCorrection/PK/deltaf/baseline/forwards_sim.txt"),col_types=cols()) %>% 
        mutate(Mass = fuel*22.7+22.7) %>% mutate(priorK = "Prior Knowledge")
      
      
      mod.datback <- dataimport(input_data = 
                                  paste0("../cpp_version/Migration_Model/OrganizedResults/",u_val,"/MassCorrection/noPK/deltaf/baseline/full_back_single.txt"),
                                dat_type = 'back') 
      
    
}
if(modver == 'mut'){
    mod.dat_nopk <- read_tsv(paste0("../cpp_version/Migration_Model/OrganizedResults/",u_val,"/MassCorrection/noPK/mut/baseline/forwards_sim.txt"),col_types=cols() )%>%
      mutate(Mass = fuel*22.7+22.7) %>% mutate(priorK = "No Prior Knowledge")
    
    mod.dat_pk <- read_tsv(paste0("../cpp_version/Migration_Model/OrganizedResults/",u_val,"/MassCorrection/PK/mut/baseline/forwards_sim.txt")) %>%
    mutate(Mass = fuel*22.7+22.7) %>% mutate(priorK = "Prior Knowledge")
    mod.datback <- dataimport(input_data = 
                                paste0( "../cpp_version/Migration_Model/OrganizedResults/",u_val,"/MassCorrection/noPK/mut/baseline/full_back_single.txt"),
                              dat_type = 'back') 
}


if(modver == 'changed_x'){
  mod.dat_nopk <- read_tsv(paste0("../cpp_version/Migration_Model/OrganizedResults/",u_val,"/MassCorrection/noPK/changed_x/baseline/forwards_sim.txt")) %>%
    mutate(Mass = fuel*22.7+22.7) %>% mutate(priorK = "No Prior Knowledge")
  
  mod.dat_pk <- read_tsv(paste0("../cpp_version/Migration_Model/OrganizedResults/",u_val,"/MassCorrection/PK/changed_x/baseline/forwards_sim.txt"),col_types=cols()) %>%
    mutate(Mass = fuel*22.7+22.7) %>% mutate(priorK = "Prior Knowledge")
  mod.datback <- dataimport(input_data = 
                              paste0("../cpp_version/Migration_Model/OrganizedResults/",u_val,"/MassCorrection/noPK/changed_x/baseline/full_back_single.txt"),
                            dat_type = 'back') 
}


mod.dat <- bind_rows(mod.dat_nopk, mod.dat_pk) %>% 
  arrange(priorK, id, t) %>% 
  mutate(Age = ifelse(Age==0, "Adult", "Juvenile"),
         Site = ifelse(s == 0, "Small", "Large"),
         md= mdy("6-20-2001")+t,
         date_i=floor_date(md, unit='week'),
         MaxLoS = ifelse(s == lead(s)| id != lead(id), NA, LoS),
         pres=ifelse(s==lead(s)&id==lead(id)&priorK==lead(priorK),1,0),
           # ifelse(is.na(MaxLoS)&s!=2,1,0 ),
         week = t %/% 7)

model_data <- mod.dat %>% filter(s!=2) %>% 
  group_by(Age, t, md,Site,s, priorK, date_i) %>% 
  summarize(
    n=n(),
    Mass = mean(fuel*22.7+22.7, na.rm=T),
    LoS = mean(LoS), 
    p_pres = mean(pres, na.rm=T)) %>% 
  group_by(t,Age, priorK) %>% 
  mutate(n_s=n()) %>%ungroup
  #filter(n_s=2) %>% 
  # mutate(deltaM = diff(Mass)) %>% ungroup

# ggplot(model_data %>% select(priorK, Age, t, deltaM) %>% distinct, aes(deltaM)) + 
#   geom_histogram(binwidth = 0.1) +
#   facet_wrap(priorK~Age)
hist_plot <- 
  ggplot(mod.dat, aes(md, fill = Age)) + geom_histogram(binwidth = 1)+ facet_grid(priorK~Site) +
    scale_fill_grey()



# plotres(mod.datback, plot_type = 'back')

# meancompare <- 
# mod.dat %>% filter(s!=2) %>% 
#   group_by(Age, s, priorK) %>% 
#   summarize(
#     n=n(),
#     Mass = mean(fuel*22.7+22.7, na.rm=T)) %>% 
#   group_by(Age, priorK) %>% 
#   mutate(n=n()) %>%# filter(n==2) %>% 
#   spread(s,Mass) %>% 
#   mutate(L_minus_S= `1`-`0`,ver='Model') %>% 
#   bind_rows(mean_site_diff) %>% 
#   select(Age, L_minus_S, ver, priorK)
  


# ggplot(mod.dat, aes(Site, y=Mass)) + facet_grid(~Age) + stat_summary(fun.data = 'mean_cl_boot') 
  
tr <- 
mod.dat %>% filter(s!=2) %>% 
  group_by(Age, Site, priorK, md) %>% 
  summarize(n=n()) %>% ungroup %>% 
  mutate(nr = n/max(n))

ModDat <- 
mod.dat %>% filter(s!=2) %>% 
  ggplot(aes(md, Mass,colour = Site, linetype=Age)) +
  stat_summary(fun.data = 'mean_cl_boot', geom='ribbon', colour='grey', alpha=0.2,
              aes( group=interaction(Site, Age))) +
  # geom_bar(data=tr, aes(y=nr, fill=Site, colour = NA),
  #          stat='identity')+
             #colour= interaction(Site,Age, sep = "; ")) )  +
  # geom_smooth()+
  # geom_point(alpha=0.05)+
  stat_summary(geom='line', fun.y='mean', na.rm=T)+
  
  facet_wrap(~priorK) +
  labs(colour = "Site", x="") +#+ ylim(20,35)
  geom_hline(yintercept = 22.7, linetype=2) +
  scale_colour_manual(values = c("Large"="Black", "Small"="Red")) +
  theme(axis.text.x = element_text(hjust = -0.75))
## ---- mass-data-extra ----
# ggsave(filename = "../ThesisChapter/figures/Mass_matching.png", 
#        ModDat,
#        width = 8, height = 5)


mass_i <- mod.dat_nopk %>% group_by(s,Age) %>% filter(s!=2) %>%
  summarize(m=mean(Mass)) %>%ungroup

mass_i_pk <- mod.dat_pk %>% group_by(s,Age) %>% filter(s!=2) %>%
  summarize(m=mean(Mass)) %>%ungroup

## ---- mass-model-trend ----
# Model predation change --------------------------------------------------

lowp_Nopk <- read_tsv(paste0("../cpp_version/Migration_Model/OrganizedResults/",u_val,"/MassCorrection/noPK/",
  modver, "/lowpred/forwards_sim.txt", sep=""),col_types=cols()) %>% 
  mutate(Mass = fuel*22.7+22.7) %>% mutate(priorK = "No Prior Knowledge", pred='Low')

highp_Nopk <- read_tsv(paste0("../cpp_version/Migration_Model/OrganizedResults/",u_val,"/MassCorrection/noPK/",
  modver, "/highpred/forwards_sim.txt"),col_types=cols()) %>% 
  mutate(Mass = fuel*22.7+22.7) %>% mutate(priorK = "No Prior Knowledge", pred='High')

lowp_pk <- read_tsv(paste0("../cpp_version/Migration_Model/OrganizedResults/",u_val,"/MassCorrection/PK/", modver,
  "/lowpred/forwards_sim.txt"),col_types=cols()) %>% 
  mutate(Mass = fuel*22.7+22.7) %>% mutate(priorK = "Prior Knowledge", pred='Low')


highp_pk <- read_tsv(paste0("../cpp_version/Migration_Model/OrganizedResults/",u_val,"/MassCorrection/PK/", modver,
  "/highpred/forwards_sim.txt"),col_types=cols()) %>% 
  mutate(Mass = fuel*22.7+22.7) %>% mutate(priorK = "Prior Knowledge", pred='High')


pred_mass <- bind_rows(list(lowp_Nopk, lowp_pk, highp_Nopk, highp_pk, mod.dat_nopk, mod.dat_pk)) %>% 
  mutate(
    Scenario = factor(ifelse(is.na(pred), "Baseline", pred),
                      levels=c("Low", "Baseline", "High")),
    Age = ifelse(Age==0, "Adult", "Juvenile"),
         Site = ifelse(s == 0, "Small",ifelse(s==1, "Large", NA)),
         md= mdy("6-20-2013")+t,
    MaxLoS = ifelse(s == lead(s)| id != lead(id), NA, LoS),
    pres=ifelse(is.na(MaxLoS)&s!=2,1,0 ),
    week = t %/% 7)

# ggplot(pred_mass %>% filter(s==0), aes(t, fill = Age) ) + 
#   facet_grid(priorK~Scenario) + geom_bar()

ydenberg2004_match_full <- 
pred_mass %>% 
  # group_by(priorK, id) %>% arrange()
  # group_by(priorK, id ) %>%
  # arrange(t) %>%
  # mutate(
  #   delta_s = ifelse(id!=lead(id)|s==2,"Departed",
  #                    ifelse(
  #                      lead(s)==s, "Remain at current site", paste(s, "to", lead(s))))) %>% 
  # ungroup %>% 
  # filter(delta_s!="1 to 0"&!is.na(delta_s)) %>% 
  filter(s!=2) %>% 
  group_by(priorK, Scenario,Age,  Site) %>% 
  summarize(n=n(), N_t = n_distinct(id))

ydenberg2004_match <- 
  pred_mass %>% 
  filter(s!=2) %>% 
  group_by(priorK, Scenario,Site) %>% 
  summarize(n=n(), N_t = n_distinct(id))


countPlot <- 
  ydenberg2004_match_full %>% filter(priorK=="Prior Knowledge") %>% 
  mutate(x_id = ifelse(Scenario=='Low', 0, ifelse(Scenario=="Baseline",1,2))) %>% 
  #filter(priorK == "Prior Knowledge" ) %>% 
  ggplot( aes(Scenario, n, colour = Site, group=Site, linetype=Site)) + geom_line(size=1) +
  # geom_line(linetype = 2, aes(y=N_t*5), size=1) +
  facet_grid(Age~., scales='free_y')  +
  scale_y_continuous(name = "Cumulative counts")+
  #, sec.axis = sec_axis(~./5, name = "True number of birds (dashed line)")) +
  scale_colour_manual(values = c("Large"="Black", "Small"="Red")) 

countPlotb <- 
  ydenberg2004_match_full %>% filter(priorK=="Prior Knowledge") %>% 
  mutate(x_id = ifelse(Scenario=='Low', 0, ifelse(Scenario=="Baseline",1,2))) %>% 
  #filter(priorK == "Prior Knowledge" ) %>% 
  ggplot( aes(Scenario, N_t, colour = Site, group=Site, linetype=Site)) + geom_line(size=1) +
  # geom_line(linetype = 2, aes(y=N_t*5), size=1) +
  facet_grid(Age~., scales='free_y')  +
  scale_y_continuous(name = "True number of birds")+
  #, sec.axis = sec_axis(~./5, name = "True number of birds (dashed line)")) +
  scale_colour_manual(values = c("Large"="Black", "Small"="Red")) 

countsvsNumbers <- 
plot_grid(countPlot+ theme(legend.position = 'none'), countPlotb, nrow=1)

ydenberg_countPlot <- 
ydenberg2004_match %>% filter(Site=="Small") %>% 
         mutate(x_id = ifelse(Scenario=='Low', 0, 
                              ifelse(Scenario=="Baseline",1,2))) %>% 
         #filter(priorK == "Prior Knowledge" ) %>% 
         ggplot( aes(Scenario, n, group=1)) +
  geom_smooth(linetype=2,method='lm', se=F, colour='grey')+
    geom_smooth(linetype=2,method='lm', se=F,aes(y=N_t), colour='grey') +
  geom_point(colour = 'Red', shape = 0) +
  geom_point(shape=23,aes(y=N_t), fill = 'Red') +
  geom_text(aes("Low", 35000,label=as.character(expression(N[T]))), parse=T, position = position_nudge(x=0.2))+
  geom_text(aes("Baseline", 80000,label=as.character(expression(N[C]))), parse=T, position = position_nudge(x=0.2))+
  labs(y=expression("N"["T"]~"or"~"N"["C"]),
       x="") + 
  scale_y_continuous(breaks = c(0,5000,10000,15000,20000,seq(40000,180000,20000))) +
  facet_wrap(~priorK)


predmass_plot <- 
  pred_mass %>% filter(s!=2) %>% 
  ggplot(aes(colour = Scenario, y=Mass, x= interaction(Site,Age, sep = "\n")) )  +
  geom_boxplot(position=position_dodge(width =0.9))+
  # stat_summary(fun.data = 'mean_cl_boot', 
  #              position=position_dodge(0.2)) + 
  facet_wrap(~priorK) +
  labs(x = "Site and Age", colour="Predator\nPopulation") +
  scale_colour_discrete(direction=-1,  h=c(360,0)+15, h.start = 3)


# ggsave(filename = "../ThesisChapter/figures/Mass_matching_predation.png", 
#        predmass_plot,
#        width = 8, height = 5)

## ---- los-model-trend ----
predLOS_plot <- 
  pred_mass %>% filter(s!=2&!is.na(MaxLoS)) %>% 
  # group_by(t, Site, Age, Scenario, priorK) %>% 
  # summarize(LoS = mean(LoS, na.rm=T)) %>% 
  ggplot(aes(colour = Scenario,
             y=MaxLoS, x= Site))+#interaction(Site,Age, sep = "\n")) )  +

  # stat_summary(alpha=0.4, fun.y = 'mean',
  #              geom='point',
  #              # position=position_dodge(0.2),
  #              aes(group=interaction(t,Scenario, Site, Age)), 
  #              position=position_jitterdodge(jitter.width = 0.0, dodge.width = 0.5)) +
  geom_boxplot(data=  pred_mass%>% filter(s!=2&!is.na(MaxLoS)), position=position_dodge(width =0.9),#%>% filter(s!=2) %>% 
                 # group_by(Site, Age, Scenario, priorK) %>%
                 # summarize(MaxLoS = mean(MaxLoS, na.rm=T)),
                 alpha = 0.0,outlier.shape = 2)+
  facet_wrap(priorK~Age) +
  labs(x = "Site and Age", colour="Predator\nPopulation", y = "Length of Stay") +
  scale_colour_discrete(direction=-1,  h=c(360,0)+15, h.start = 3)

# ggsave(filename = "../ThesisChapter/figures/LoS_matching_predation.png", 
#        predLOS_plot,
#        width = 8, height = 5)

## ---- los-data ----
J_BS <- readxl::read_excel("/home/dhope/Documents/SFU/MSc/Sandpiper Project/Barry Smith Analysid - Data, outputs etc/WESA Residence Time Summary (editedOIKOS) (version 1).xls",
                           sheet = "WESA J Weighted", skip = 2) %>% 
  mutate(Age = "Juvenile") %>% select(Age, G, W, Beginning, Year, `Res Time`, Pred, V)
A_BS <- readxl::read_excel("/home/dhope/Documents/SFU/MSc/Sandpiper Project/Barry Smith Analysid - Data, outputs etc/WESA Residence Time Summary (editedOIKOS) (version 1).xls",
                           sheet = "WESA A Weighted", skip = 2) %>% 
  mutate(Age = "Adult") %>% select(Age, G, W, Beginning,  `Res Time`, Pred, V)

BS_Dat <- bind_rows(A_BS, J_BS) %>% mutate(date_i = ymd(Beginning)) %>% group_by(G, Age) %>% 
  mutate(meanlos = mean(`Res Time`, na.rm=T)) %>% ungroup %>% 
  mutate(rlos = (`Res Time` - meanlos)/meanlos)

cols_danger <- c("Small" = "Red", #"#F8766D", 
                 "Large" = "Black",#"#00BA38",
                 "Depart" ="#619CFF" )#

sum_resprob_mod <- 
model_data %>% filter(s!=2& yday(date_i)>=yday(ymd("2001-07-04")-3) &yday(date_i)<=yday(ymd("2001-08-29")+6) ) %>% 
  group_by(Age, Site, priorK, date_i) %>%
  summarize(pres=mean(p_pres), n=n()) %>% filter(n>1)
# glm_i_pk <- mod.dat %>% filter(s!=2& 
#                                  yday(date_i)>=yday(ymd("2001-07-04")) &
#                                  yday(date_i)<=yday(ymd("2001-08-29")) & 
#                                  priorK =="Prior Knowledge") %>% 
# glm(data=., formula = pres~date_i*Site*Age, family = 'binomial')
# 
# glm_i_nopk <- mod.dat %>% filter(s!=2& 
#                                  yday(date_i)>=yday(ymd("2001-07-04")) &
#                                  yday(date_i)<=yday(ymd("2001-08-29")) & 
#                                  priorK !="Prior Knowledge") %>% 
#   glm(data=., formula = pres~date_i*Site*Age, family = 'binomial')
# 
# aug.mod.glm_pk <- broom::augment(glm_i_pk, mod.dat %>% filter(s!=2&
#                                                           yday(date_i)>=yday(ymd("2001-07-04")) &
#                                                           yday(date_i)<=yday(ymd("2001-08-29"))&
#                                                           priorK =="Prior Knowledge"))
# aug.mod.glm_nopk <- broom::augment(glm_i_nopk, mod.dat %>% filter(s!=2&
#                                                           yday(date_i)>=yday(ymd("2001-07-04")) &
#                                                           yday(date_i)<=yday(ymd("2001-08-29"))&
#                                                           priorK !="Prior Knowledge"))
# inverse_logit <- function(x) plogis(x)



LoS_compare <- 
      ggplot() + 
        # stat_summary(fun.y='mean', geom='point',colour='red', alpha=0.2, shape = 3,
        #              data=BS_Dat, aes(date_i, V, shape =Age))+
        geom_line(alpha=0.3,colour = 'red',linetype=2,
        data=BS_Dat, aes(date_i, Pred, group = interaction(G, Age))) +
        # geom_line(data=bind_rows(aug.mod.glm_nopk, aug.mod.glm_pk), aes(x=date_i, y=inverse_logit(.fitted), colour = Site,
        #               linetype = Age)) +
        # geom_line(data=J_BS)+
        stat_summary(data = model_data %>% filter(s!=2& yday(date_i)>=yday(ymd("2001-07-04")-3) &yday(date_i)<=yday(ymd("2001-08-29")+6) ),#& priorK=="Prior Knowledge"),
                     position=position_dodge(1),
                     aes(x=date_i,
                         #floor_date(as.Date("04/20/2017", "%m/%d/%Y"), unit="week")
                         y = p_pres, colour = Site,
                                          shape = Age
                         ), geom='pointrange',
                     fun.data = 'mean_cl_boot') +
        # geom_point(data = model_data %>% filter(s!=2& yday(date_i)>=yday(ymd("2001-07-04")-3) &yday(date_i)<=yday(ymd("2001-08-29")+6) ),#& priorK=="Prior Knowledge"),
        #            position=position_dodge(1),
        #            aes(x=md,
        #                #floor_date(as.Date("04/20/2017", "%m/%d/%Y"), unit="week")
        #                y = p_pres, colour = Site,
        #                shape = Age)) +
        #   stat_summary(data = mod.dat %>% filter(s!=2& yday(date_i)>=yday(ymd("2001-07-04")) &yday(date_i)<=yday(ymd("2001-08-29")) ),#& priorK=="Prior Knowledge"), 
        #              # position=position_dodge(2),
        #              aes(x=date_i,
        #                  y = pres, colour = Site,
        #                  linetype = Age), geom='line',
        #              fun.y = 'mean') + 
        facet_wrap(~priorK) +
        scale_x_date(breaks=mdy(c("7-1-2001", "8-1-2001","9-1-2001")),
          # date_breaks="1 month", 
          labels=c("Jul", "Aug", " ") ) +#date_labels = "%b")+
        scale_colour_manual(values = cols_danger) + 
        theme(axis.text.x=element_text(hjust = -1.0)) +
        labs(x = "Date", y="Daily residence probabilities")# +
        # theme(axis.text.x = element_text(hjust = -0.75)) 

rawdat <- 
mod.dat %>% filter(!is.na(MaxLoS)) %>% group_by(Site, priorK,Age) %>% 
  mutate(meanlos = mean(MaxLoS)) %>% ungroup %>% 
  mutate(rlos= (MaxLoS - meanlos)/meanlos)
  
LoS_compare2 <- 
  ggplot() + 
   geom_line(alpha=0.3,colour = 'red',linetype=2,
            data=BS_Dat, aes(date_i, rlos*100, group = interaction(G, Age))) +
  stat_summary(data = rawdat %>% filter(s==0& yday(date_i)>=yday(ymd("2001-07-04")-3) &yday(date_i)<=yday(ymd("2001-08-29")+6) ),#& priorK=="Prior Knowledge"),
               position=position_dodge(1),
               aes(x=date_i,
      
                   y = rlos*100, colour = Site,
                   shape = Age
               ), geom='pointrange',
               fun.data = 'mean_cl_boot') +
facet_grid(priorK~., scales='free_y') +
  scale_x_date(breaks=mdy(c("7-1-2001", "8-1-2001","9-1-2001")),
              
               labels=c("Jul", "Aug", " ") ) +#date_labels = "%b")+
  scale_colour_manual(values = cols_danger) + 
  theme(axis.text.x=element_text(hjust = -1.0)) +
  labs(x = "Date", y="Percent change in lengths of stay from mean")# 



# Bootstrapping Plots -----------------------------------------------------


# test_dat <- tibble(id=1:2000, Month = rep(7:8, nboot), s= round(runif(2000),0),
#                    site = ifelse(s==0, "BB", "SI"), Mass = rnorm(2000, 25, 5))
#                    


sumdat <- function( dat){
  # d <- dat[i,]
  # print(as.data.frame(dat))
  analysis(dat) %>% group_by(Month, site) %>% 
    summarize(Mass=mean(Mass,na.rm = T)) %>% 
    ungroup %>% 
    spread(key = site, value = Mass) %>% 
    mutate(L_minus_S=BB-SI,
           Age = ifelse(Month == 7, "Adult", "Juvenile"),
           ver="Banding") 
}


library(purrr)
library(modelr)
catchDat <- banding_dat %>% 
  filter(Month !=6 &# Age %in% c("A", "J", "U") &
           ((month(Date) ==7 & day(Date) < 25) |
              (month(Date) ==8 & day(Date) > 5) ) &
           site %in% 
           c("BB", "SI"))

dailycatch <- catchDat %>% group_by(DoY) %>% 
  summarize(n=n()) %>% ungroup()
# boot <- bootstrap(catchDat ,n =  2000)
# models <- map_df(boot$strap, ~sumdat(dat=.), .id='id')
require(boot)
require(rsample)
bootstrapruns <- catchDat %>% 
  rsample::bootstraps(times=nboot) 
  
bootstrapresults <- map_df(bootstrapruns$splits, sumdat)



# cat("Adults ------------------------------------\n")
# qnorm(c(0.025, 0.975), mean(bootstrapresults$L_minus_S[bootstrapresults$Age=="Adult"]), 
#       sd(bootstrapresults$L_minus_S[bootstrapresults$Age=="Adult"]))

# quantile(bootstrapresults$L_minus_S[bootstrapresults$Age=="Adult"], probs = c(0.025, 0.975))
# cat("Juveniles ------------------------------------\n")
# qnorm(c(0.025, 0.975), mean(bootstrapresults$L_minus_S[bootstrapresults$Age=="Juvenile"]), 
#       sd(bootstrapresults$L_minus_S[bootstrapresults$Age=="Juvenile"]))

# quantile(bootstrapresults$L_minus_S[bootstrapresults$Age=="Juvenile"], probs = c(0.025, 0.975))


alpha = .05
catch_boot <- bootstrapresults %>% group_by(Age) %>% 
  summarize(
            lci = quantile(L_minus_S, alpha / 2, na.rm=T),
            uci = quantile(L_minus_S, 1 - alpha / 2, na.rm=T),
            L_minus_S= mean(L_minus_S, na.rm=T)) %>% 
  mutate(ver="Capture\nData", priorK="Capture Data")





sumModDat <- function(dat,var="Mass", scen=F){
  as.data.frame(dat) %>% filter(s!=2) %>%
  {
  if(isTRUE(scen)){
    group_by(., Age, Site, priorK, Scenario)  
  } else{ group_by(., Age, Site, priorK)  } } %>% 
  {
    if(var=="Mass") summarize(. , Mass = mean(fuel*22.7+22.7, na.rm=T)) 
    else if(var=="LoS") summarize(., Mass = mean(MaxLoS, na.rm=T) )
  } %>% 
    ungroup %>% 
    # group_by(Age, priorK) %>% 
    # mutate(n=n()) %>%# filter(n==2) %>% 
    spread(Site,Mass) %>% 
    mutate(L_minus_S= Large-Small,ver='Simulation\noutput')
}
#https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
mod.dat.sample <- function(dat, catch_n, i=1){
  dat %>% mutate(DoY = as.numeric(yday(md))) %>% 
    group_by(DoY) %>% 
    nest() %>% left_join(catch_n, by = c("DoY")) %>%
    filter(!is.na(n)) %>% 
    mutate(samp = map2(data, n, sample_n, replace=T)) %>% 
    select(DoY, samp) %>%
    unnest() %>% mutate(.id=i)
}


mod.dat.sampled <- mod.dat.sample(mod.dat, dailycatch)
sampledSummary <- sumModDat(mod.dat.sampled)

require(boot)
bootstrapresults_mod <- mod.dat %>% 
  mutate(Month=month(md), Day = day(md)) %>% 
  filter(Month !=6 &# Age %in% c("A", "J", "U") &
           ((Month ==7 &Day < 25) |
              (Month==8 & Day > 5) )) %>% 
  # filter(priorK=="Prior Knowledge") %>%
  # bootstrap(nboot) %>% 
  rerun(.n = nboot, sample_i =mod.dat.sample(dat=., catch_n=dailycatch)) %>% 
  map_df(~sumModDat(.x$sample_i))

# bootstrapresults_mod_nopk <- mod.dat %>% 
#   filter(priorK=="No Prior Knowledge") %>%
#   bootstrap(nboot) %>% 
#   group_by(.id) %>%
#   do(sumModDat(dat=.[['strap']]))

ci_Mod <-  bootstrapresults_mod %>% 
  #bind_rows(bootstrapresults_mod_pk, bootstrapresults_mod_nopk) %>% 
  group_by(priorK, Age) %>% 
  summarize(
            lci = quantile(L_minus_S, alpha / 2, na.rm=T),
            uci = quantile(L_minus_S, 1 - alpha / 2, na.rm=T),
            L_minus_S= mean(L_minus_S, na.rm=T)) %>% 
  mutate(ver="Simulation\noutput") 

ci_boot_res <- bind_rows(ci_Mod, catch_boot)

modvscapture <- ggplot(ci_boot_res, aes(ver, L_minus_S, colour = priorK)) + 
  geom_pointrange(aes(ymin=lci, ymax=uci))+
  facet_grid(Age~.) +
  # geom_point(data=sampledSummary, aes(x="Simulation\noutput", y=L_minus_S), shape=2)+
  ylim(0,3.2) +
  labs(x="", y="Differences in mean mass\n(g; Large minus small sites)",
       colour = "") + scale_color_brewer(palette = 2,type = 'qual')
# ggsave(paste0(".img/Model_mass_compare_", modver, ".png", sep=""), modvscapture, width = 6, height = 4,
#        units='in', dpi=600,type = "cairo")



scen_prod <- 
  pred_mass %>% 
  mutate(Month=month(md), Day = day(md)) %>% 
  filter(Month !=6 &# Age %in% c("A", "J", "U") &
           ((Month ==7 &Day < 25) |
              (Month==8 & Day > 5) )) %>% 
  rerun(.n = nboot, sample_i =mod.dat.sample(dat=., catch_n=dailycatch)) %>% 
  map_df(~sumModDat(.x$sample_i, scen = T))
  # bootstrap(nboot) %>% 
  # group_by(.id) %>%
  # do(sumModDat(.$strap,scen = T))

# scen_prod <- 
#   pred_mass %>% 
#   group_by(Scenario) %>% 
#   do(rs=bootstrap(.,nboot)) %>% 
#   group_by(Scenario) %>% 
#   unnest %>% 
#   group_by(Scenario,.id) %>% 
#   do(sumModDat(.$strap))

calc_ci <- function(dat){
  dat %>% 
    summarize(
      lci_L = quantile(Large, alpha / 2, na.rm=T),
      uci_L = quantile(Large, 1 - alpha / 2, na.rm=T),
      Large= mean(Large, na.rm=T),
      
      lci_S = quantile(Small, alpha / 2, na.rm=T),
      uci_S = quantile(Small, 1 - alpha / 2, na.rm=T),
      Small= mean(Small, na.rm=T),
      
      lci_delta = quantile(L_minus_S, alpha / 2, na.rm=T),
      uci_delta = quantile(L_minus_S, 1 - alpha / 2, na.rm=T),
      L_minus_S= mean(L_minus_S, na.rm=T)) %>% 
    mutate(ver="Simulation\noutput") 
}


mod_scen <- scen_prod %>% 
  group_by(Scenario,priorK, Age) %>% 
  calc_ci


Scenario_mass <- 
ggplot(mod_scen, aes(Scenario, L_minus_S)) +
  geom_pointrange(aes(ymin=lci_delta, ymax=uci_delta)) +
  facet_grid(Age~priorK) + 
  labs(x="Predation Scenario", y="Mean difference in masses\n(Large - Small; g)",
       colour = "")

# ggsave(paste0(".img/scenario_mass_plot_", modver, ".png", sep=""),Scenario_mass,  width = 6, height = 4,
#        units='in', dpi=600,type = "cairo")


Scenario_mass_r <- 
  ggplot(mod_scen, aes(Scenario, Large)) +
  geom_linerange(aes(ymin=lci_L, ymax=uci_L), colour = 'black')+
  geom_linerange(aes(y=Small, ymin=lci_S, ymax=uci_S), colour = 'red')+
  geom_line(aes(group=1))+ geom_line(aes(y=Small, group=1), colour='red', linetype=2) +
  facet_grid(Age~priorK) + 
  labs(x="Predation Scenario", y="Mean masses \n(Large (black) and Small (red) sites; g)",
       colour = "")


# ggsave(paste0(".img/scenario_mass_plotRaw_", modver, ".png", sep=""),Scenario_mass_r,  width = 6, height = 6,
#        units='in', dpi=600,type = "cairo")

# ggplot(scen_prod, aes(Scenario, Small, colour = priorK)) +
#   stat_summary()+ 
#   stat_summary(aes(y=Large), shape = 2)+
#   facet_grid(Age~.)


# LOS-bootstrap -----------------------------------------------------------

bootstrap_mod_los <- mod.dat %>% 
  # filter(priorK=="Prior Knowledge") %>%
  rsample::bootstraps(nboot) 
bootstrapresults_mod_los <- map_df(bootstrap_mod_los$splits, sumModDat,var="LoS")

ci_Mod_los <-  bootstrapresults_mod_los %>% 
  #bind_rows(bootstrapresults_mod_pk, bootstrapresults_mod_nopk) %>% 
  group_by(priorK, Age) %>% 
 calc_ci



scen_prod_los_runs <-  pred_mass %>% rsample::bootstraps(nboot) 
  
scen_prod_los <- map_df(scen_prod_los_runs$splits, sumModDat, var="LoS", scen = T)

mod_scen_los <- scen_prod_los %>% 
  group_by(priorK, Age, Scenario) %>% 
 calc_ci

los_plot <- 
ggplot(mod_scen_los, aes(Scenario,Large)) + 
  geom_linerange(aes(ymin=lci_L, ymax=uci_L), colour = 'black')+
  geom_linerange(aes(y=Small, ymin=lci_S, ymax=uci_S), colour = 'red')+
  geom_line(aes(group=1))+ geom_line(aes(y=Small, group=1), colour='red', linetype=2) +
  facet_grid(Age~priorK) + 
  labs(x="Predation Scenario", y="Length of stay at departure/move")

# ggsave(paste0(".img/los_plot_", modver, ".png", sep=""), los_plot, width = 6, height = 4,
#        units='in', dpi=600,type = "cairo")

# PrintPlots --------------------------------------------------------------
PloteVerything <- F
if(exists("PloteVerything")){ 
  pdf(paste(modver,"_plots_nomove.pdf", sep=""))
  FRPlot%>% print
  ModDat%>% print
  predLOS_plot%>% print
  LoS_compare%>% print
  modvscapture%>% print
  hist_plot%>% print
  Scenario_mass%>% print
  Scenario_mass_r%>% print
  los_plot%>% print
  countsvsNumbers %>% print
  dev.off()
}

write_rds(list(catch_boot=catch_boot,ci_Mod=ci_Mod, modvscapture=modvscapture,mod_scen_los=mod_scen_los,
  FRPlot=FRPlot,Scenario_mass_r=Scenario_mass_r, LoS_compare2=LoS_compare2,los_plot=los_plot,ydenberg_countPlot=ydenberg_countPlot, Mass_plot=Mass_plot, cols_danger=cols_danger ), ".rds/bandingdata.rds")