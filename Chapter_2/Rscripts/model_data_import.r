## Function to clean data

# 1. Is it single run, monte carlo, or MC Loop?
# 2. Do the migrants have prior knowledge?
# 3. Is it forward or backwards data?
# 4. Summarize the data?

if(!exists("dirloc")) dirloc <- "../cpp_version/Migration_Model/.results/"

coltypes = list('back' = paste0(dirloc, "dsvm_back-spec.rds"),
	'forward'=paste0(dirloc, "dsvm_forward-spec.rds") )

# cols_danger <- c("Small" = "#F8766D", "Large" = "#00BA38",
#                  "Depart" ="#619CFF" )#

cols_danger <- c("Small" = "Red", #"#F8766D", 
                 "Large" = "Black",#"#00BA38",
                 "Depart" ="#619CFF" ,
                 "Medium" = "Purple")#

                 
# Variables
dat_type = 'back' # OR 'forward' or "MC" OR "MCLOOP" 
dataimport <- function(input_data, dat_type, pk){
	suppressPackageStartupMessages(require(tidyverse))
	require(lubridate)
	if(dat_type != 'loop'){
	dat <- read_tsv(input_data,
				col_types = read_rds(coltypes[[dat_type]]))
	}
	if(dat_type =='back'){
		if(max(dat$s)==2){
		dat_out <- dat  %>% filter(s!=max(s)) %>% #& `d(s,t,x)`!=-9
						mutate(`d(s,t,x)`=replace(`d(s,t,x)`, which(`d(s,t,x)`<0L), NA),
						DecName = factor(ifelse(`d(s,t,x)` == 0, "Small", ifelse(`d(s,t,x)`==1, "Large", "Depart")),
	                            levels=c("Small", "Large", "Depart")),
	           			SiteName = factor(ifelse(s == 0, "Small", ifelse(s==1, "Large", "Depart")),
	                             levels=c("Small", "Large", "Depart")))
	} else{
		dat_out <- dat  %>% filter(s!=max(s)& `d(s,t,x)`!=-9) %>% 
	    mutate(DecName = factor(ifelse(`d(s,t,x)` == 0, "Small", 
	    	ifelse(`d(s,t,x)`==1, "Medium",
	    		ifelse(`d(s,t,x)`==2, "Large","Depart")) ),
	                            levels=c("Small","Medium", "Large", "Depart")),
	           SiteName = factor(ifelse(s == 0, "Small", 
	           	ifelse(s==1, "Medium",
	    		ifelse(s==2, "Large","Depart")) ),
	                            levels=c("Small","Medium", "Large", "Depart"))) 
	}
	}
	if(dat_type == 'forward'){
		max_site <- max(dat$s)
		dat_out <- dat %>%
		arrange(id, t) %>% 
		  mutate(age = ifelse(Age == 0, "Adult", "Juvenile"),
		         MaxLoS = ifelse(s == lead(s)| id != lead(id), NA, LoS)) %>% 
		  rowwise %>% 
		  mutate(x = ifelse(s == 0, rnorm(1,1,0.05), ifelse(s==1, rnorm(1,3,0.05),rnorm(1, 2, 0.1))),
		         y = ifelse(s == 2, rnorm(1,1,0.05), rnorm(1,2,0.05))) %>% ungroup %>% 
		  ungroup %>% arrange(id, t) %>%
		  mutate(SiteName = factor(ifelse(s == 0, "Small", 
		  	ifelse(s==1, ifelse(max_site==2, "Large", "Medium"), 
		  	ifelse(s==2, ifelse(max_site==2, "Depart", "Large"),"Depart"))),
		                           levels=
		                           	c("Small","Medium", "Large", "Depart")),
		  		 priorK = pk,
		  		 date = mdy("06-20-17") + t,
		  		 fat = fuel*22.7, mass=fat+22.7) %>% group_by(priorK, id) %>% 
		  arrange(t) %>% mutate(deltaf = fuel -lag(fuel),
	                        p_depart = exp(-1/LoS),
	                        pres=ifelse(is.na(MaxLoS)&s!=max_site,1,0 ),
	                        week = t %/% 7) %>% ungroup

	}
	if (dat_type == 'loop'){
		dat_out <- input_data %>% 
				mutate(TotalBirds = site1+site0,
				       p_at_Large = ifelse(TotalBirds >0,site1/TotalBirds, NA)) %>% 
				  group_by(pk,danger, food, time ) %>%
				  # group_by(pk,danger, food, time) %>% 
				  summarize(mean0= mean(site0, na.rm=T),
				            med0 = median(site0, na.rm=T),
				            mean1= mean(site1, na.rm=T),
				            med1 = median(site1, na.rm=T),
				            q25_0 = quantile(site0, 0.025, na.rm=T),
				            q975_0 = quantile(site0, 0.975, na.rm=T),
				            q25_1 = quantile(site1, 0.025, na.rm=T),
				            q975_1 = quantile(site1, 0.975, na.rm=T),
				            medp = median(p_at_Large,na.rm=T),
				            meanp = mean(p_at_Large, na.rm=T),
				            q25_p = quantile(p_at_Large,0.025, na.rm=T),
				            q975_p = quantile(p_at_Large,0.975, na.rm=T),
				            meanTotal = mean(TotalBirds,na.rm=T)) %>% ungroup %>% 
				  mutate(
				    age = ifelse(time <=30, "Adult", "Juvenile"),
				    date = mdy("06/18/2015") + time,
				    date_lab = paste0(month.abb[month(date)]," ",  mday(date))) %>%
				  arrange(time) %>% mutate(date_f = factor(date_lab, levels=unique(date_lab)),
				                           index = food/danger)

		dat_out$p_at_Large <- dat_out$med1 / (dat_out$med1 + dat_out$med0)

	}
	return(dat_out)
}

getmaxdate <- function(a, s, dat){
  out <- dat %>% 
  filter_(ifelse(a==0," time<42", "time>=42")) %>% 
  group_by(priorK, run) %>% 
    rename_(site = paste(s)) %>% 
    mutate(siteN = paste(s)) %>% 
  slice(which.max(site)) %>%  ungroup %>% 
  mutate(date = mdy("06-20-17") + time) %>% 
    select(run, time, date, priorK, siteN, site) %>% 
    mutate(age = paste(a))
  return(out)
}

pk_all = list("t" = "Prior Knowledge", 'f' = "No Prior Knowledge")
# back1 <- dataimport(
# 	input_data = "../cpp_version/Migration_Model/OrganizedResults/noU/noKnowledge/full_back_single.txt",
#  dat_type = 'back', pk= pk_all[["t"]])

# for1 <- dataimport(
# 	input_data = '../cpp_version/Migration_Model/OrganizedResults/noU/noKnowledge/forwards_sim.txt',
#  dat_type = 'forward', pk= pk_all[["f"]])

# print(back1)

# head(for1)


# Plot function

plotres <- function(dat, plot_type, t=25,sesa=F){
	require(cowplot)
	require(ggplot2)
	require(dplyr)
	plotOUt <- "cat"
	if(plot_type == 'back'){
	  maxF <- max(dat$fuel)
	  if(isTRUE(sesa)) maxF  <- max(dat$fuel)/2
		plotOUt <- ggplot(dat , aes(mdy("6-20-2013")+t, fuel/maxF, z=(DecName))) + #geom_contour( aes(colour=..level..)) +
		  geom_raster(aes(fill=as.factor(DecName)), interpolate = F)+
		  scale_fill_manual(values = cols_danger) + 
		  facet_wrap(~SiteName) + theme(legend.position = 'none') +
		  coord_cartesian(xlim = c(mdy("6-20-2013")+0,mdy("6-20-2013")+90)) +
		    labs(x = "Date" , y = "Relative Fuel Load") 
	}
	if(plot_type == 'u'){
	  maxF <- max(dat$fuel)
	  if(isTRUE(sesa)) maxF  <- max(dat$fuel)/2
		plotOUt <- ggplot(dat %>% 
			mutate(u=replace(u, which(u<0L), NA)) #%>%filter(u>=0)
			 , aes(t, fuel/maxF, z=(u))) + #geom_contour( aes(colour=..level..)) +
		  geom_raster(aes(fill=u), interpolate = F)+
		  scale_fill_gradient()+ 
		  facet_wrap(~SiteName) + theme(legend.position = 'bottom') +
		    labs(x = "Date" , y = "Relative Fuel Load") 
	}
	if(plot_type=='fitness'){
	  maxF <- max(dat$fuel)
	  
	  plotOUt <- ggplot(dat %>% filter(`F(s,t,x)`!=0), aes(t, fuel/maxF,z=`F(s,t,x)`)) + 
	    geom_raster(aes(fill=`F(s,t,x)`), interpolate=F) +
	    scale_fill_gradient()+
	    facet_wrap(~SiteName) +# theme(legend.position = 'none') +
	    labs(x = "Date" , y = "Relative Fuel Load")
	}
	
	if (plot_type == 'hist'){
		plotOUt <- dat %>% filter(s!=max(s)) %>% 
		  ggplot(aes(date)) +  #facet_wrap(priorK~SiteName) +
		  # facet_wrap(~SiteName)+
		  geom_density(stat='count', #alpha=0.2,
		               aes(
		                 group=interaction(age, SiteName),
		    linetype=age,colour=SiteName))+ labs(x= "Date", y="Daily\nAbundance") +
		  theme(legend.position='none') +
		  scale_colour_manual(values = cols_danger) + #c("Small"="Red", "Large" = "Black"))  +
		  # scale_fill_manual(values = list("0"="Orange", "1" = "Sky blue")) +
		   scale_x_date(date_breaks = "1 month", date_labels="%b")+ scale_linetype_manual(values=c("solid", "dotted"))
		  
	}
	if (plot_type == 'mass'){
		plotOUt <- dat %>%
		filter(s!=max(s)) %>% 
		 ggplot(aes(date, fuel*22.7+22.7, colour=SiteName, linetype = age)) + 
		  stat_summary(alpha=0.5, fill='grey',colour='grey',
		               aes(group=interaction(SiteName,age)),
		               # position=position_dodge(width = 0.2),
		               fun.data = 'mean_cl_boot',
		               geom='ribbon')  +
		  stat_summary(fun.y='mean',
		               geom='line') +
		  labs(x="Date", y="\nMass (g)", colour = "Site", shape = "Age")+ 
		  scale_colour_manual(values = cols_danger) + #c("Small"="Red", "Large" = "Black"))  +
		   # facet_wrap(~SiteName)+
		   # scale_colour_manual(values = c("Adult"="Orange", "Juvenile" = "Sky blue"))  +
		   scale_x_date(date_breaks = "1 month", date_labels="%b")+ scale_linetype_manual(values=c("solid", "dotted"))
}

	if (plot_type == 'los'){
		plotOUt<- dat %>%
		filter( !is.na(MaxLoS)) %>% 
		ggplot(aes(date, MaxLoS+1,colour=SiteName, linetype = age)) + 
		# stat_summary(position=position_dodge(width = 0.2),fun.data = 'mean_cl_boot') +
		  stat_summary(alpha=0.5, fill='grey',colour='grey',
		               aes(group=interaction(SiteName,age)),
		               # position=position_dodge(width = 0.2),
		               fun.data = 'mean_cl_boot',
		               geom='ribbon')  +
		  stat_summary(fun.y='mean',
		               geom='line') +
		  scale_colour_manual(values = cols_danger) + #c("Small"="Red", "Large" = "Black"))  +
		  labs(x = "Date", y = "Lengths\nof stay (days)", 
			shape = "Age", colour = "Site") + 
		# facet_wrap(~SiteName) +
		# scale_colour_manual(values = c("Adult"="Orange", "Juvenile" = "Sky blue"))  +
		    scale_x_date(date_breaks = "1 month", date_labels="%b")+ scale_linetype_manual(values=c("solid", "dotted"))
	}

	if (plot_type == 'intensity'){
		plotOUt<- dat %>%
			filter(s!=max(s)) %>% 
			 ggplot(aes(date, u, colour=SiteName, linetype = age)) + 
		  stat_summary(alpha=0.5, fill='grey',colour='grey',
		               aes(group=interaction(SiteName,age)),
		               # position=position_dodge(width = 0.2),
		               fun.data = 'mean_cl_boot',
		               geom='ribbon')  +
		  stat_summary(fun.y='mean',
		               geom='line') +
		  scale_colour_manual(values = c("Small"="Red", "Large" = "Black"))  +
			 # stat_summary(position=position_dodge(width = 0.2),
			 # 	fun.data = 'mean_cl_boot')  +
			  labs(x="Date", y="Foraging\nIntensity", colour = "Age")+ 
			   # facet_wrap(~SiteName)+
			   # scale_colour_manual(values = c("Adult"="Orange", "Juvenile" = "Sky blue")) +
			   ylim(0,1) +
		    scale_x_date(date_breaks = "1 month", date_labels="%b")
	}

	if (plot_type=='mcfacet'){
		plotOUt<-dat %>% filter(time %% 3==0 & time < 70 & time > 10&danger<7 &(food==1|food%%2==0&food<7)) %>% #food>=0.5&
				  ggplot( aes(danger, medp, group = as.factor(food))) +
				  geom_ribbon(aes(ymax = q975_p, ymin=q25_p), colour = 'grey', alpha =0.25)+
				  geom_line(aes(colour = as.factor(floor(food)))) + facet_wrap(~date_f, nrow=2)+
				  theme(axis.text.x = element_text(angle=45, hjust = 1))+scale_colour_brewer(type='seq', palette="Set1")+
				  labs(x="Relative danger of Large site", y = "Proportion of Birds at Large Site", colour = "Relative Food\nat Large Site")
	}

	if(plot_type=='mc_ex'){
		

		plotOUt<- dat %>% filter(food>=2& time==t) %>% #mutate(pk = "No Prior Knowledge") %>% 
				  # bind_rows(cedar_sumPriorK %>% filter(food>=0.5& time==25)%>% mutate(pk = "Prior Knowledge")) %>% 
				  ggplot( aes(danger, medp, group = as.factor(food), frame = time)) +
				  geom_ribbon(aes(ymax = q975_p, ymin=q25_p), colour = 'grey', alpha =0.25)+
				  geom_line(aes(colour = as.factor(floor(food)))) + #facet_grid(~pk)+
				  labs(x="Relative danger of large site", y = "Proportion of birds at large site", colour = "Relative Food\nat Large Site")
	}
	if(plot_type=='mc_ex_together'){
		plotOUt<-ggplot(dat, aes(x=danger,group = as.factor(food))) +
        geom_ribbon(aes(ymax = q975_p, ymin=q25_p), fill = 'grey', alpha =0.75) +
        geom_line(aes(y=medp, colour = as.factor(floor(food)))) + facet_grid(month(date, label = T,abbr=F)~.)+
        ylim(0,1)+ labs(x = "Danger", y="Proportion at Large Site", colour="Food") + coord_cartesian(xlim=c(1,6)) +scale_colour_brewer(type='seq', palette="PuBu")
	}


 
return(plotOUt)

}


plotProp <- function(x, type ='danger',var=2){
	if(type=='danger'){
		xf <- x %>% filter(food == var) 
		max_c <- max(c(max(xf$q975_1), max(xf$q975_0)))
  	plt <-
	  xf %>%
	  ggplot(
	         aes(x=danger))
	  }
	  if(type=='food'){
		xf <- x %>% filter(danger == var) 
		max_c <- max(c(max(xf$q975_1), max(xf$q975_0)))
	  	plt <-
		  xf %>%
		  ggplot(aes(x=food)) 
		
	  }
	  plt_out <- plt + geom_ribbon(aes(ymax = q975_0, ymin=q25_0), fill = 'grey', alpha =0.25) +
		  geom_ribbon(aes(ymax = q975_1, ymin=q25_1), fill = 'grey', alpha =0.25) +
		  geom_ribbon(aes(ymax = q975_p*max_c, ymin=q25_p*max_c), colour = 'grey', alpha =0.25) +
		   geom_line(aes(y= mean0), linetype=2, colour = cols_danger[['Small']]) +
		  geom_line(aes(y=mean1),colour = cols_danger[['Large']], linetype=2) +
		  geom_line(aes(y=p_at_Large*max_c), colour='#33FF00') + facet_wrap(~age)+
		  scale_y_continuous(sec.axis = sec_axis(~./max_c,name = "Proportion of Birds at Large Site")) +
		    labs(x = paste("Relative", type, "of small site"), y="Number of Birds", ec.axis = "Cats")

	  return(plt_out) 
}


countPlot <- function(x){
  x %>% 
  ggplot( 
         aes(Date, count,  colour = scenario_f)) + 
    geom_ribbon(aes(ymin= lci, ymax=uci, group = scenario_f), colour = 'grey', alpha = 0.5)+
    geom_line() + facet_wrap(Age~s, scales='free_x')+ 
    labs(y="Number of Birds", colour = "Scenario")
}


draw.plot <- function(dat_f,angles)
{
  for(t in 1:length(unique(dat_f$time))){
  for(i in 1:length(angles))
  {
    dat <- dat_f %>% filter(time==unique(dat_f$time)[[t]] &
                                           !is.na(meanp))
    
    if(nrow(dat)<2){next}
    
    return(wireframe(data = dat,
                    meanp~danger*food, shade=T, pretty=T ,distance=0,
                    screen=list(z=angles[i],x=-60),main=paste0("Time: ", unique(dat_f$time)[[t]]),
                    zlab="",scales=list(draw=T)))
    }
    # setTxtProgressBar(txtProgressBar(style=3),(length(angles)*(t-1)+i)/(length(angles)*length(unique(dat_f$time))))
    }
}