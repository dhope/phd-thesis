readMCLoopDat <- function(file_,  pk, u,returnFull=F){
		if(!exists("dataimport")){source("../Rscripts/model_data_import.r")}
		require(lubridate)
		cedar_noPriorK <- read_tsv(file_, col_names = F)
		headers <- read_tsv("../cpp_version/Migration_Model/OrganizedResults/header.txt", col_names = F) %>% .[1,]
		if(length(names(cedar_noPriorK))==6){
			names(cedar_noPriorK) <- headers[-7]
		} else names(cedar_noPriorK) <- headers
		# write_rds(cedar_noPriorK, "../Rscripts/.rds_files/cedar_noPriorK.rds")
		# cedar_noPriorK <- read_rds("../Rscripts/.rds_files/cedar_noPriorK.rds")

		mcLoop <- dataimport(input_data = cedar_noPriorK, dat_type = 'loop', 'nl')

		# str(mcLoop)
		if(isTRUE(returnFull)) {return(mcLoop)} 

		# filt_dat <- filter(mcLoop, time %in% c(25, 56) & (danger == 0.24  | food == 0.5)) %>% 
		#         filter(danger > 0.)
		write_rds(mcLoop,paste0("../Model_Description_Chapter/.rds_files/cedar_", u, "u.rds", sep =""))
	# return(filt_dat)
	}


filt_mcdat <- function(dat, danger_=4, food_ =2){
	filter(dat, time %in% c(25, 56) & (danger == danger_  | food == food_)) %>% 
		         filter(danger > 0.) %>% return
}

# ggplot(filt_dat_pk %>% filter(pk==1& food %in% c(2) & time%%2==0& (time >10 | time>70)), aes(danger, mean0, colour = food, group = food)) + geom_line() + facet_grid(date_f~food) +theme( axis.line = element_blank(), legend.position = 'none', axis.text = element_blank(), axis.ticks = element_blank(), strip.text = element_blank(), axis.text.x = element_blank()) + labs(x="", y="")

ann_line<-data.frame(
	ymin=c(0.25, 0.5),ymax=c(.75, 0.5),x=c(7,7),
	xmax = c(7,8),
    food=c(3,3), time=c(56,52))

ann_text <- data.frame(
	x=c(9,7.5),y=c(.5,.25),
	food=c(3,3),time=c(56,52),
	label=c("+0.5","+1"))

# filt_dat_pk %>%
#  filter(pk==1& time%%2==0& 
#  	(time >10 | time<70))

tuftepplot <- function(dat){
	dat %>%
	ggplot(aes(danger, p_at_Large, 
 	colour = food)) + 
	geom_line() + facet_grid(time~food) +
	theme( axis.line = element_blank(), 
		legend.position = 'none',
		 axis.text = element_blank(),
		  axis.ticks = element_blank(),
		   strip.text.y = element_blank(),
		    axis.text.x = element_blank(),
		    strip.text.x=element_blank()) + 
	labs(x="", y="") +
	 geom_segment(colour = "red", 
	 	data=ann_line,
	 	aes(x=x,xend=xmax,y=ymin,yend=ymax)) +
	 geom_text(data=ann_text,aes(x=x,y=y,label=label),size=1) 
	}

# ad_plot <- filt_dat_pk %>%
#  filter(pk==1& time%%4==0& 
#  	(time >15 | time<40)) %>% tuftepplot

# juv_plot <- filt_dat_pk %>%
#  filter(pk==1& time%%4==0& 
#  	(time >=45 | time<70)) %>% tuftepplot + 
#  	theme(strip.text.x=element_blank())

#  plot_grid(ad_plot, juv_plot, nrow = 2)