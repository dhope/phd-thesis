# Script to run logistic regression on falcon presence in the Fraser River
# Delta. 

# David Hope


library('gam')
library(dplyr)
require(ggplot2)
readkey <- function()
{
    cat ("Press [enter] to continue")
    line <- readline()
}
file_location <- '/home/dhope/Documents/SFU/PhD/Thesis_Project/FalconData/FromBarrySmith.csv' 
graphics.off()
falcons <- read.csv(file_location, sep = '\t') %>% filter(!is.na(Count.1)) %>%
				mutate(presence_f = ifelse(Count.1 == 0, "No","Yes"), 
				       presence_f = ifelse(Count.1 == 0, 0,1),
					Date = DoY -170+45) %>%
				filter(DoY > 150 & DoY < 300) %>%
				arrange(Year, DoY)


#

Tf <- 275-175

jpeg('falconModel.jpeg', width = 1280,height = 960 ,
	pointsize = 20)

# falcon_presence <- subset(falcons, DoY > 175 & DoY < 275)
# falcon_presence <- falcon_presence[!is.na(falcon_presence$Count.1),]
# falcon_presence <- falcon_presence[order(falcon_presence$DoY,falcon_presence$Year),]
# falcon_presence$Date <- (falcon_presence$DoY - 175)
with(falcons, plot(DoY, presence, pch = '|', lty = 0.5, ylim = c(0,1),
				xlim = c(150, 300), ylab = 'Probability of Falcon Detection',
				xlab = "Day of Year"
	))

# ggplot(falcons, aes(DoY, presence)) + geom_point(shape = 124) + 
# geom_smooth(method = glm, method.args = list(family = binomial))


dates <- unique(falcons$DoY)
write.csv(falcon_presence, file = 'falconpresence.csv')

presence.glm <- glm(presence_f~Date , data = falcons,
 family = binomial)

require(caret)

# define training control
train_control <- trainControl(method = 'repeatcv', repeats = 10, savePredictions = T)#method="boot", number=1000)
# train the model
model <- train(presence_f~Date , data = falcons,
 family = binomial, trControl=train_control,tuneLength = 5 , method="glm")
# summarize results
print(model)
require(boot)
boot.fun <- function(formula, data_i, i) {
          d <- data.frame(data_i[i,])
          fit <- glm(formula , data= d,
 								family = binomial)
          return(coef(fit)) 
          
        }

results <- boot(data = falcons, statistic = boot.fun, R = 3000, 
				formula = "presence~Date")

plot(results, index=1) # intercept 
plot(results, index=2)

bci.intercept <- boot.ci(results, index = 1)
bci.date <- boot.ci(results, index = 2)

lci.intercept <- bci.intercept$bca[4]
uci.intercept <- bci.intercept$bca[5]

lci.date <- bci.date$bca[4]
uci.date <- bci.date$bca[5]

print(summary(presence.glm))
presence.coef <- summary(presence.glm)$coefficients
max.prob <- max(fitted(presence.glm))
lines(falcons$DoY, fitted(presence.glm), col = 'blue')

# a.s <- c(presence.coef[1,1], presence.coef[1,1] + 1.96* presence.coef[1,2],presence.coef[1,1] - 1.96* presence.coef[1,2]) 
# b.s <- c(presence.coef[2,1], presence.coef[2,1] + 1.96* presence.coef[2,2],presence.coef[2,1] - 1.96* presence.coef[2,2]) 

a.s <- c(presence.coef[1,1], uci.intercept,lci.intercept) 
b.s <- c(presence.coef[2,1], uci.date, lci.date)

calc.prob.falc <- function(par, x){
						a <- par[1]
						b <- par[2]
						var <- 1/(1+exp(-(a + b*(x))))
						return (var)
					}

tVals <- 175:(175+Tf)
yout <-list(length = 3)
colours <- c('red', 'blue', 'blue')	

for (cycles in seq(1,3)){
	par <- c(a.s[cycles+1], b.s[cycles])
	print(par)
	y <- calc.prob.falc(par, x = (0:Tf))
	lines(tVals, y, col = colours[cycles])#, add = TRUE)
	yout[[cycles]] <- y
	}
	

verticiesTime   <- c(tVals, rev(tVals) )
verticiesy      <- c( yout[[2]], rev(yout[[3]]) )
polygon( verticiesTime, verticiesy, col = 'lightgray', border = NA )
lines(tVals, yout[[1]], col = colours[1], add = TRUE)



require(boot)
xcalc <- function(y, m){
	(logit(y) - coef(m)[["(Intercept)"]]) / coef(m)[["Date"]]
	#(-log(1/(y-1)) + c1)/c2F
	} 
xest <- xcalc(max.prob/2, presence.glm)
abline(h=max.prob/2)
abline(v=xest + 175)
print(xest + 175)

dev.off()



plot(results, index=1) # intercept 
dev.new()
plot(results, index=2)



# ############## Attempt to assess affect of snowmelt
# readkey()
# dev.new()
# require(ggplot2)
# snow <- read.csv('NiehausSnowmelt.csv', header = T, sep = '\t')
# sub.snow <- merge(falcon_presence, snow, by = "Year" )
# fit <- glm(presence~Date + Date : Snowmelt  , data = sub.snow,
#  family = binomial)

# summary(fit) # display results
# confint(fit) # 95% CI for the coefficients
# exp(coef(fit)) # exponentiated coefficients
# exp(confint(fit)) # 95% CI for exponentiated coefficients
# predict(fit, type="response") # predicted values
# residuals(fit, type="deviance") # residuals 

# #plt <- ggplot(fit,aes(x=Date + 175, y = fitted(fit), group = Snowmelt, col = Snowmelt))
# #plt + geom_line() + ylim(0,1) + geom_point() + geom_point(data = falcon_presence))
# ########## NOT WORKING!


# Skip to here ------------------------------------------------------------


require(tidyverse)
require(lubridate)
file_location <- '/home/dhope/Documents/SFU/PhD/Thesis_Project/FalconData/FromBarrySmith.csv' 
mean.snowmelt <- readRDS(".rds/meansnowmelt.rds")
vgsnow <- readRDS(".rds/vgSnowmelt.rds") %>% rename(vgSnow = mean.snowmelt)
falcons <- read_tsv(file_location) %>% 
  rename(Count.1 = Count_1) %>% 
  filter(!is.na(Count.1)) %>%
  mutate(presence = ifelse(Count.1 == 0, 0,1),
         Date = DoY -171) %>% 
  filter(DoY>=150&DoY <=300) %>% mutate(D.resc =arm::rescale(DoY)) %>% 
  group_by(Year) %>% mutate(pcumsum = CuSum / max(CuSum)) %>% ungroup %>% 
  left_join(dplyr::select(mean.snowmelt, Year, mean.snowmelt), by = "Year") %>% 
  left_join(dplyr::select(vgsnow, Year, vgSnow), by = "Year") %>% 
  mutate(snow =arm::rescale(mean.snowmelt),
         yr.st = arm::rescale(Year)) %>% group_by(Year) %>% 
  mutate(birdscounted = sum(Count.1), max.count = max(Count.1),
         firstDay = min(DoY), lastDay = max(DoY),n=n(),RangeDays = lastDay-firstDay) %>% ungroup %>% 
  filter(RangeDays >=150& Year>1993) %>% 
  mutate(date_mdy = mdy("6-20-2013") + Date) %>% 
  group_by(Year) %>% mutate(relCount = Count.1/max(Count.1,na.rm=T)) %>% ungroup


ggplot(falcons, aes(Date, relCount)) + geom_point() + geom_smooth(aes(colour = as.factor(Year)), se=F) + geom_vline(xintercept = 53)

require(lme4)
require(broom)

ggplot(falcons, aes(Date, presence, group=Year, colour = as.factor(Year))) + geom_smooth(method = glm, 
                                                                                         formula="y~x + I(x**2)",
                                                                                         method.args = list(family = binomial), se=F) + 
  geom_vline(xintercept = 53) + geom_hline(yintercept = 0.5) + coord_cartesian(xlim = c(45,60), ylim = c(0.4,0.6))


presence.glm <- glm(presence~Date , data = falcons,
                    family = binomial("logit"))

psum.glm <- glm(pcumsum~Date , data = falcons,
                    family = binomial("logit"))

falc_aug <- broom::augment(presence.glm,falcons,type.predict = "response")
falc_psum_aug <- broom::augment(psum.glm, falcons,type.predict = "response")
ggplot(falc_aug, aes(DoY, presence )) + geom_point(shape = "|") + geom_line(aes(y=.fitted)) + ylim(0,1) +
  geom_line(aes(y=.fitted),data = falc_psum_aug, colour = 'red', size = 2) + geom_point(aes(y=pcumsum), alpha = 0.25) +
  geom_vline(xintercept = c(170, 260))

falc_indivYr <- falcons %>% group_by(Year) %>% mutate(n=n()) %>% filter(n>5) %>% 
  do(augment(glm(presence~D.resc , data = .,
         family = binomial("logit")),.,type.predict = "response"))

ggplot(falc_indivYr, aes(DoY, presence, group = Year, colour = mean.snowmelt)) + geom_line(aes(y=.fitted)) + geom_point(shape = "|")+
  facet_wrap(~Year) + 
  geom_vline(data = filter(niehausdat, Year %in% falcons$Year), aes(xintercept = `Falcon index`),
             linetype=2)
ggplot(falc_indivYr, aes(DoY, presence, group = Year, colour = mean.snowmelt)) + geom_line(aes(y=.fitted)) + 
  geom_point(shape = "|") + scale_color_gradient(low = 'red', high = 'blue')

falclmer <- glmer(presence~Date + (1|Year), family=binomial("logit"), data = falcons)
falclmer2 <- glmer(presence~Date + (1+ Date|Year), family=binomial("logit"), data = falcons)
falclmer4 <- glmer(presence~D.resc + I(D.resc**2) + (1+ D.resc|Year), family=binomial("logit"), data = falcons)
falclm


bbmle::AICctab(falclmer2, falclmer4)

# confint(falclmer2, method='boot')

falcons$.fitted_glme <- predict(falclmer2, type = "response")
# lmerplt + facet_wrap(~Year) 
tidy(falclmer4)
lmerplt <- ggplot(falcons, aes(DoY, presence, group = Year, colour = mean.snowmelt)) + geom_line(aes(y=.fitted_glme)) +
  geom_point(shape = "|")+ scale_color_gradient(low = 'red', high = 'blue') + labs(colour = "Snowmelt\nDate") + 
  geom_line(data = falc_aug, aes(y = .fitted, colour =NULL, group = NULL))
lmerplt + geom_vline(xintercept = c(171,171+90))
lmerplt + facet_wrap(~Year) 

falclmer3 <- glmer(presence~Date*snow + (1|Year), family=binomial("logit"), data = falcons)
falcons$.fitted_glme3 <- predict(falclmer3, type = "response")
lmerplt3 <- ggplot(falcons, aes(DoY, presence, group = Year, colour = mean.snowmelt)) + geom_line(aes(y=.fitted_glme3)) +
  geom_point(shape = "|")+ scale_color_gradient(low = 'red', high = 'blue') + labs(colour = "Snowmelt\nDate")
lmerplt3 + facet_wrap(~Year)
sjPlot::sjp.glmer(falclmer2)
sjPlot::sjp.glmer(falclmer2, 'fe')
ggplot(falcons, aes(mean.snowmelt, DoY, colour = .fitted_glme3)) + geom_point()+ scale_color_gradient(low = 'red', high = 'blue') 

bbmle::AICctab(falclmer, falclmer2, falclmer3, presence.glm, weights = T)

str(falcons)

falcons.aug <- broom::augment(falclmer2, falcons, type.predict = "response")
falcons.aug$.inv <- logit(falcons.aug$presence)
ggplot(falcons.aug, aes(date_mdy, .fitted)) + geom_line(aes(group=Year)) + geom_point(aes(y=.inv))

logit <- function(x) qlogis(x)
inverse_logit <- function(x) plogis(x)


broom::augment(falclmer4, falcons) %>% 
  ggplot(aes(date_mdy, inverse_logit(.fitted), group = Year)) + geom_line() +
  geom_line(data=falcons.aug, linetype=2)+
  geom_point(aes(y=presence), shape = "|", position = position_jitter(height = 0.05, width = 0))



falclmer2 %>% tidy(conf.int=T)
sjPlot::sjp.lmer(falclmer2, type = 'fe.resid')

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}


newobs <-select(falcons, DoY, Date, date_mdy) %>% distinct()
newobs$.predict <- predict(falclmer2, newdata = newobs, re.form = NA, type = 'response')# logit2prob(mm%*%fixef(m))#

m  <- falclmer2
mm<-model.matrix(~Date,newobs)
predFun<-function(.) logit2prob(mm%*%fixef(.))
bb<-bootMer(m,FUN=predFun,nsim=200)
bb_se<-apply(bb$t,2,function(x) x[order(x)])
newobs$blo<-bb_se[1,]#*.95
newobs$bhi<-bb_se[200,]  
  


fiftydays <- c(226,
               211,
               232,
               208,
               223,
               226,
               223,
               217,
               
               226,
               223,
               175,
               220,
               229
)

niehausdat <- readxl::read_excel("/home/dhope/Documents/SFU/MSc/Sandpiper Project/Amanda Niehaus stuff/phenology summary for Ron.xls")


predF <- function(x, pred_max, pred_time_shape) 1./ (1 + exp(- ( pred_max + pred_time_shape * x)))


pred_max <- -4.56998353; pred_time_shape <- 0.04992743
newobs$lci <-   predF(newobs$Date, pred_max, pred_time_shape )
pred_max <- -2.69245060; pred_time_shape <- 0.08584379
newobs$uci <-   predF(newobs$Date, pred_max, pred_time_shape )
pred_max <- tidy; pred_time_shape <- 0.08584379
newobs$halfmu <- predF(newobs$Date,
                       pred_max = tidy(falclmer2)[1,2], 
                       pred_time_shape = tidy(falclmer2)[2,2]*.5
                        )
newobs$oldver <- predF(newobs$Date, pred_max = -3.611108,
                       pred_time_shape =0.066233 )
  
  require(cowplot)
fullplot <- 
ggplot(newobs, aes(date_mdy, .predict))  + 
  geom_ribbon(aes(ymin=blo, ymax=bhi), colour = 'grey', alpha =0.5) +
  geom_point(data = falcons, aes(y=presence), shape = "|") +
  geom_line(data =  falcons.aug, aes(y=logit2prob(.fitted), group = Year),
            alpha = 0.3)+ geom_line() + 
  geom_line(aes(y=halfmu), linetype=2)+
  geom_line(aes(y=oldver), linetype=1,colour='red')+
  # geom_line(aes(y=uci), colour = 'red')+
  # geom_line(aes(y=lci), colour = 'blue') +
  geom_vline(xintercept = c(mdy("6-20-2013"), mdy("6-20-2013") + 90)) +
  labs(y="Probability of Predator Presence",x =""
       )

require(tidyverse)
write_rds(list("newobs"=newobs,"falcons"= falcons,"falcons.aug"= falcons.aug, "logit2prob"= logit2prob), "../knitr_results/.rds_files/falcon.rds")
fullplot + facet_wrap(~Year) + 
  geom_vline(data = filter(niehausdat, Year %in% falcons$Year), aes(xintercept = as_date(`Falcon index`,origin= "2013-01-01" )),
             linetype=2)

ggsave(filename = "../ThesisChapter/figures/FalconPresence.png", fullplot)

confint(falclmer2)
fullplot + geom_vline(xintercept = as_date(fiftydays,origin= "2013-01-01" ), linetype=2)


pred_max <- -3.611108; pred_time_shape <- 0.066233;
plot(0:120, predF(0:120, pred_max, pred_time_shape), type='l', ylim = c(0,1), ylab="Probability of Predator Presence",
     xlab="Days after DoY June 20")
pred_max <- -3.611108+0.89807; pred_time_shape <- 0.066233;
lines(0:90, predF(0:90, pred_max, pred_time_shape), type='l', col = 'red')
pred_max <- -3.611108-0.89807; pred_time_shape <- 0.066233;
lines(0:90, predF(0:90, pred_max, pred_time_shape), type='l', col = 'blue')
pred_max <- -3.611108; pred_time_shape <- 0.066233+0.01748;
lines(0:90, predF(0:90, pred_max, pred_time_shape), type='l', col = 'green')
pred_max <- -3.611108; pred_time_shape <- 0.066233-0.01748;
lines(0:90, predF(0:90, pred_max, pred_time_shape), type='l', col = 'purple')
pred_max <- -3.611108+0.89807; pred_time_shape <- 0.066233+0.01748;
lines(0:90, predF(0:90, pred_max, pred_time_shape), type='l', col = 'red', lty=2)
pred_max <- -3.611108-0.89807; pred_time_shape <- 0.066233-0.01748;
lines(0:90, predF(0:90, pred_max, pred_time_shape), type='l', col = 'blue', lty=2)
pred_max <- -3.611108; pred_time_shape <- 0.066233*.5;
lines(0:120, predF(0:120, pred_max, pred_time_shape), type='l', col = 'orange', lty=1)
lmerplt + xlim (170,260)

ggplot(falcons, aes(mean.snowmelt, DoY, colour = pcumsum)) + 
  geom_raster(aes(z=pcumsum, fill = pcumsum))+
  # geom_point()+ 
  scale_fill_gradient(low = 'red', high = 'blue') 
ggplot(falcons, aes(DoY, pcumsum, group = Year, colour = mean.snowmelt)) + geom_line()+ scale_color_gradient(low = 'red', high = 'blue') 



hawkYr50 <- falcons %>% group_by(Year) %>% 
  summarize(fiftyDay = DoY[which.min(abs(pcumsum - 0.5))],
            predfiftyDay = DoY[which.min(abs(.fitted_glme2 - 0.5))],
            nextDay = DoY[which.min(abs(pcumsum - 0.5))+1],
            prevDay = DoY[which.min(abs(pcumsum - 0.5))-1],
            prop50 = pcumsum[which(DoY == fiftyDay)],
            varDays = var(c(fiftyDay, nextDay, prevDay)),
            dev50 = prop50 - 0.5,
            ndays = n(),
            nbirds = sum(Count.1),
            meanCount = mean(Count.1)) %>% ungroup %>% 
  left_join(dplyr::select(mean.snowmelt, mean.snowmelt, Year), by = "Year")

hawkYr50  %>% 
  ggplot(aes(mean.snowmelt, fiftyDay, colour = dev50)) + geom_point()+ geom_smooth(method='lm',se=F) + 
  geom_point(shape= 2, aes(y=predfiftyDay)) + geom_smooth(method='lm', aes(y=predfiftyDay), se=F)


falcons_with_na <- falcons %>% select(DoY, Year, presence) %>% spread(DoY, presence) %>% 
  gather(key = "DoY", value = 'presence', -1)
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(falcons_with_na,2,pMiss)
library(mice)
tempData <- mice(falcons_with_na,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
completedData <- complete(tempData,1)
ggplot(completedData, aes(DoY, presence, group = Year)) + 
geom_point(shape = "|") + scale_color_gradient(low = 'red', high = 'blue') + facet_wrap(~Year)
