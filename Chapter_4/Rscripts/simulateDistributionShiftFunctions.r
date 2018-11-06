moveBird <- function(nbirds,  nsites = 1, year, dat,method = 'numbers', 
                     quantiles_d = c(0.1, 0.9)){
  
  
  low_highDanger <- 1- quantile(dat$propSafety, probs = quantiles_d)
  allLowD <- dat[which(dat$propDanger <= low_highDanger[[2]]), ]
  nsites_lowD <- min(nrow(allLowD), nsites)
  allhighD <- dat[which(dat$propDanger >= low_highDanger[[1]]), ]
  nsites_highD <- min(nrow(allhighD), nsites)
  lowD_sites <- sample_n(allLowD, 
                         nsites_lowD)[["Site.ID"]]
  highD_sites <- sample_n(allhighD, nsites_highD)[["Site.ID"]]
  if(sum(dat[which(dat$Site.ID %in% highD_sites),][["Birds"]])-nbirds <=0) {
    highD_sites <- sample_n(allhighD, nsites_highD)[["Site.ID"]]
    #highD_sites <- sample(dat[which(dat$propDanger >= low_highDanger[[2]]), ][["Site.ID"]], nsites)
    if(sum(dat[which(dat$Site.ID %in% highD_sites),][["Birds"]])-nbirds <=0) {
      # cat("You've run out of birds at dangerous sites : ", 
      #     sum(dat[which(dat$Site.ID %in% highD_sites),][["Birds"]]), " -- ", nbirds, " - ", nsites, "\n")
      return(NA)
    }}
  highD_birds <- filter(dat, Site.ID %in% highD_sites) %>% 
    mutate(propB = Birds / sum(Birds),
           birdsLost = as.integer(round(propB * nbirds,0)),
           newBirds = Birds - birdsLost) %>% 
    select(Site.ID, propB, birdsLost, Birds, newBirds)
  
  
  # print(highD_birds)
  # print(dat$Site.ID)
  # si <- highD_birds$Site.ID
  # print(highD_birds$newBirds[which(highD_birds$Site.ID == si)])
  simDat <- dat %>% rowwise() %>% 
    mutate(Birds = ifelse(Site.ID %in% lowD_sites, Birds + as.integer(round(nbirds / nsites, 0)), 
                          ifelse(Site.ID %in% highD_birds$Site.ID, #Birds - 10000,
                                 highD_birds$newBirds[highD_birds$Site.ID == Site.ID],
                                 Birds))) %>% 
    ungroup %>% mutate(
      oldTotalBirds = TotalBirdsCounted,
      TotalBirdsCounted = sum(Birds),
      propBirds = Birds / TotalBirdsCounted,
      cBirds = cumsum(Birds),
      cpropBirds = cumsum(propBirds)  )
  
  
  outdf <- 
    calculateDstat(year, simDat) %>% mutate(nbirds = nbirds, nsites_low = nsites_lowD, 
                                            nsites_high = nsites_highD,
                                            q1 = quantiles_d[1],
                                            q2 = quantiles_d[2]
    )
  return(outdf)
  
}


moveProportion <- function(proportion,  nsites = 1, year, dat,
                           quantiles_d = c(0.1, 0.9)){
  
  
  low_highDanger <- 1- quantile(dat$propSafety, probs = quantiles_d)
  allLowD <- dat[which(dat$propDanger <= low_highDanger[[2]]), ]
  nsites_lowD <- min(nrow(allLowD), nsites)
  allhighD <- dat[which(dat$propDanger >= low_highDanger[[1]]), ]
  nsites_highD <- min(nrow(allhighD), nsites, na.rm=T)
  lowD_sites <- sample_n(allLowD, 
                         nsites_lowD)[["Site.ID"]]
  highD_sites <- sample_n(allhighD, nsites_highD)[["Site.ID"]]
  highD_birds <- filter(dat, Site.ID %in% highD_sites) %>% 
    mutate(propB = Birds / sum(Birds, na.rm=T),
           propLost = proportion,
           newBirds = as.integer(round(Birds * (1-propLost), 0))) %>% 
    dplyr::select(Site.ID, propB, propLost, Birds, newBirds)
  
  Nmoving <- sum(highD_birds$Birds- highD_birds$newBirds, na.rm = T) %>% 
    round(0) %>% as.numeric
  
  # print(highD_birds)
  # print(dat$Site.ID)
  # si <- highD_birds$Site.ID
  # print(highD_birds$newBirds[which(highD_birds$Site.ID == si)])
  simDat <- dat %>% rowwise() %>% 
    mutate(Birds = ifelse(Site.ID %in% lowD_sites, Birds + 
                            as.integer(round(Nmoving / nsites_lowD,0)), 
                          ifelse(Site.ID %in% highD_birds$Site.ID, #Birds - 10000,
                                 highD_birds$newBirds[highD_birds$Site.ID == Site.ID],
                                 Birds)))%>% 
    ungroup %>% mutate(
      oldTotalBirds = TotalBirdsCounted,
      TotalBirdsCounted = sum(Birds),
      propBirds = Birds / TotalBirdsCounted,
      cBirds = cumsum(Birds),
      cpropBirds = cumsum(propBirds)  )
  
  
  outdf <- 
    calculateDstat(year, simDat) %>% mutate(proportion = proportion, nsites_low = nsites_lowD, 
                                            nbirds = Nmoving,
                                            nsites_high = nsites_highD,
                                            q1 = quantiles_d[1],
                                            q2 = quantiles_d[2]
    )
  return(outdf)
  
}

simulateCounts <- function(year, shape2, dat, returnDat =FALSE, seed = 5735779){
  set.seed(seed)
  dat.y <- filter(dat, Year == year) %>% 
    arrange(desc(propDanger)) 
  n_sites <- nrow(dat.y)
  t.birds <- unique(dat.y$TotalBirdsCounted)
  # dangers <- sort(decreasing = T, dat.y$propDanger)
  pbirds <- ( dbeta(dat.y$propDanger, 1,shape2)  / sum(dbeta(dat.y$propDanger, 1,shape2)) )
  # print(pbirds)
  # print(dat.y$propDanger)
  dat.y$propBirds <- pbirds
  dat.y$Birds <- dat.y$propBirds * t.birds
  simulatedData <- dat.y %>% rename(oldcpropBirds = cpropBirds) %>% 
    mutate(
                                    cpropBirds = cumsum(propBirds),
                                    
                                    cBirds = cumsum(Birds))
  
  auc <- calculateDstat(year, simulatedData, 'intermediate')
  if(isTRUE(returnDat))(return(list(simulatedData, auc)))
  return(auc)
}
resetPars()

pars$beta_shapes <- c(1,14)

calcValues <- function(x, pars){
  with(pars, x %>% 
    mutate(danger = map_dbl(.$area, ~calc_D(.x, T) ),
           safety = 1-danger,
           dbetaBirds = dbeta(danger, beta_shapes[1], beta_shapes[2]),
           pbetaBirds = dbetaBirds / sum(dbetaBirds),
           betaBirds  = pbetaBirds * nbirds) ) %>%  
  map_df("betaBirds", calculateSimulatedSDI, dat = .,binned = pars$binned, binsize = pars$binsize) 
}

originaldataSLR <- 
with(pars, tibble(siteid = seq(1, nsites), 
                 area = seq(rangeArea[1], rangeArea[2], length.out = nsites)) ) 

calcValues(originaldataSLR, pars)



simulateSLR <- function(originaldat,annualrate, years)
{
  calcYear <- function(originaldat,annualrate, year)
  {
    yrdat <- originaldat  %>% mutate(area = pmax(0,area  + area*year*annualrate)) %>% 
      calcValues(pars) %>% mutate(Year = year)
    return(yrdat)
  }
  df <- map_df(years, ~calcYear(originaldat,annualrate, .x))
  return(df)
}

x <- map_df(1:1000, ~simulateSLR(originaldataSLR, -0.01, 0:100))
write_rds(x,"SLR.rds")
ggplot(x[!is.na(x$aucRatio),], aes(Year, aucRatio)) + 
  stat_summary(fun.data='mean_cl_boot', geom='ribbon', colour = 'grey') +
  # geom_point(alpha=0.01, size=0.5) + ylim(0,1) +
  geom_point(data = AUC.results$AUC, colour = 'red', aes(Year-min(Year), aucRatio))

simcounts <- function(simNum, shapeStart, shapeEnd, years, dat){
  allyrs <- min(years):max(years)
  trend <- tibble(Year=allyrs, beta = seq(shapeStart,shapeEnd, length.out = length(allyrs))) %>% 
  filter(Year %in% years)

  trend.df <- map2_df(trend[["Year"]],trend[["beta"]], 
                      simulateCounts, dat = dat, returnDat = FALSE , seed=simNum) %>% 
    mutate(Yr.st = arm::rescale(Year))
  
  lm.res <- broom::augment(lm(aucRatio~Yr.st, data = trend.df), trend.df) %>% mutate(runN = simNum)
  return(lm.res)
}


run_yr <- function(year, seed=15654, data_all,quantiles_d = c(0.1, 0.9), runType = 'numbers') {
  # if(!exists("moveBird")){
  #   source('simulateDistributionShiftFunctions.r')
  # }
  set.seed(seed)
  sum_dat_2006 <- summarizeData(data_all, 1231, average = T, transform = "None",useMice = F,binIt = F,useArea = F) %>% 
  filter(Year == year)
  Nbirds <- sum_dat_2006$TotalBirdsCounted %>%  round(0) %>% unique %>% as.integer ## SHOULD BE SUM OF YEAR

  simdata <- sum_dat_2006 %>%arrange(desc(propDanger)) %>% 
    mutate(Birds = as.integer(round(Nbirds * propArea, 0)),
           propBirds = Birds / Nbirds,
           cBirds = cumsum(Birds),
           cpropBirds = cumsum(propBirds) )
                                   
# calculateDstat(year, simdata)
  if(runType == 'numbers'){
  
  # moveBird(nbirds = 1, nsites = 1, dat = simdata)
  runs <- expand.grid(10**seq(0,6, by =0.1), 150)
  res <- map2(runs[,1], runs[,2], moveBird, dat = simdata, year = year, 
              quantiles_d = quantiles_d) %>% .[!is.na(.)] %>% 
    do.call('rbind',.) %>% mutate(Year = year, seed = seed)
  }
  if(runType == 'proportion'){
   runs <- expand.grid(seq(0.0001,1, 0.01), 150)
    res <- map2(runs[,1], runs[,2], moveProportion, year = year, dat = simdata, 
              quantiles_d = quantiles_d) %>% .[!is.na(.)] %>% 
    do.call('rbind',.) %>% mutate(Year = year, seed = seed)
  }
return(res)
}