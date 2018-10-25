library(tidyverse)
library(knitr)
library(xtable)
library(cowplot)


removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  # print(x[!x %in% stopwords])
  paste(x[!x %in% stopwords], collapse = " ")
}

stopwords = c("const", "double", "float", "int", "bool", "unsigned")

processFile = function(filepath) {
  con = file(filepath, "r")
  lnns = readLines(con)
  for (i in 1:length(lnns)){
    ln = lnns[i]
    # print(line)
    if ( ln=="" ) {
      next
    }
    if(grepl("^#", ln)){next}
    if (grepl("//", ln)){
      if (grepl("^//", ln)){
        # cat(ln, "\t", "yup", "\n")
        next
      } else{
        ln <- gsub("//.*$","", ln) 
      }
    }
      
    if(grepl("senRun", ln)){
      break
    }
    
   ln_final <- ln %>% 
          gsub("\\{","c(",. ) %>% 
          gsub("\\}",")",. )  %>% 
          gsub("\\[.+\\]", "",.) %>% 
          gsub("true;", "TRUE", .) %>% 
          gsub("false;", "FALSE", .) %>% 
     gsub("=", "<<-",.)
    ln_out <- removeWords(ln_final, stopwords)
    # print(ln_out)
    try(eval(parse(text=ln_out)),silent = T)
  }

  close(con)
}


if(!exists("nosensitivity"))
  {
    processFile("../cpp_version/Migration_Model/noU/global_ch1FinalBackup_August2018.cpp")
  } else processFile("../cpp_version/Migration_Model/noU/global.cpp")


if(!exists("nosensitivity")){
tab_import <- readODS::read_ods("../VariableSources/VariableSources.ods", sheet="Table1_Chapter")
} else{tab_import <- readODS::read_ods("../VariableSources/VariableSources.ods", sheet="Table1_Chapter2")}
tab_base <- tab_import %>% 
  mutate(Eqn = ifelse(is.na(Equation), NA, paste0("\\eqref{eq:", Equation, "}", sep ="") ),
         Parameter2 = paste0("$", Parameter, "$", sep ="") ,
         `Initial Values` = NA)

vars <- unique(tab_base$Name)
x<- NA
t <- NA
for (i in 1:length(vars)){
  tab_base$`Initial Values`[tab_base$Name==vars[i]] = paste(eval(parse(text = vars[i])), collapse = ",")
}


tab_base$`Initial Values`[tab_base$Name=="baselineflywayPredation"] <-"1"

# tab_import <- tab_import %>% mutate(`Initial Values` = ifelse(!is.na(as.numeric(`Initial Values`), 
#   as.character(round(as.numeric(`Initial Values`))))))

xtab_1 <-tab_base %>% filter(Group!="Forward Simulation") %>% 
  arrange(Order) %>% 
  mutate(`Baseline value`= as.numeric(`Initial Values`) )%>% 
  select(Parameter2, `Baseline value`, Units, Description, Eqn) %>% 
  rename(Parameter = Parameter2) %>% 
  xtable(label = "tab:variables",digits=c(0,0,3,0,0,0),  display=c("d","s", "g", "s", "s", "s"),
         caption="Model parameters associated with the dynamic programming equation for migrant passage through a landscape of stopover sites on southward migration.
         Baseline parameter values are from the original model without foraging intensities. Any modifications from these values are described in the text.") 


align(xtab_1) <- "r|r||c|c|p{9cm}|c|"
# print(xtab_1,sanitize.text.function=function(x){x},
#        floating = TRUE, floating.environment = "sidewaystable",
#        include.rownames = FALSE)


xtab_2 <-tab_base %>% filter(Group=="Forward Simulation") %>% 
  arrange(Order) %>% 
  select(Parameter2, `Initial Values`, Units, Description, Eqn) %>% 
  rename(Parameter = Parameter2, `Baseline value`= `Initial Values`) %>% 
  xtable(label = "tab:variables-forward",
         caption="Model parameters associated with forward simulation of migrant passage through a landscape of stopover sites on southward migration. Double entries are for adults and juveniles. Baseline parameter values are from the original model without prior knowledge or foraging intensities. Any modifications from these values are described in the text.") 


align(xtab_2) <- "r|r||c|c|p{9cm}|c|"

if(!exists("output")&!exists('nosensitivity')) source('../Rscripts/cpp_sensitivity.r')

if(!exists("nosensitivity"))
{
do.multirow<-function(df, which=1:ncol(df)){
    for(c in which){
        runs <- rle(as.character(df[,c]))
        if(all(runs$lengths>1)){
            tmp <- rep("", nrow(df))
            tmp[c(1, 1+head(cumsum(runs$lengths),-
1))] <-
                    paste("\\multirow{",runs$lengths,"}{*}{",df[c(1,
1+head(cumsum(runs$lengths),-1)),c],"}",sep="")
            df[,c] <- tmp
        }
    }
    return(df)
}

 
# elast <- output %>% #read.csv("../VariableSources/ElasticitywithPK_noU_medci.csv") %>% 
#   mutate(age = ifelse(grepl("\\sa$", var), "Adult", ifelse(grepl("\\sj$", var), "Juvenile", NA)),
#          Name = gsub("\\s[aj]$", "", var)) %>% left_join(select(tab_base, Name, GroupOrder, Parameter2), 
#                                                          by = "Name") %>%
#   mutate(par3=ifelse(delta==0.05, "", ifelse(delta==1, "*", "**" ))) %>% 
#   arrange(Age, GroupOrder, Name, age) %>% rename(Parameter = Parameter2) %>% 
#   select(Age, Parameter, par3, age, 4:11) %>%
#     slice(which(age==Age|is.na(age))) %>% select(-age) %>%
#   #lci0, e0 , uci0, lci1,e1, uci1) %>%  
#     group_by(Age) %>% 
#     mutate(mr_age =
#       ifelse(row_number()==1, paste("\\multirow{", group_size(.),"}{*}{",Age,"}", sep="" ) ,
#          "") ) %>% ungroup %>% filter(!is.na(Parameter)) %>%
#     mutate(Age=mr_age) %>% select(-mr_age) %>%
#   xtable(label = "tab:elasticity",
#          caption="Elasticities for variables associated with model parameters.
#          Elasticities are calculated as the relative change in counts at the
#          large or small sites on model dates July 15 (Adult) or August 15 (Juvenile). 
#          The baseline model has no foraging intensity (No U) and migrants to not have prior knowledge (No PK). 
#          Elasticities in the modified models are also shown. ") 
# names(elast) <- c( "Age","Parameter","","No PK\nLarge","No PK\nSmall", "No PK\nWith u\nLarge","No PK\nWith u\nSmall",
#                    "PK\nLarge","PK\nSmall","PK\nWith u\nLarge" ,   "PK\nWith u\nSmall" )
#   align(elast) <- paste(c("|r|r|rl|",rep("p{1.1cm}p{1.1cm}|",4)),collapse = "")
# align(elast) <- "|r|r|rl||rl|rl|rl|rl|"
# print(elast,sanitize.text.function=function(x){x},
#        floating = TRUE, floating.environment = "sidewaystable",
#        include.rownames = FALSE)
removedcol <- c("No PK\nWith u\nLarge", "No PK\nWith u\nSmall","PK\nWith u\nLarge" ,   "PK\nWith u\nSmall" )
elast_noU <- output %>% #read.csv("../VariableSources/ElasticitywithPK_noU_medci.csv") %>% 
  mutate(age = ifelse(grepl("\\sa$", var), "Adult", ifelse(grepl("\\sj$", var), "Juvenile", NA)),
         Name = gsub("\\s[aj]$", "", var)) %>% left_join(select(tab_base, Name, GroupOrder, Parameter2), 
                                                         by = "Name") %>%
  mutate(par3=ifelse(delta==0.05, "", ifelse(delta==1, "*", "**" ))) %>% 
  arrange(Age, GroupOrder, Name, age) %>% rename(Parameter = Parameter2) %>% 
  select(Age, Parameter, par3, age, 4:11) %>%
    slice(which(age==Age|is.na(age))) %>% select(-age,-removedcol)%>%
  #lci0, e0 , uci0, lci1,e1, uci1) %>%  
    group_by(Age) %>% 
    mutate(mr_age =
      ifelse(row_number()==1, paste("\\multirow{", group_size(.),"}{*}{",Age,"}", sep="" ) ,
         "") ) %>% ungroup %>% filter(!is.na(Parameter)) 

elast_tab_a <- elast_noU %>% filter(Age == "Adult") %>% select(-Age, -mr_age) #%>% xtable(label = "tab:elasticity-a",
         # caption="Elasticities for variables associated with model parameters in adults
         # Elasticities are calculated as the relative change in counts at the
         # large or small sites on model date July 15. 
         # In the baseline model migrants to not have prior knowledge (No PK). 
         # Elasticities in the modified models are also shown. ")

elast_tab_j <- elast_noU %>% filter(Age == "Juvenile") %>% select(-Age, -mr_age)# %>% 
  # xtable(label = "tab:elasticity-j",
  #        caption="Elasticities for variables associated with model parameters in juveniles.
  #        Elasticities are calculated as the relative change in counts at the
  #        large or small sites on model date August 15. 
  #        In the baseline model migrants to not have prior knowledge (No PK). 
  #        Elasticities in the modified models are also shown. ")


elast_tab <- full_join(elast_tab_a, elast_tab_j, by=c("Parameter","par3"))  %>%
    xtable(label = "tab:elasticity",
         caption="Elasticities for variables associated with model parameters.
         Elasticities are calculated as the relative change in counts at the
         large or small sites on model dates July 15 (Adult) or August 15 (Juvenile). 
         In the baseline model migrants to not have prior knowledge (No PK). 
         Elasticities in the modified models are also shown. ") 
names(elast_tab) <- c("Parameter","","\nLarge","\nSmall", 
                   "PK\nLarge","PK\nSmall","\nLarge","\nSmall", 
                   "PK\nLarge","PK\nSmall" )

  align(elast_tab) <- paste(c("|r|rl|",rep("p{1.2cm}p{1.2cm}|",4)),collapse = "")
  # align(elast_tab_j) <- paste(c("|r|rl|",rep("p{1.1cm}p{1.1cm}|",2)),collapse = "")



}