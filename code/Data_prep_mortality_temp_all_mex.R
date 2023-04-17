
#------------------------------------------------------------------------------#
#------------------Heat and mortality in Mexico--------------------------------#   
#------------------------R code 2/4--------------------------------------------#
#----------------------Mortality temp data prep--------------------------------#   
#---------------------Update:4/15/23-------------------------------------------#
#-------------------------Lara Schwarz-----------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------Check and set the directory---------------#
getwd()
setwd("D:/Lara/Border/Data")
#-------------------------------------Installing packages----------------------#
library(plyr)
library(ggplot2)
library(readr)
library(dplyr)
library(data.table)
library(reshape2)
library(survival)
library(haven)
library(sf)
library(tsibble)
library(skimr)
library(DataExplorer)
library(visdat)
library(tidyverse)
library(reshape2)
library(cowplot)
library(magrittr)
#-------------------------------------Data Import------------------------------#

## uploading and processing mortality data 
setwd("D:/Lara/Border/Data/Mexico_mortality/INEGI_data/1998_2020")

death_1998 = read.csv("DEFUN98.csv")
death_1999 = read.csv("DEFUN99.csv")
death_2000 = read.csv("DEFUN00.csv")
death_2001 = read.csv("DEFUN01.csv")
death_2002 = read.csv("DEFUN02.csv")
death_2003 = read.csv("DEFUN03.csv")
death_2004 = read.csv("DEFUN04.csv")
death_2005 = read.csv("DEFUN05.csv")
death_2006 = read.csv("DEFUN06.csv")
death_2007 = read.csv("DEFUN07.csv")
death_2008 = read.csv("DEFUN08.csv")
death_2009 = read.csv("DEFUN09.csv")
death_2010 = read.csv("DEFUN10.csv")
death_2011 = read.csv("DEFUN11.csv")

death_2012 = read.csv("defunciones_generales_2012.csv")
death_2013 = read.csv("defunciones_generales_2013.csv")
death_2014 = read.csv("defunciones_generales_2014.csv")
death_2015 = read.csv("defunciones_generales_2015.csv")
death_2016 = read_csv("defunciones_generales_2016.csv")
death_2017 = read_csv("defunciones_generales_2017.csv")
death_2018 = read_csv("defunciones_registradas_2018.csv")
death_2019 = read_csv("defunciones_registradas_2019.csv")
death_2020 = read_csv("defunciones_registradas_2020.csv")

head(death_2012)
head(death_2013)
head(death_2014)
head(death_2015)
head(death_2016)
head(death_2017)
head(death_2018)
head(death_2019)
head(death_2020)

names(death_1998)<-tolower(names(death_1998))
names(death_1999)<-tolower(names(death_1999))
names(death_2000)<-tolower(names(death_2000))
names(death_2001)<-tolower(names(death_2001))
names(death_2002)<-tolower(names(death_2002))
names(death_2003)<-tolower(names(death_2003))
names(death_2004)<-tolower(names(death_2004))
names(death_2005)<-tolower(names(death_2005))
names(death_2006)<-tolower(names(death_2006))
names(death_2007)<-tolower(names(death_2007))

names(death_2008)<-tolower(names(death_2008))
names(death_2009)<-tolower(names(death_2009))
names(death_2010)<-tolower(names(death_2010))
names(death_2011)<-tolower(names(death_2011))


## it is important to note the dataset for each year includes the deaths registered in that year and may include deaths that occcurred in previous years

death_1998 = subset(death_1998, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr,  causa_def  )     )
death_1999 = subset(death_1999, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr,  causa_def  )     )
death_2000 = subset(death_2000, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr,  causa_def  )     )
death_2001 = subset(death_2001, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr,  causa_def  )     )
death_2002 = subset(death_2002, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr,  causa_def  )     )
death_2003 = subset(death_2003, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr,  causa_def  )     )
death_2004 = subset(death_2004, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr,  causa_def  )     )
death_2005 = subset(death_2005, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr,  causa_def  )     )
death_2006 = subset(death_2006, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr,  causa_def  )     )
death_2007 = subset(death_2007, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr,  causa_def  )     )
death_2008 = subset(death_2008, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr,  causa_def  )     )
death_2009 = subset(death_2009, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr,  causa_def  )     )
death_2010 = subset(death_2010, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr, causa_def  )     )
death_2011 = subset(death_2011, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr, causa_def  )     )

death_2012 = subset(death_2012, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr,  causa_def  )     )
death_2013 = subset(death_2013, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr, causa_def  )     )
death_2014 = subset(death_2014, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr, causa_def  )     )
death_2015 = subset(death_2015, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr, causa_def  )     )
death_2016 = subset(death_2016, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr, causa_def  )     )
death_2017 = subset(death_2017, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr, causa_def  )     )
death_2018 = subset(death_2018, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr, causa_def  )     )
death_2019 = subset(death_2019, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr, causa_def  )     )
death_2020 = subset(death_2020, select = c(dia_ocurr, mes_ocurr , anio_ocur, ent_ocurr, mun_ocurr, causa_def  )     )



deaths_1998_2020 <- rbind(death_1998, death_1999, death_2000, death_2001, death_2002, death_2003, death_2004,
                          death_2005, death_2006, death_2007, death_2008, death_2009, death_2010, death_2011,
                          death_2012, death_2013, death_2014, death_2015, death_2016, death_2017, death_2018, 
                          death_2019, death_2020)


deaths_1998_2020$date<-as.Date(with(deaths_1998_2020,paste(anio_ocur,mes_ocurr,dia_ocurr,sep="-")),"%Y-%m-%d")

# restricting from 1998-2020
deaths_1998_2020 <- deaths_1998_2020[deaths_1998_2020$date >= as.Date("1998-01-01"),]
deaths_1998_2020 <- deaths_1998_2020[deaths_1998_2020$date <= as.Date("2020-12-31"),]

## Making a dataset with overall deaths per municipality
deaths_1998_2020$death<- 1

## Creating variable for merge with GADM data 

deaths_1998_2020$mun_ocurr<- sub("^0+", "", deaths_1998_2020$mun_ocurr)
deaths_1998_2020$ent_ocurr<- sub("^0+", "", deaths_1998_2020$ent_ocurr)

deaths_1998_2020$GID<-paste0("MEX.", deaths_1998_2020$ent_ocurr, ".", deaths_1998_2020$mun_ocurr)


save(deaths_1998_2020, file = "Mexico_mortality/INEGI_data/1998_2020/Mex_deaths_1998_2020_original.Rdata")

Mex_deaths <-deaths_1998_2020 %>%
           dplyr::group_by(GID, date) %>%
           dplyr::summarise(deaths = sum(death, na.rm = TRUE))

## creating dataset for total deaths
Mex_deaths_tot <-deaths_1998_2020 %>%
  dplyr::group_by(GID) %>%
  dplyr::summarise(deaths = sum(death, na.rm = TRUE))


## Mapping- Tried both GADM and government (govmex) Shapefiles. GADM has some issues with states being incorrect. 
Mexico_mun_gadm <-  st_read("D:/Lara/Border/Data/Mexico_shapefile/gadm40_MEX_2.shp", quiet = TRUE)
Mexico_mun <-  st_read("D:/Lara/Border/Data/Mexico_shapefile/mex_admbnda_govmex_20210618_shp/mex_admbnda_adm2_govmex_20210618.shp", quiet = TRUE)

Mexico_mun$GID_2_1<-paste0("MEX")
Mexico_mun$GID_2_2<-substring(Mexico_mun$ADM2_PCODE, 3, 4)
Mexico_mun$GID_2_3<-substring(Mexico_mun$ADM2_PCODE, 5, 7)


Mexico_mun$GID_2_2<-sub("^0+", "", Mexico_mun$GID_2_2) 
Mexico_mun$GID_2_3<-sub("^0+", "", Mexico_mun$GID_2_3) 

Mexico_mun$GID_2<- paste0(Mexico_mun$GID_2_1, ".", Mexico_mun$GID_2_2, ".", Mexico_mun$GID_2_3)

Mexico_mun_gov = subset(Mexico_mun, select = c(GID_2, ADM2_ES, ADM2_PCODE, ADM1_ES , geometry )     )
length(unique(Mexico_mun_gov$ADM2_ES)) 


## subset important variables in death datset
Mex_deaths = subset(Mex_deaths, select = c(GID , date, deaths )     )
Mex_deaths_tot = subset(Mex_deaths_tot, select = c(GID , deaths )     )


#use complete cases only
Mex_deaths_complete <- Mex_deaths[complete.cases(Mex_deaths),]
Mex_deaths_tot_complete <- Mex_deaths_tot[complete.cases(Mex_deaths_tot),]

# for daily dataset fill missing dates
Mex_death_tsbl <- as_tsibble(Mex_deaths_complete, key = GID, index=date)


Mex_deaths_complete<-Mex_death_tsbl %>%
  group_by_key(GID) %>%
  fill_gaps(date )

# changin NAs to 0 in both
Mex_deaths_complete$deaths[is.na(Mex_deaths_complete$deaths)] <- 0   # change NAs to 0
Mex_deaths_tot_complete$deaths[is.na(Mex_deaths_tot_complete$deaths)] <- 0   # change NAs to 0

# converting back to data frame
Mex_deaths_complete<- as.data.frame(Mex_deaths_complete)


## save complete dataset for future use

save(Mex_deaths_complete, file = "Mexico_mortality/INEGI_data/1998_2020/Mex_deaths_1998_2020.Rdata")


#taking daily average deaths by municipality
Mexico_deaths_avg <-Mex_deaths_complete %>%
  dplyr::group_by(GID) %>%
  dplyr::summarise(deaths = mean(deaths, na.rm = TRUE))



## Mapping overall deaths by municipality
#Mex_mun_gadm_deaths_avg <- merge(Mexico_mun_gadm, Mexico_deaths_avg ,by.x=c("GID_2"), by.y=c("GID"))
#Mex_mun_gadm_deaths_tot <- merge(Mexico_mun_gadm, Mex_deaths_tot_complete ,by.x=c("GID_2"), by.y=c("GID"))


Mex_mun_gov_deaths_avg <- merge(Mexico_mun_gov, Mexico_deaths_avg ,by.x=c("GID_2"), by.y=c("GID"))
Mex_mun_gov_deaths_tot <- merge(Mexico_mun_gov, Mex_deaths_tot_complete ,by.x=c("GID_2"), by.y=c("GID"))

Mex_mun_gov_deaths_tot_more1000 = Mex_mun_gov_deaths_tot[Mex_mun_gov_deaths_tot$deaths>1000,]
Mex_mun_gov_deaths_tot_more10000 = Mex_mun_gov_deaths_tot[Mex_mun_gov_deaths_tot$deaths>10000,]


## plotting death data
avg<-ggplot() + 
  geom_sf(data = Mex_mun_gov_deaths_avg, (aes(fill=deaths))) + 
  ggtitle("Average daily deaths") + 
  coord_sf()+
  scale_fill_viridis_c(option = "D")
avg

hist(Mex_mun_gov_deaths_avg$deaths[Mex_mun_gov_deaths_avg$deaths < 10]) 
summary(Mex_mun_gov_deaths_avg$deaths)

tot<-ggplot() + 
  geom_sf(data = Mex_mun_gov_deaths_tot, (aes(fill=deaths))) + 
  ggtitle("Total deaths") + 
  coord_sf()+
  scale_fill_viridis_c(option = "D")
tot

hist(Mex_mun_gov_deaths_tot$deaths)
summary(Mex_mun_gov_deaths_tot$deaths)



tot_over1000<-ggplot() + 
  geom_sf(data = Mex_mun_gov_deaths_tot_more1000, (aes(fill=deaths))) + 
  ggtitle("Mexico Municipalities") + 
  coord_sf()+
  scale_fill_viridis_c(option = "D")
tot_over1000

hist(Mex_mun_gov_deaths_tot_more1000$deaths)
summary(Mex_mun_gov_deaths_tot_more1000$deaths)
length(unique(Mex_mun_gov_deaths_tot_more1000$ADM2_ES)) 



tot_over10000<-ggplot() + 
  geom_sf(data = Mex_mun_gov_deaths_tot_more10000, (aes(fill=deaths))) + 
  ggtitle("Municipalities with over 10,000 deaths") + 
  coord_sf()+
  scale_fill_viridis_c(option = "D")
tot_over10000

hist(Mex_mun_gov_deaths_tot_more10000$deaths)
summary(Mex_mun_gov_deaths_tot_more10000$deaths)
length(unique(Mex_mun_gov_deaths_tot_more10000$ADM2_ES)) 
length(unique(Mex_mun_gov_deaths_tot_more10000$ADM1_ES)) 

## Merging death and temperature data 

## loading mortality data 
load(file = "D:/Lara/Border/Data/Mexico_mortality/INEGI_data/1998_2020/Mex_deaths_1998_2020.Rdata")

## loading temperature data 
load(file = "D:/Lara/Border/Data/Mexico_temperature/Temp_munic_Mex_98_2017/Mex_temp_1998_2020.Rdata")

## merging both datasets

Mex_mort_temp_1998_2020 <- merge(Mex_deaths_complete, Mex_temp_1998_2020, by=c("date", "GID"), all=TRUE)

save(Mex_mort_temp_1998_2020, file = "D:/Lara/Border/Data/Mexico_mortality/Temp_mortality_project/Mex_mort_temp_1998_2020.Rdata")

## creating heat wave variables
load(file = "D:/Lara/Border/Data/Mexico_mortality/Temp_mortality_project/Mex_mort_temp_1998_2020.Rdata")

Mex_mort_temp_1998_2020$month <- month(Mex_mort_temp_1998_2020$date)
Mex_mort_temp_1998_2020$year <- year(Mex_mort_temp_1998_2020$date)

Mex_mort_temp_1998_2020<-Mex_mort_temp_1998_2020[!is.na(Mex_mort_temp_1998_2020$tmax_pop_weighted), ]

#removing 2020 for dataset through 2019
Mex_mort_temp_1998_2019 <- Mex_mort_temp_1998_2020[Mex_mort_temp_1998_2020$date < as.Date("2020-01-01"),]

#DF<-as.data.table(Mex_mort_temp_1998_2020)
DF<-as.data.table(Mex_mort_temp_1998_2019)

## create heatwave index based on municipal specific daily maximum temperature from 1998 to 2020
DF[, quantile(tmax_pop_weighted, probs = c(0.9)), by = GID] #threshold values by municipality (90th)
DF[, quantile(tmax_pop_weighted, probs = c(0.95)), by = GID] #threshold values by municipal (95th)
DF[, quantile(tmax_pop_weighted, probs = c(0.99)), by = GID] #threshold values by municipal (99th)

DF[, quantile(tmin_pop_weighted, probs = c(0.9)), by = GID] #threshold values by municipality (90th)
DF[, quantile(tmin_pop_weighted, probs = c(0.95)), by = GID] #threshold values by municipal (95th)
DF[, quantile(tmin_pop_weighted, probs = c(0.99)), by = GID] #threshold values by municipal (99th)


## creating heat wave variables
DF[, hw_90 := as.numeric(tmax_pop_weighted >= quantile(tmax_pop_weighted, probs = 0.9)), by = GID]
DF[, hw_95 := as.numeric(tmax_pop_weighted >= quantile(tmax_pop_weighted, probs = 0.95)), by = GID]
DF[, hw_99 := as.numeric(tmax_pop_weighted >= quantile(tmax_pop_weighted, probs = 0.99)), by = GID]

DF[, hw_90_min := as.numeric(tmin_pop_weighted >= quantile(tmin_pop_weighted, probs = 0.9)), by = GID]
DF[, hw_95_min := as.numeric(tmin_pop_weighted >= quantile(tmin_pop_weighted, probs = 0.95)), by = GID]
DF[, hw_99_min := as.numeric(tmin_pop_weighted >= quantile(tmin_pop_weighted, probs = 0.99)), by = GID]

## 2 day heat wave events 

DF<-DF %>% 
  group_by(GID) %>% 
  arrange(date) %>%
  mutate(hw_90_lag1=lag(hw_90)) %>%
  mutate(hw_95_lag1=lag(hw_95)) %>%
  mutate(hw_99_lag1=lag(hw_99)) %>%
  mutate(hw_90_min_lag1=lag(hw_90_min)) %>%
  mutate(hw_95_min_lag1=lag(hw_95_min)) %>%
  mutate(hw_99_min_lag1=lag(hw_99_min)) 

DF<-DF %>% 
  mutate(hw_90_2 = case_when(hw_90 == 1 & hw_90_lag1 == 1 ~ 1, 
                             is.na(hw_90) | is.na(hw_90_lag1) ~ NA_real_, 
                             hw_90 == 0 | hw_90_lag1 == 0 ~ 0)) %>% 
  mutate(hw_95_2 = case_when(hw_95 == 1 & hw_95_lag1 == 1 ~ 1, 
                             is.na(hw_95) | is.na(hw_95_lag1) ~ NA_real_, 
                             hw_95 == 0 | hw_95_lag1 == 0 ~ 0)) %>% 
  mutate(hw_99_2 = case_when(hw_99 == 1 & hw_99_lag1 == 1 ~ 1, 
                             is.na(hw_99) | is.na(hw_99_lag1) ~ NA_real_, 
                             hw_99 == 0 | hw_99_lag1 == 0 ~ 0)) %>%
  mutate(hw_90_2_min = case_when(hw_90_min == 1 & hw_90_min_lag1 == 1 ~ 1, 
                             is.na(hw_90_min) | is.na(hw_90_min_lag1) ~ NA_real_, 
                             hw_90_min == 0 | hw_90_min_lag1 == 0 ~ 0)) %>% 
  mutate(hw_95_2_min = case_when(hw_95_min == 1 & hw_95_min_lag1 == 1 ~ 1, 
                             is.na(hw_95_min) | is.na(hw_95_min_lag1) ~ NA_real_, 
                             hw_95_min == 0 | hw_95_min_lag1 == 0 ~ 0)) %>% 
  mutate(hw_99_2_min = case_when(hw_99_min == 1 & hw_99_min_lag1 == 1 ~ 1, 
                             is.na(hw_99_min) | is.na(hw_99_min_lag1) ~ NA_real_, 
                             hw_99_min == 0 | hw_99_min_lag1 == 0 ~ 0)) 

DF<- as.data.frame(DF)

# looking at hw for each month
hw_by_month <- table(DF$month, DF$hw_95)    # Create table with groups
prop.table(hw_by_month, margin = 1)


## making NAs in death data equal to 0

DF$deaths[is.na(DF$deaths)] <- 0

save(DF, file = "D:/Lara/Border/Data/Mexico_mortality/Temp_mortality_project/Mex_mort_hw_1998_2019.Rdata")

#save(DF, file = "D:/Lara/Border/Data/Mexico_mortality/Temp_mortality_project/Mex_mort_hw_1998_2020.Rdata")

## making CVD only dataset

load(file = "D:/Lara/Border/Data/Mexico_mortality/INEGI_data/1998_2020/Mex_deaths_1998_2020_original.Rdata")

## types of hospitalizations--CHANGE ME if want to change type of hospitalizations
icds <- list(
  c(paste0("^", c(390:398, 401:405, 410:417, 420:438, 440: 448, 451:459)), 
    paste0("^I0", 0:9), 
    paste0("^I", c(10:16, 20:28, 30:52, "5A", 60:89, 95:99))
  ), ## what is "I5A"? Is it correct?
  c(paste0("^", c(460:466, 470:478, 480:487, 490:496, 500:508, 511:519)),
    paste0("^J0", 0:9),
    paste0("^J", c(10, 18, 20:22, 30:47, 60:70, 80:86, 90:99))
  )
)
allicd <- unique(deaths_1998_2020$causa_def ) ## all existing codes in dataset
codes <- lapply(icds, function(a) {
  sapply(a, function(b) {
    loc <- grep(b, allicd)
    allicd[loc]
  })
})
names(codes) <- c(
  "circulatory",
  "respiratory"
)

deaths_1998_2020 <- data.table(deaths_1998_2020)

for (i in 1:length(deaths_1998_2020 )) { ## PDD
  
  #list$missingdate[i] <- sum(is.na(Urg_2012_2019$date))
  
  deaths_1998_2020 <- deaths_1998_2020[, .N, by=.(date, ent_ocurr, mun_ocurr , causa_def)] 
}

foo <- deaths_1998_2020[, .N, by=.(date, ent_ocurr, mun_ocurr )]
foo[, N:=NULL]

for (i in 1:length(codes)) {
  temp0 <- deaths_1998_2020[causa_def %in% unlist(codes[[i]]), ]
  # temp0 <- Urg_2012_2019[AFECPRIN %in% codes[[i]], ]
  temp <- temp0[, sum(N), by=.(ent_ocurr, mun_ocurr, date)]
  foo <- foo[temp, on=.(ent_ocurr, mun_ocurr, date), V1:=V1]
  names(foo)[ncol(foo)] <- names(codes)[i]
}
foo <- foo[order(foo$date), ]
foo <- foo[order(foo$ent_ocurr), ]
summary(foo)

#foo <- foo[foo$date >= as.Date("2010-01-01"),]

#sum(foo$admtdate < as.Date("1999-01-01"), na.rm = TRUE)

##remove days without any of the coded hospitalizations and change NA to 0
for (j in names(codes))  set(foo, which(is.na(foo[[j]])),j,0)
foo <- foo[rowSums(foo[, names(codes), with=FALSE]) > 0, ]


cc.all <- foo[, .(circulatory = sum(circulatory), 
                  respiratory = sum(respiratory)), 
              by=.(ent_ocurr, mun_ocurr, date)]

Mex.all<-cc.all[complete.cases(cc.all), ]

# Creating variable for merge with GADM data 

Mex.all$mun_ocurr<- sub("^0+", "", Mex.all$mun_ocurr)
Mex.all$ent_ocurr<- sub("^0+", "", Mex.all$ent_ocurr)

Mex.all$GID<-paste0("MEX.", Mex.all$ent_ocurr, ".", Mex.all$mun_ocurr)

Mex_cvd_mort_temp_1998_2020 <- merge(Mex.all, Mex_temp_1998_2020, by=c("date", "GID"), all=TRUE)


Mex_cvd_mort_temp_1998_2020$month <- month(Mex_cvd_mort_temp_1998_2020$date)
Mex_cvd_mort_temp_1998_2020$year <- year(Mex_cvd_mort_temp_1998_2020$date)


Mex_cvd_mort_temp_1998_2020<-Mex_cvd_mort_temp_1998_2020[!is.na(Mex_cvd_mort_temp_1998_2020$tmax_pop_weighted), ]

DF_cvd<-as.data.table(Mex_cvd_mort_temp_1998_2020)



## create heatwave index based on municipal specific daily maximum temperature from 1998 to 2020
DF_cvd[, quantile(tmax_pop_weighted, probs = c(0.9)), by = GID] #threshold values by municipality (90th)
DF_cvd[, quantile(tmax_pop_weighted, probs = c(0.95)), by = GID] #threshold values by municipal (95th)
DF_cvd[, quantile(tmax_pop_weighted, probs = c(0.99)), by = GID] #threshold values by municipal (99th)

DF_cvd[, quantile(tmin_pop_weighted, probs = c(0.9)), by = GID] #threshold values by municipality (90th)
DF_cvd[, quantile(tmin_pop_weighted, probs = c(0.95)), by = GID] #threshold values by municipal (95th)
DF_cvd[, quantile(tmin_pop_weighted, probs = c(0.99)), by = GID] #threshold values by municipal (99th)


## creating heat wave variables
DF_cvd[, hw_90 := as.numeric(tmax_pop_weighted >= quantile(tmax_pop_weighted, probs = 0.9)), by = GID]
DF_cvd[, hw_95 := as.numeric(tmax_pop_weighted >= quantile(tmax_pop_weighted, probs = 0.95)), by = GID]
DF_cvd[, hw_99 := as.numeric(tmax_pop_weighted >= quantile(tmax_pop_weighted, probs = 0.99)), by = GID]

DF_cvd[, hw_90_min := as.numeric(tmin_pop_weighted >= quantile(tmin_pop_weighted, probs = 0.9)), by = GID]
DF_cvd[, hw_95_min := as.numeric(tmin_pop_weighted >= quantile(tmin_pop_weighted, probs = 0.95)), by = GID]
DF_cvd[, hw_99_min := as.numeric(tmin_pop_weighted >= quantile(tmin_pop_weighted, probs = 0.99)), by = GID]

## 2 day heat wave events 

DF_cvd<-DF_cvd %>% 
  group_by(GID) %>% 
  arrange(date) %>%
  mutate(hw_90_lag1=lag(hw_90)) %>%
  mutate(hw_95_lag1=lag(hw_95)) %>%
  mutate(hw_99_lag1=lag(hw_99)) %>%
  mutate(hw_90_min_lag1=lag(hw_90_min)) %>%
  mutate(hw_95_min_lag1=lag(hw_95_min)) %>%
  mutate(hw_99_min_lag1=lag(hw_99_min)) 

DF_cvd<-DF_cvd %>% 
  mutate(hw_90_2 = case_when(hw_90 == 1 & hw_90_lag1 == 1 ~ 1, 
                             is.na(hw_90) | is.na(hw_90_lag1) ~ NA_real_, 
                             hw_90 == 0 | hw_90_lag1 == 0 ~ 0)) %>% 
  mutate(hw_95_2 = case_when(hw_95 == 1 & hw_95_lag1 == 1 ~ 1, 
                             is.na(hw_95) | is.na(hw_95_lag1) ~ NA_real_, 
                             hw_95 == 0 | hw_95_lag1 == 0 ~ 0)) %>% 
  mutate(hw_99_2 = case_when(hw_99 == 1 & hw_99_lag1 == 1 ~ 1, 
                             is.na(hw_99) | is.na(hw_99_lag1) ~ NA_real_, 
                             hw_99 == 0 | hw_99_lag1 == 0 ~ 0)) %>%
  mutate(hw_90_2_min = case_when(hw_90_min == 1 & hw_90_min_lag1 == 1 ~ 1, 
                                 is.na(hw_90_min) | is.na(hw_90_min_lag1) ~ NA_real_, 
                                 hw_90_min == 0 | hw_90_min_lag1 == 0 ~ 0)) %>% 
  mutate(hw_95_2_min = case_when(hw_95_min == 1 & hw_95_min_lag1 == 1 ~ 1, 
                                 is.na(hw_95_min) | is.na(hw_95_min_lag1) ~ NA_real_, 
                                 hw_95_min == 0 | hw_95_min_lag1 == 0 ~ 0)) %>% 
  mutate(hw_99_2_min = case_when(hw_99_min == 1 & hw_99_min_lag1 == 1 ~ 1, 
                                 is.na(hw_99_min) | is.na(hw_99_min_lag1) ~ NA_real_, 
                                 hw_99_min == 0 | hw_99_min_lag1 == 0 ~ 0)) 

DF_cvd<- as.data.frame(DF_cvd)


DF_cvd$circulatory[is.na(DF_cvd$circulatory)] <- 0
DF_cvd$respiratory[is.na(DF_cvd$respiratory)] <- 0


save(DF_cvd, file = "D:/Lara/Border/Data/Mexico_mortality/Temp_mortality_project/Mex_cvd_mort_hw_1998_2020.Rdata")




## loading temperature data 
load(file = "D:/Lara/Border/Data/Mexico_temperature/Temp_munic_Mex_98_2017/Mex_temp_1998_2020.Rdata")

## merging both datasets

Mex_mort_temp_1998_2020 <- merge(Mex_deaths_complete, Mex_temp_1998_2020, by=c("date", "GID"), all=TRUE)

save(Mex_mort_temp_1998_2020, file = "D:/Lara/Border/Data/Mexico_mortality/Temp_mortality_project/Mex_mort_temp_1998_2020.Rdata")

## creating heat wave variables
load(file = "D:/Lara/Border/Data/Mexico_mortality/Temp_mortality_project/Mex_mort_temp_1998_2020.Rdata")

Mex_mort_temp_1998_2020$month <- month(Mex_mort_temp_1998_2020$date)
Mex_mort_temp_1998_2020$year <- year(Mex_mort_temp_1998_2020$date)





## Mapping descriptive temperature data
load(file = "D:/Lara/Border/Data/Mexico_mortality/Temp_mortality_project/Mex_mort_hw_1998_2020.Rdata")



DF2<-DF %>%
  group_by(ent, month) %>%
  arrange(ent) %>%
  summarise(tmax_mean = mean(tmax_pop_weighted), tmin_mean = mean(tmin_pop_weighted)) 

DF3<-DF %>%
  group_by(ent, month) %>%
  arrange(ent) %>%
  summarise() 

DF_names<-Mexico_mun %>%
  distinct(ADM1_ES, ADM1_PCODE) 

DF$ent<-as.numeric(DF$ent)

DF2$ent<-as.numeric(DF2$ent)

DF_names$ent<-substring(DF_names$ADM1_PCODE, 3, 4)
DF_names$ent<-as.numeric(DF_names$ent)

DF2<- merge(DF2, DF_names,by="ent", all.x = T)

names(DF2$ADM1_ES)[names(DF2$ADM1_ES) == "Veracruz"] <- "Veracruz de Ignacio de la Llave"

DF2<- DF2 %>%
  mutate(ADM1_ES = if_else(ADM1_ES == "Veracruz de Ignacio de la Llave", "Veracruz", ADM1_ES))

#my_colour <- get_colour(DF)
a <- ggplot(DF2, aes(month, tmax_mean ,  colour = ADM1_ES)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "loess") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.x = element_blank(),
        legend.position = "none") +
  #scale_color_manual(values = my_colour) +
  labs(title = "Average Maximum Temperature (°C)", subtitle = "", y = "Degrees Celsius") +
  facet_wrap(~ADM1_ES) +
  theme(strip.text.x = element_text(size = 7))+
  NULL

b <- ggplot(DF2, aes(month, tmin_mean , colour = ADM1_ES)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "loess") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") +
  #scale_color_manual(values = my_colour) +
  labs(title = "Average Minimum Temperature (°C)", subtitle = "") +
  facet_wrap(~ADM1_ES) +
  theme(strip.text.x = element_text(size = 7))+
  NULL

plot_grid(a, b)



