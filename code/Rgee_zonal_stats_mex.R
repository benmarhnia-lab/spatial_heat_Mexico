#------------------------------------------------------------------------------#
#------------------Heat and mortality in Mexico--------------------------------#   
#-------------------------R code 1/4-------------------------------------------#
#--------------Data Prep- Population-Weighted Temperature----------------------#
#-------------------------Update:4/15/23---------------------------------------#
#-------------------------Lara Schwarz-----------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------Check and set the directory---------------#
getwd()
setwd("D:/Lara/Border/Data")
#-------------------------------------Installing packages----------------------#
install.packages("rgee")
install.packages("geojsonio")
install.packages("reticulate")
install.packages("mapview")
install.packages("ggspatial")
rgee::ee_install()
use_python("C:/Users/lnschwar/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")
#py_install( "earthengine-api==0.1.277", "rgee")
rgee:: ee_Initialize()
rgee:: ee_check()

library(reticulate)
library(rgee)
library(viridis)
library(tidyverse)
library(sf)
library(mapview)
library(ggspatial)
library(data.table)

#-------------------------------------Data Import------------------------------#

Mexico_mun_gov <-  st_read("Mexico_shapefile/mex_admbnda_adm2_govmex_20210618/mex_admbnda_adm2_govmex_20210618.shp", quiet = TRUE)
Mexico_mun_gov<- subset(Mexico_mun_gov, select=c(ADM2_PCODE, geometry))
ee_Mex <- sf_as_ee(Mexico_mun_gov)

#--------------------Estimating population-weighted temp-----------------------#

start_time <- Sys.time()

# specify years, did this in batches because all years made it crash
years <- c( 2017, 2018, 2019, 2020)

# 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019

for (year in years){

srt_yr = year
end_yr = year

# population data 
population = ee$ImageCollection$Dataset$WorldPop_GP_100m_pop$
filter(ee$Filter$calendarRange(srt_yr,end_yr,"year"))$
        filter(ee$Filter$calendarRange(1,12,"month"))
 

pop = population$mean()

#Map$addLayer(pop, {}, 'population')

## getting maximum temperature data (i've set this to one year but would ideally like to have parallel computing for years 1998-2017)
tmax <- ee$ImageCollection("NASA/ORNL/DAYMET_V4")$
  select("tmax")$
  filter(ee$Filter$calendarRange(srt_yr,end_yr,"year"))$
  filter(ee$Filter$calendarRange(1,12, "month"))


first_image = tmax$first()
#Map$addLayer(first_image, {}, 'Temp')

# Get information about the  projection.
#PopProjection <- pop$projection()
#cat("PopProjection", PopProjection$crs()$getInfo())

TempProjection <- first_image$projection()
#cat("TempProjection", TempProjection$crs()$getInfo())

#PopresProjection <- pop_res$projection()
#cat("PopresProjection", PopresProjection$crs()$getInfo())


#Reducing grid to 1000 m scale (same as Daymet)
pop_1k=  pop$reproject(crs = TempProjection)$reduceResolution(
  reducer = ee$Reducer$sum()$unweighted(),
  maxPixels = 1024)

#Getting population-weighted temperature Image Collection
max_temp_pop= tmax$map(function(img) {return (img$multiply(pop_1k) )})

#max_temp_pop_mean=max_temp_pop$mean()

#Map$addLayer(max_temp_pop_mean, {}, 'Tmax pop')


#Loop for each municipality day
  mun_list <- list()
  for(i in 1:nrow(Mexico_mun_gov)){
      mun_ee <- Mexico_mun_gov[i,] %>% sf_as_ee()
    pet <- ee_extract(
      x = max_temp_pop,
      y = mun_ee,
      fun = ee$Reducer$sum(),
      sf = FALSE)
    mun_list[[i]] <- pet
  }
  
  
  
  mun_list_pop_1000 <- list()
  for(i in 1:nrow(Mexico_mun_gov)){
    mun_ee <- Mexico_mun_gov[i,] %>% sf_as_ee()
    pet <- ee_extract(
      x = pop_1k,
      y = mun_ee,
      fun = ee$Reducer$sum(),
      sf = FALSE)
    mun_list_pop_1000[[i]] <- pet
  }
  
  
# Getting data in dataframe format
mun_temp_wide=rbindlist(mun_list)
mun_temp_wide$year<- paste0(year)
  
mun_temp <- melt(setDT(mun_temp_wide), id.vars = c("ADM2_PCODE","year"), variable.name = "date")


 mun_pop=rbindlist(mun_list_pop_1000)
 
 mun_temp_pop <- merge(mun_temp, mun_pop, by = "ADM2_PCODE", all.x = TRUE)
 
 mun_temp_pop$pop_weighted_temp= mun_temp_pop$value/mun_temp_pop$population
 
 setwd("Mexico_temperature/Temp_munic_Mex_98_2017/tmax")
 #setwd("D:/Lara/Border/Data/Mexico_temperature/Temp_munic_Mex_98_2017/tmin")
 
 write.csv(mun_temp_pop, file=paste(year,".csv",sep=""), row.names = FALSE)
 
 
}
 
end_time <- Sys.time()

Total_time <- end_time- start_time

Total_time

## Combining temperature data for Mexico 

## maximum temperature
# set wd
setwd("Mexico_temperature/Temp_munic_Mex_98_2017/tmax")
# import files
files = list.files(pattern="*.csv")
dataset = do.call(rbind, lapply(files, fread))
rm(files)
# transform data to df
Mex_tmax <- as.data.frame(unclass(dataset))

Mex_tmax$month<-substring(Mex_tmax$date, 6, 7)
Mex_tmax$day<-substring(Mex_tmax$date, 8, 9)

Mex_tmax$date<-as.Date(with(Mex_tmax,paste(year,month,day,sep="-")),"%Y-%m-%d")

Mex_tmax<- rename(Mex_tmax, tmax_pop_weighted = pop_weighted_temp)

Mex_tmax = subset(Mex_tmax, select = c("ADM2_PCODE", "date", "population", "tmax_pop_weighted" )  )


## Minimum temperature
setwd("Mexico_temperature/Temp_munic_Mex_98_2017/tmin")
# import files
files = list.files(pattern="*.csv")
dataset = do.call(rbind, lapply(files, fread))
rm(files)
# transform data to df
Mex_tmin <- as.data.frame(unclass(dataset))

Mex_tmin$month<-substring(Mex_tmin$date, 6, 7)
Mex_tmin$day<-substring(Mex_tmin$date, 8, 9)

Mex_tmin$date<-as.Date(with(Mex_tmin,paste(year,month,day,sep="-")),"%Y-%m-%d")

Mex_tmin<- rename(Mex_tmin, tmin_pop_weighted = pop_weighted_temp)

Mex_tmin = subset(Mex_tmin, select = c("ADM2_PCODE", "date", "tmin_pop_weighted" )  )

Mex_temp_1998_2020 <- merge(Mex_tmax, Mex_tmin, by=c("date", "ADM2_PCODE"))

Mex_temp_1998_2020$year <- year(Mex_temp_1998_2020$date)
Mex_temp_1998_2020$month <- month(Mex_temp_1998_2020$date)

save(Mex_temp_1998_2020, file = "Mexico_temperature/Temp_munic_Mex_98_2017/Mex_temp_1998_2020.Rdata")



Mex_temp_1998_2020 %>%
  group_by(month) %>%
  summarise_at(vars(tmin_pop_weighted, tmax_pop_weighted), list(name = mean))

Mex_temp_1998_2020$mun<- sub("^0+", "", Mex_temp_1998_2020$mun)
Mex_temp_1998_2020$ent<- sub("^0+", "", Mex_temp_1998_2020$ent)

Mex_temp_1998_2020$GID<-paste0("MEX.", Mex_temp_1998_2020$ent, ".", Mex_temp_1998_2020$mun)


save(Mex_temp_1998_2020, file = "Mexico_temperature/Temp_munic_Mex_98_2017/Mex_temp_1998_2020.Rdata")