
#------------------------------------------------------------------------------#
#-------------------------Heat and mortality in Mexico-------------------------#   
#-------------------------------R code 3/4-------------------------------------#
#-------------------------------Mexico Census data ----------------------------#   
#-----------------------------Update:4/15/23-----------------------------------#
#-----------------------------Lara Schwarz-------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------Check and set the directory---------------#
getwd()
setwd("D:/Lara/Border/Data/Mexico_social/00")
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
library(maptools)
library(foreign)

## Mapping
Mexico_mun_census <-  st_read("municipal.shp", quiet = TRUE)
plot(Mexico_mun_census$geometry)

# There are 2456 municpalities in Mexico according to this shapefile
length(unique(Mexico_mun_census$CVEGEO))

# uploading census data
census_economy<- read.dbf("tablas/cpv2010_municipal_caracteristicas_economicas.dbf")
census_economy<-census_economy[-c(92:99)]
census_education<- read.dbf("tablas/cpv2010_municipal_caracteristicas_educativas.dbf")
census_education<-census_education[-c(101:108)]

census_social<- read.dbf("tablas/cpv2010_municipal_desarrollo_social.dbf")
census_health<- read.dbf("tablas/cpv2010_municipal_servicios_de_salud.dbf")
census_health<-census_health[-c(14:21)]

census_housing<- read.dbf("tablas/cpv2010_municipal_viviendas.dbf")
census_housing<-census_housing[-c(82:89)]

census_all<- cbind( census_economy, census_education, census_social, census_health, census_housing )

census_all= as.data.frame(census_all)

census_all$GEO <- as.character(census_all$CVEGEO)

#dictionary to pick variables of interest 
census_dictionary<- read.dbf("descriptores/desc_cpv2010.DBF")

## joining census data with shapefile
Mex <- merge(Mexico_mun_census, census_all, by.x="CVEGEO", by.y="GEO")


## creating census CSV file for meta-regression (11/15/2022)
Mex$ent<- substring(Mex$CVEGEO, 1, 2)
Mex$mun<- substring(Mex$CVEGEO, 3, 6)

##removing leading 0s
Mex$mun<- sub("^0+", "", Mex$mun)
Mex$ent<- sub("^0+", "", Mex$ent)

## Create GID with same format as other datset
Mex$GID<-paste0("MEX.", Mex$ent, ".", Mex$mun)

# creating numeric social vulnerability variable
Mex$GRADO_MARG_NUM <- as.numeric(Mex$GRADO_MARG)

Mex <-  Mex %>%
  mutate(GRADO_MARG_NUM = recode(GRADO_MARG_NUM, '1' = 4, '2'  = 2, '3' = 3, '4' = 5, '5'= 1))


Mex_census_all<-Mex


Mex_census_all= as.data.frame(Mex_census_all)

Mex_census_all=Mex_census_all[-c(1:3,449)]
write.csv(Mex_census_all, file="Mex_census_2010.csv", row.names = FALSE )


## Plotting census data
Unemployed<-ggplot() + 
  geom_sf(data = Mex, (aes(fill=ECO25))) + 
  ggtitle("Mexico Municipalities unemployment") + 
  coord_sf()+
  scale_fill_viridis_c(option = "D")
Unemployed

Education<-ggplot() + 
  geom_sf(data = Mex, (aes(fill= EDU31_R))) + 
  ggtitle("Mexico Municipalities education") + 
  coord_sf()+
  scale_fill_viridis_c(option = "D")
Education

Social<-ggplot() + 
  geom_sf(data = Mex, (aes(fill= GRADO_MARG))) + 
  ggtitle("Mexico Municipalities Social") +
  scale_fill_manual(breaks=c('Muy bajo', 'Bajo', 'Medio', 'Alto', 'Muy alto'), values = c("darkred", "firebrick3", "orange2", "gold1", "forestgreen")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_sf()
Social
