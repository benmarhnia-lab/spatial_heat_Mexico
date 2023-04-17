# spatial_heat_Mexico
This is the code and data for the following project:

Socio-geographical variation in the effects of extreme heat on mortality: a spatial analysis of municipality-level vulnerability across Mexico

# Datasets

Mexico mortality data- Gobierno de Mexico, 1998-2020
downloaded from https://datos.gob.mx/busca/dataset/defunciones
Example data: Mex_mort_2020

Mexico census data- INEGI, 2010
downloaded from https://www.inegi.org.mx/programas/ccpv/2010/
Mex_census_2010.csv

Daily population-weighted temperature for municipalities in Mexico, Daymet V4 and WorldPop
data from https://developers.google.com/earth-engine/datasets/catalog/NASA_ORNL_DAYMET_V4
and https://developers.google.com/earth-engine/datasets/catalog/WorldPop_GP_100m_pop
Example data: Mex_tmax_2020.csv

# Code

Script 1/4:
Rgee_zonal_stats_mex.R -processing daily population-weighted temperature estimates

Script 2/4:
Data_prep_mortality_temp_all_mex.R -cleaning and combining mortality and temperature data

Script 3/4:
Mexico_census_prep.R -cleaning and prepping census data

Script 4/4
Spatial_analysis_mortality_temp_mex.R- spatial analysis
