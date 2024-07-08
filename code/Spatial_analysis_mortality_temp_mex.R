#------------------------------------------------------------------------------#
#-------------------------Heat and mortality in Mexico-------------------------#   
#-------------------------R code 4/4-------------------------------------------#
#-------------------------Spatial analysis-------------------------------------#
#-------------------------Date:7/8/24-----------------------------------------#
#-------------------------Lara Schwarz-----------------------------------------#
#------------------------------------------------------------------------------#

### codes originally written and adapted from uc_cche project by Chen

#-------------------------------------Download packages-----------------------------#


# packages for matching procedure
library(lme4)
library(survival)
library(lubridate)
library(data.table)
library(sf)
library(dplyr)

# packages for BHM
library(sp)
library(gstat) 
library("spBayes")
library(coda)
library(MBA)
library(sf)
library(ggplot2)
library(FRK)
library(fields)
library(RColorBrewer)
library(maptools)
library(rgeos)
library(tidyverse)
library(raster)
library(rgdal)
library(tiff)
library(automap)
library(cowplot)

#package for meta-regression
library(meta)

## packages for case-crossover visualization
library(pals)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(sf)
library(MetBrewer)

#-------------------------------------Prepping spatial data----------------------------#
#dataset <- load(file = "D:/Lara/Border/Data/Mexico_mortality/Temp_mortality_project/Mex_mort_hw_1998_2020.Rdata")

dataset <- load(file = "D:/Lara/Border/Data/Mexico_mortality/Temp_mortality_project/Mex_mort_hw_1998_2019.Rdata")

## creating an absolute heat wave threshold
DF$hw_30abs <- ifelse(DF$tmax_pop_weighted >= 30, 1, 0)
DF$hw_35abs <- ifelse(DF$tmax_pop_weighted >= 35, 1, 0)


shape <- st_read("D:/Lara/Border/Data/Mexico_social/00/municipal.shp")  ### Mexico shapefile
shape <- st_transform(shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

shape$ent<- substring(shape$CVEGEO, 1, 2)
shape$mun<- substring(shape$CVEGEO, 3, 6)

##removing leading 0s
shape$mun<- sub("^0+", "", shape$mun)
shape$ent<- sub("^0+", "", shape$ent)

## Create GID with same format as other dataset
shape$GID<-paste0("MEX.", shape$ent, ".", shape$mun)


world <- map_data("world2")
Mexico <- world[world$region ==  "Mexico", ]
Mexico<-Mexico[is.na(Mexico$subregion),]

#ggplot(Mexico)
Mexico$long<-Mexico$long-360


Mexico_mun <- df_to_SpatialPolygons(Mexico, "region", c("long", "lat"), 
                                    proj=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#-------------------------------------Descriptive stats----------------------------#
avg_deaths<- DF %>%
  dplyr:: group_by(GID) %>%
  dplyr::summarise(deaths_avg=mean(deaths),
                   deaths_tot=sum(deaths))
                   
stats<-merge(DF, avg_deaths, by= "GID" )
                  
summary(stats$tmax_pop_weighted[stats$hw_99==1])                 
summary(stats$tmax_pop_weighted[stats$hw_95==1])                 
summary(stats$tmin_pop_weighted[stats$hw_95_min==1])                 
summary(stats$tmin_pop_weighted[stats$hw_99_min==1])                 

summary(stats$deaths[stats$hw_95_min==1])                 
summary(stats$deaths[stats$hw_95_min==0])    
summary(stats$deaths[stats$hw_99_min==1])                 
summary(stats$deaths[stats$hw_99_min==0])    
summary(stats$deaths[stats$hw_95==1])                 
summary(stats$deaths[stats$hw_95==0])    
summary(stats$deaths[stats$hw_99==1])                 
summary(stats$deaths[stats$hw_99==0])    
summary(stats$deaths[stats$hw_30abs==1])                 
summary(stats$deaths[stats$hw_30abs==0])    
summary(stats$deaths[stats$hw_35abs==1])                 
summary(stats$deaths[stats$hw_35abs==0])    


sum(stats$hw_99[stats$hw_99==1])
sum(stats$hw_95[stats$hw_95==1])
sum(stats$hw_95_min[stats$hw_95_min==1])
sum(stats$hw_99_min[stats$hw_99_min==1])
sum(stats$hw_35abs[stats$hw_35abs==1])
sum(stats$hw_30abs[stats$hw_30abs==1])

group1 <- stats$deaths[stats$hw_30abs == 1]
group2 <- stats$deaths[stats$hw_30abs == 0]

# Perform t-test
t_test_result <- t.test(group1, group2)

# Extract p-value
p_value <- t_test_result$p.value

# Print the p-value
print(p_value)


# List of 'hw' variable names 
hw_vars <- c("hw_99", "hw_95", "hw_99_min", "hw_95_min", "hw_35abs", "hw_30abs")  

# Initialize an empty list to store results
results_list <- list()

# Loop through each 'hw' variable
for (hw_var in hw_vars) {
  # Subset the data for current 'hw_var' when it equals 1 and 0
  group1 <- stats$deaths[stats[[hw_var]] == 1]
  group2 <- stats$deaths[stats[[hw_var]] == 0]
  
  # Perform t-test
  t_test_result <- t.test(group1, group2)
  
  # Extract and store p-value
  p_value <- t_test_result$p.value
  
  # Store results in a list
  results_list[[hw_var]] <- list(
    hw_variable = hw_var,
    p_value = p_value
  )
}

# Print results
for (result in results_list) {
  cat("Variable:", result$hw_variable, "\t", "P-value:", result$p_value, "\n")
}


                   
#-------------------------------------Descriptive mapping----------------------------#
# 
data<- DF %>%
  dplyr::group_by(GID) %>%
  dplyr::summarise(tmax_hw99 = min(tmax_pop_weighted[hw_99==1]),
                   tmax_hw95 = min(tmax_pop_weighted[hw_95==1]),
                   tmin_hw99 = min(tmin_pop_weighted[hw_99_min==1]),
                   tmin_hw95 = min(tmin_pop_weighted[hw_95_min==1])
                    )


hw_viz <- merge(shape, data, by="GID", all.x=TRUE)

rng = range(c((25), (48)))


rng_min = range(c((10), (35)))

## Map of HW thresholds
hw_99<-ggplot()  +
        geom_sf(data = hw_viz, aes(fill = tmax_hw99), colour = NA)  +

        geom_polygon(data=Mexico_mun,aes(x=long, y=lat, group=group),
                     fill = NA, colour = "grey30",  size = 0.01) +
        scale_fill_gradient2(low = "gold", mid = "lightcoral",
                             high="darkred", midpoint = mean(rng), na.value = "darkgrey", limits=c(floor(rng[1]), ceiling(rng[2]))) +
        guides(fill = guide_colourbar(barheight = unit( 2.25 , "in" ),
                                      ticks.colour = "black",
                                      ticks.linewidth = 1, frame.colour = "black",
                                      frame.linewidth = 1))+
  labs(fill = "99th Tmax") +  # Change the legend name here
  theme(panel.background = element_rect(fill = NA),  # Set background to no color
        panel.grid = element_blank(),
        text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        aspect.ratio = 1,
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1))

hw_95<-ggplot()  +
               geom_sf(data = hw_viz, aes(fill = tmax_hw95), colour = NA)  +

               geom_polygon(data=Mexico_mun,aes(x=long, y=lat, group=group),
                            fill = NA, colour = "grey30", size = 0.01) +
  scale_fill_gradient2(low = "gold", mid = "lightcoral",
                       high="darkred", midpoint = mean(rng), na.value = "darkgrey", limits=c(floor(rng[1]), ceiling(rng[2]))) +
               guides(fill = guide_colourbar(barheight = unit( 2.25 , "in" ),
                                             ticks.colour = "black",
                                             ticks.linewidth = 1, frame.colour = "black",
                                             frame.linewidth = 1))+
  labs(fill = "95th Tmax") +  # Change the legend name here
  theme(panel.background = element_rect(fill = NA),  # Set background to no color
        panel.grid = element_blank(),
        text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        aspect.ratio = 1,
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1))
               

hw_99_min <- ggplot() +
  geom_sf(data = hw_viz, aes(fill = tmin_hw95), colour = NA) +
  geom_polygon(data = Mexico_mun, aes(x = long, y = lat, group = group),
               fill = NA, colour = "grey30", size = 0.01) +
  scale_fill_gradient2(low = "olivedrab4", mid = "gold",
                       high = "lightcoral", midpoint = mean(rng_min), na.value = "darkgrey",
                       limits = c(floor(rng_min[1]), ceiling(rng_min[2]))) +
  guides(fill = guide_colourbar(barheight = unit(2.25, "in"),
                                ticks.colour = "black",
                                ticks.linewidth = 1, frame.colour = "black",
                                frame.linewidth = 1)) +
  labs(fill = "99th Tmin") +  # Change the legend name here
  theme(panel.background = element_rect(fill = NA),  # Set background to no color
        panel.grid = element_blank(),
        text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        aspect.ratio = 1,
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1))

hw_95_min <- ggplot() +
  geom_sf(data = hw_viz, aes(fill = tmin_hw95), colour = NA) +
  geom_polygon(data = Mexico_mun, aes(x = long, y = lat, group = group),
               fill = NA, colour = "grey30", size = 0.01) +
  scale_fill_gradient2(low = "olivedrab4", mid = "gold",
                       high = "lightcoral", midpoint = mean(rng_min), na.value = "darkgrey",
                       limits = c(floor(rng_min[1]), ceiling(rng_min[2]))) +
  guides(fill = guide_colourbar(barheight = unit(2.25, "in"),
                                ticks.colour = "black",
                                ticks.linewidth = 1, frame.colour = "black",
                                frame.linewidth = 1)) +
  labs(fill = "95th Tmin") +  # Change the legend name here
  theme(panel.background = element_rect(fill = NA),  # Set background to no color
        panel.grid = element_blank(),
        text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        aspect.ratio = 1,
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1))

outdir_res<-"D:/Lara/Border/results/Heat_cardio_resp_hosp/Heat_mortality/Descriptive_figures"

## Map of HW thresholds
png(file.path(outdir_res,  "HW_thresholds.png"),
    width = 6.5, height = 6, units = "in", res = 300)
print(plot_grid(hw_99, hw_95, hw_99_min, hw_95_min))

dev.off()


#-------------------------------------Case Crossover Analysis by state -----------------------------#

DF$wday<-wday(DF$date)
DF$year<-year(DF$date)
DF$week<-week(DF$date)
DF$day<-format(as.Date(DF$date,format="%Y-%m-%d"), "%d")

#DF$date<- as.numeric(as.character(as.Date(DF$date, format = "%Y-%m-%d"), format="%Y%m%d"))

DF_mort = subset(DF, select = c("ent", "date", "GID", "month", "year", "day", "wday", "deaths")     ) 
DF_temp = subset(DF, select = c("ent", "date", "GID", "month", "year", "wday",  "hw_99")     )

DF_mort<-DF_mort[which(  DF_mort$deaths>0), ]
DF_mort$ID_grp<-seq.int(nrow(DF_mort)) 

DF_temp <- DF_temp[order(DF_temp$date), ]
#DF_temp$date<- as.numeric(as.character(as.Date(DF_temp$date, format = "%Y-%m-%d"), format="%Y%m%d"))
DF_temp$date2 <- as.numeric(format(as.Date(DF_temp$date, format = "%Y-%m-%d"), "%Y%m%d"))

DF_mort$death_date <- as.numeric(format(as.Date(DF_mort$date, format = "%Y-%m-%d"), "%Y%m%d"))
DF_mort<-DF_mort %>% dplyr::select( -c("date", "ent"))

DF_data_cc <- DF_mort %>%       
  dplyr::left_join(DF_temp, by = c("GID","year", "month", "wday"), relationship = "many-to-many") %>%  #add the temperature data
  dplyr::mutate(case = if_else(death_date==date2, 1, 0))            #generate case and control observations

model_region<-clogit(case~ hw_99 + strata(ID_grp), data=DF_data_cc, weights=deaths, method="approximate")
summary(model_region)

model_state <- clogit(case ~ hw_99 + strata(ID_grp) + strata(ent), 
                      data = DF_data_cc, 
                      weights = deaths, 
                      method = "approximate")

summary(model_state)
summary_model <- summary(model_state)

state_coef <- coef(summary_model)


# Assuming 'ID_grp' is your municipality variable
states <- unique(DF_data_cc$ent)

# Create empty data frame to store results
results_df <- data.frame(Municipality = numeric(), Coefficient = numeric(), CI_Lower = numeric(), CI_Upper = numeric(), stringsAsFactors = FALSE)


# Create empty data frame to store results
results_df <- data.frame(state = character(), Coefficient = numeric(), SE = numeric(), stringsAsFactors = FALSE)

# Assuming 'ent' is your grouping variable (replace with the actual variable name)
states <- unique(DF_data_cc$ent)

# Create empty data frame to store results
results_df <- data.frame(state = character(), Coefficient = numeric(), SE = numeric(), 
                         stringsAsFactors = FALSE)

# Loop through each group
for (state in states) {
  # Subset data for the current group
  subset_data <- subset(DF_data_cc, ent == state)
  
  # Fit the Cox proportional hazards model
  model_region <- try(clogit(case~ hw_99 + strata(ID_grp), data=subset_data, weights=deaths, method="approximate"), 
                      silent = TRUE)
  
  # Check if the model was successfully fitted
  if (!inherits(model_region, "try-error")) {
    # Extract relevant information from the model summary
    model_summary <- summary(model_region)
    coef_table <- model_summary$coefficients
    
    # Store results in the data frame
    results_df <- bind_rows(results_df, list(state = as.character(state), 
                                             Coefficient = coef_table["hw_99", "coef"], 
                                             SE = coef_table["hw_99", "se(coef)"]))
  }
}
results_df$OR<-exp(results_df$Coefficient)
z_scores <- qnorm(1 - 0.05 / 2)  # 0.05 corresponds to the 95% confidence level
results_df$lower_ci <- results_df$OR * exp(-z_scores * results_df$SE)
results_df$upper_ci <- results_df$OR * exp(z_scores * results_df$SE)

# from R

# Step 2: Compute variances
results_df$variances <- results_df$SE^2

# Step 3: Compute total variance
results_df$total_variance <- sum(results_df$variances)
# total_variance = 0.02268512

# Step 4: Compute standard deviation
standard_deviation <- sqrt(total_variance)
# standard_deviation = sqrt(0.2145) = 0.4634

# Print the results
print(results_df)
out<- results_df
out$state<-as.numeric(out$state)
# Visualizing results of case crossover

# Assuming 'ent' is your grouping variable (replace with the actual variable name)
group_var <- "ent"

# Load your data and replace the paths with your actual file paths
#out <- fread(file.path(outdir1, "results", paste0(dataset, "_1396_region_model_summary.csv")), colClasses = c("code" = "character"))
#file.name <- paste0(dataset, "_1396_case_crossover.png")

# Assuming your data has a column 'state' for the states in Mexico
state_shapes <- read_sf("D:/Lara/Border/Data/Mexico_social/00/estatal.shp", stringsAsFactors = FALSE, as_tibble = FALSE)
state_shapes$CVEGEO<-as.numeric(state_shapes$CVEGEO)

# Assuming 'code' is the variable representing the state codes in your 'out' dataset
out_states <- out[out$state %in% state_shapes$CVEGEO, ]


#for (ot_ in unique(out$outcome)) {
# for (lag_ in unique(out$lag)) {
#  for (estimate_ in unique(out$Coefficient)) {
## results for states
#temp_states <- out_states[out_states$outcome==ot_ & out_states$lag==lag_ & out_states$estimate==estimate_, ]
temp_states <- out
state_order <- unique(temp_states$state[order(temp_states$Coefficient)])
temp_states$state <- factor(temp_states$state, levels=state_order)
state_shapes$CVEGEO<-factor(state_shapes$CVEGEO)
sp_states <- merge(state_shapes, temp_states, by.x = "CVEGEO", by.y = "state", all.y = TRUE)

# col_values_states <- met.brewer("Johnson", n = nrow(out))

# fixing states with accents in name
sp_states$NOMBRE[sp_states$CVEGEO == 15] <- "Estado de Mexico"
sp_states$NOMBRE[sp_states$CVEGEO == 16] <- "Michoacan de Ocampo"
sp_states$NOMBRE[sp_states$CVEGEO == 19] <- "Nuevo Leon"
sp_states$NOMBRE[sp_states$CVEGEO == 22] <- "Queretaro"
sp_states$NOMBRE[sp_states$CVEGEO == 24] <- "San Luis Potosi"
sp_states$NOMBRE[sp_states$CVEGEO == 31] <- "Yucatan"
sp_states$NOMBRE[sp_states$CVEGEO == 9] <- "Ciudad de Mexico"

# sp_states <- sp_states[order(sp_states$Coefficient), ]
col_values_states <- met.brewer("Johnson", n = nrow(out))





# Order sp_states by Coefficient
sp_states <- sp_states[rev(order(sp_states$Coefficient)), ]

# Get number of states
num_states <- nrow(sp_states)

# Create color palette with higher values in red
#col_values_states <- rev(brewer.pal(num_states, "RdYlBu"))
col_values_states <- met.brewer("Johnson", n = nrow(sp_states))
col_values_states <- rev(col_values_states)

# Plot
p1 <- ggplot(sp_states) +
  geom_sf(aes(fill = Coefficient)) +
  scale_fill_gradientn(colors = col_values_states, 
                       breaks = unique(sp_states$Coefficient), 
                       labels = sp_states$NOMBRE, 
                       guide = guide_legend(title = "State")) +
  labs(x = "", y = "", title = "") +
  theme_bw() +
  theme(
    panel.grid = element_blank(), 
    legend.text = element_text(size = rel(0.8)),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank()
  )



## Figure with odds ratios by State

## results for counties
temp1 <- sp_states
# state.order <- unique(temp1$CVEGEO[order(temp1$Coefficient, decreasing = TRUE)])
# temp1$state <- factor(temp1$state, levels=state_order)
temp1 <- temp1[order(temp1$Coefficient), decreasing = TRUE ]

sp1 <- temp1

dodge <- position_dodge(width=0.6)
col.values1.met <- met.brewer("Johnson", n=nrow(temp1)) ## colors for 57 counties

# Reorder NOMBRE based on Coefficient
temp1$NOMBRE <- factor(temp1$NOMBRE, levels = temp1$NOMBRE[order(temp1$Coefficient)])

# Reverse the order of levels for NOMBRE
temp1$NOMBRE <- factor(temp1$NOMBRE, levels = rev(levels(temp1$NOMBRE)))

# Plot with reordered levels
p2 <- ggplot(temp1, aes(y=OR, col=NOMBRE, x=NOMBRE, ymax=upper_ci, ymin=lower_ci)) + 
  geom_hline(yintercept = 1, size=1.2) +
  geom_point(position=dodge, size=3) + geom_errorbar(position=dodge, width=0.2, size=1.2) + 
  scale_color_manual(values= unname(col.values1.met)[order(temp1$Coefficient)], na.value = "darkgrey") +
  labs(title="", 
       x="State",
       y="Odds ratio") + 
  theme_bw()+
  theme(text = element_text(size=10), legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1))


grid.arrange(p1, p2, 
             nrow=2, heights = c(1, 1) )



#-------------------------------------By Municipality analysis: Matching procedures------------------------------#

## run within-community matched design (Schwarz et al. 2021): controls identified as in the same summer and not an event day, use inverse distance weighting of all control days
## matched time-series (Liu et al. 2017): controls identified as 1) within the window of 7 calendar days before or after the exposed day in another year and 2) separated from any other exposed day for more than 2 days
## Bobb et al. 2014: controls identified as 1) within the window of 3 days before and after the exposed day in another year and 2) separated from any other exposed day for more than 2 days
start_time <- Sys.time()
##################
## identify potential control days for each exposed day baed on Liu et al. method with controls identified as: 
## 1) within the window of 7 calendar days before or after the exposed day in another year and 
## 2) separated from any other exposed day for more than 2 days
year.control <- function(exposed, control, years, nbuffer) { 
  if (length(exposed) > 0) {
    out <- lapply(exposed, function(e_day) {
      e_buffer <- yday(e_day) + (-nbuffer:nbuffer) ## potential days of year for control (7 day window from exposure)
      buffer_year <- years[years!=year(e_day)] ## potential years for control (not the year of event)
      # ndays <- yday(as.Date(paste0(year(e_day), "-12-31"))) ## days in the year of exposure
      # ndays_bf <- yday(as.Date(paste0(year(e_day) - 1, "-12-31"))) ## days in the year before exposure
      # ## there are 4 event days across eh1wf1, eh1wf0, eh0wf1 that had potential 
      # ## days of year that includes day 366, which has no corresponding day in some years
      # ## thus I decreased the corresponding potential days of year by one and set
      # ## the days in the year of exposure to 365 for these four event days
      # if (ndays == 366 & max(e_buffer) > 365) { 
      #   e_buffer <- e_buffer - 1
      #   ndays <- 365
      # }
      c_day <- sapply(buffer_year, function(yr) { ## potential control days based on exposed date
        
        ndays <- yday(as.Date(paste0(yr, "-12-31"))) ## days in the year of exposure
        ndays_bf <- yday(as.Date(paste0(yr - 1, "-12-31"))) ## days in the year before exposure
        
        sapply(e_buffer, function(dd) {
          if(dd < 0) {
            as.IDate(paste(yr-1, dd + ndays_bf), format = "%Y %j")
          } else if (dd > ndays) {
            as.IDate(paste(yr+1, dd - ndays), format = "%Y %j")
          } else {
            as.IDate(paste(yr, dd), format = "%Y %j")
          }
        })
      })
      c_pool <- control[control %in% c(c_day)]  ## potential control days after excluding days close to other exposure
      return(c(e_day, c_pool))
    })
    names(out) <- exposed
  } else {
    out <- numeric()
  }
  return(out)
}

## identify potential control days for each exposed day baed on Liu et al. method: controls identified as 
## 1) within the window of buffer calendar days (30) before or after the exposed day and 
## 2) separated from any other exposed day for more than 2 days
month.control <- function(exposed, control, nbuffer) { 
  if (length(exposed) > 0) {
    out <- lapply(exposed, function(e_day) {
      e_buffer <- e_day + (-nbuffer:nbuffer) ## potential days for control (buffer day window from exposure)
      c_pool <- control[control %in% e_buffer]  ## potential control days after excluding days close to other exposure
      return(c(e_day, c_pool))
    })
    names(out) <- exposed
  } else {
    out <- numeric()
  }
  return(out)
}

## randomly selected n.bobb.control controls for each exposed day, 
## run glmer (poisson) with random effect for each matched exposed-control group 
## unless only one exposed day exists and glm will be used.
glmer.analysis <- function(zcta, exposure, event, c.list, outcome.dt, n.control) { 
if (length(c.list) > 0) {
  out <- numeric()
  for (i in 1:length(c.list)) {
    if (length(c.list[[i]]) > 1) {
      c_pool <- c.list[[i]][-1]
      e_day <- c.list[[i]][1]
      sel <- c_pool[sample(1:length(c_pool), min(n.control, length(c_pool)), replace = FALSE)]
      temp <- data.frame(date = c(e_day, sel), exposed = c(1, rep(0, length(sel))), id = i, available.c = length(c_pool))
      out <- rbind(out, temp)
    }
  }
  if (class(out)=="data.frame") {
    out <- merge(out, outcome.dt, by = "date", all.x = TRUE)
    if (length(unique(out$id)) > 1) {
      f <- reformulate("exposed + (1|id)", response = event)
      m <- tryCatch({
        glmer(f , data = out, family = "poisson")
      }, condition = function(cond) {
        # cat("\t", "glmer", zcta, exposure, event, as.character(cond))
        cond$call <- NULL
        cond
      })
    } else {
      f <- reformulate("exposed", response = event)
      m <- tryCatch({
        glm(f , data = out, family = "poisson")
      }, condition = function(cond) {
        # cat("\t", "glmer", zcta, exposure, event, as.character(cond))
        cond$call <- NULL
        cond
      })
    }
  } else {
    m <- simpleError(paste("No available control day"))
    m$call <- NULL
  }
} else {
  m <- simpleError(paste("No available exposed day"))
  m$call <- NULL
}

if (!inherits(m, what = "condition")) {
  est <- summary(m)$coefficients["exposed",1]
  se <- summary(m)$coefficients["exposed",2]
  temp <- c(exp(est), exp(est - 1.96*se),  exp(est + 1.96*se), length(c.list))
} else {
  temp <- m
}
return(temp)
}

## weight the outcome of all potential controls using the inverse distance in year 
## and directly calculate the incidence ratio for each match exposed and control group, then calculate the average
year.wt.analysis <- function(exposure, event, c.list, outcome.dt) { 
  if (length(c.list) > 0) {
    out <- numeric()
    for (i in 1:length(c.list)) {
      if (length(c.list[[i]]) > 1) {
        e_day <- c.list[[i]][1]
        c_pool <- data.table(date=c.list[[i]], exposed = c(1, rep(0, length(c.list[[i]])-1)))
        c_pool$wt <- 1/round(abs(as.numeric(difftime(c_pool$date, e_day, units = "days"))/365), digits = 0)
        c_pool$wt[1] <- 0 ## assign 0 weight to the day with exposure
        c_pool <- merge(c_pool, outcome.dt, by="date", all.x=TRUE)
        
        rr <- c_pool[exposed==1, eval(as.name(event))]/c_pool[, sum(eval(as.name(event))*wt)/sum(wt)]
        rr <- ifelse(is.infinite(rr), NA, rr)
        out <- c(out, rr)
      }
    }
    
    
    temp <- c(mean(out, na.rm=TRUE), sum(!is.na(out)))
    
    if (is.na(temp[1])) {
      temp <- simpleError(paste("No non-infinite RR"))
      temp$call <- NULL
    }
    
    
  } else {
    temp <- simpleError(paste("No available exposed day"))
    temp$call <- NULL
  }
  return(temp)
}

## weight the outcome of all potential controls using the distance in day and 
## directly calculate the incidence ratio for each match exposed and control group, then calculate the average

month.wt.analysis <- function(exposure, event, c.list, outcome.dt) { 
  if (length(c.list) > 0) {
    out <- numeric()
    for (i in 1:length(c.list)) {
      if (length(c.list[[i]]) > 1) {
        
        e_day <- c.list[[i]][1]
        c_pool <- data.table(date=c.list[[i]], exposed = c(1, rep(0, length(c.list[[i]])-1)))
        c_pool$wt <- 1/round(abs(as.numeric(difftime(c_pool$date, e_day, units = "days"))), digits = 0)
        c_pool$wt[1] <- 0 ## assign 0 weight to the day with exposure
        c_pool <- merge(c_pool, outcome.dt, by="date", all.x=TRUE)
        
        rr <- c_pool[exposed==1, eval(as.name(event))]/c_pool[, sum(eval(as.name(event))*wt)/sum(wt)]
       rr <- ifelse(is.infinite(rr), NA, rr)
        #rd <- (c_pool[exposed==1, eval(as.name(event))]-c_pool[, sum(eval(as.name(event))*wt)/sum(wt)])
        #/c_pool$POB1*100000
       # out <- c(out, rr)
        
        out <- c(out, rr)
     
        
       
          }
    }
    
    
    temp <- c(mean(out, na.rm=TRUE), sum(!is.na(out)))
    
    if (is.na(temp[1])) {
      temp <- simpleError(paste("No non-infinite RR/RD"))
      temp$call <- NULL
    }
    
    
  } else {
    temp <- simpleError(paste("No available exposed day"))
    temp$call <- NULL
  }
  return(temp)
  #month.wt.res[i, ] = out
  
}

outdir1<-"D:/Lara/Mexico_heat_2024/RR"

 dt<-DF
 dt$yday <- yday(dt$date)
 dt$year<-year(dt$date)
 setDT(dt)


bobb.control.n <-  4
events <- c("deaths")

nms <- c(
  
paste(rep(c("month_wt"), each=2), c("rr", "ngrp"), sep="_"))
nms <- paste(rep(nms, times=1), rep(c("hw_99", "hw_95", "hw_99_min","hw_95_min", "hw_30abs", "hw_35abs"), each=length(nms)), sep="_")
bar <- data.frame(GID=rep(unique(dt$GID), each = length(events)), event = events, 
                 hw_30abs = NA, hw_35abs = NA, 
                  hw_95 = NA, hw_99 = NA, hw_99_min=NA, hw_95_min=NA,
                  bobb_control_pool = NA, 
                  setNames(replicate(length(nms), NA, simplify = F), nms)
)

fail <- numeric()
for (i in 1:nrow(bar)) {
  
  set.seed(i + 824) #random number
  event_ <- bar$event[i]
 # ha_ <- ha[ha$GID==bar$GID[i], ]
  foo <- dt[dt$GID==bar$GID[i], ]
  #foo <- merge(foo, ha_[, .(date, deaths)], by="date", all.x = TRUE)
  setnafill(foo, fill=0, cols = c("deaths")) ## fill in zeros for days without either csd or resp

  ## potential control identification method from Bobb et al--remove those close to exposure
  #buffer_days <- foo$date[foo$hw_90==1 |foo$hw_95==1 |foo$hw_99==1]
  buffer_days <- foo$date[foo$hw_30abs==1|foo$hw_35abs==1]
  
  buffer_days <- unique(c(buffer_days-3, buffer_days-2, buffer_days-1, buffer_days, buffer_days + 1, buffer_days + 2, buffer_days + 3))
  day00_bobb <- foo$date[!(foo$date %in% buffer_days)]
  bar$bobb_control_pool[i] <- length(day00_bobb) ## total number of potential control days excluding those close to exposure
 
  #"hw_99", "hw_95", "hw_99_min","hw_95_min",
  for (exposure_ in c( "hw_30abs", "hw_35abs")) {
    for (event_ in c("deaths")) {

      ## identify exposed days using Bobb method
      
      days <- foo$date[which(foo[, exposure_, with=FALSE]==1)] 
      #days <- foo$date[(paste0("foo$", exposure_))==1] #
      bar[i, exposure_] <- length(days)
      
      
      ## control identification method  same year within 60 days before and after the exposure and exclude these within three days of an event (second is the same as bobb et al. method)
      baz2 <- month.control(days, day00_bobb, 60)
      
      
      ## weighted analysis method adopted from Schwarz et al.
      temp4 <- month.wt.analysis(exposure_, event_, baz2, foo)
      if (!inherits(temp4, what = "condition")) {
        bar[i, paste0(c("month_wt_rr_", "month_wt_ngrp_"), exposure_)] <- temp4
      } else {
        fail <- rbind(fail, data.frame(GID=bar$GID[i], event = event_, exposure = exposure_, ngrps = length(baz2), method = "month_wt", error=as.character(temp4)))
        bar[i, paste0("month_wt_ngrp_", exposure_)] <- length(baz2)
      }
      
  }
}
}


outdir1<-"D:/Lara/Mexico_heat_2024/RR"


#write.csv(bar, file.path(outdir1, "results", paste0(dataset, "_specific.csv")), row.names = FALSE)
#write.csv(fail, file.path(outdir1, "results", paste0(dataset, "_specific_fail.csv")), row.names = FALSE)


write.csv(bar, file.path(outdir1, "results", paste0(dataset, "_specific_abs.csv")), row.names = FALSE)
write.csv(fail, file.path(outdir1, "results", paste0(dataset, "_specific_fail_abs.csv")), row.names = FALSE)


#-------------------------------------Bayesian Spatial Model-data prep-----------------------------#
outdir1<-"D:/Lara/Mexico_heat_2024/RR"

indir1<- "D:/Lara/Border/Data/Mexico_social/00"


methods <- c("month_wt")


## read in data for analysis
bar <- fread(file.path(outdir1, "results", paste0(dataset, "_specific.csv")))
#bar <- fread(file.path(outdir1, "results", paste0(dataset, "_specific_abs.csv")))

fail <- fread(file.path(outdir1, "results", paste0(dataset, "_specific_fail.csv")))
#fail <- fread(file.path(outdir1, "results", paste0(dataset, "_specific_fail_abs.csv")))

shapefile <- st_read(file.path(indir1, "municipal.shp"))


## projection WGS84
shapefile_reproj = st_transform(shapefile,  "+proj=lonlat  +ellps=WGS84 +no_defs" )

##upload WorldPop raster layer
##Population counts Unconstrainted indidivual countries Mexico 2010
## https://hub.worldpop.org/geodata/summary?id=32496
gpop = raster("D:/Lara/Border/Data/Mexico_social/worldpop/mex_ppp_2010_1km_ASCII_XYZ/mex_ppp_2010_1km_Aggregated.tif")

polys<-shapefile_reproj

extent(gpop) <- extent(polys)

## compute population weighted centroids
## Convert polygons to a raster layer
z <- rasterize(polys, gpop)

## Compute weighted x and y coordinates within each rasterized region
xx <- zonal(init(gpop, v="x")*gpop, z) / zonal(gpop,z)
yy <- zonal(init(gpop, v="y")*gpop, z) / zonal(gpop,z)

id<-as.matrix(shape$CVEGEO)

## Combine results in a matrix
Mex_pop_centroids <- cbind(xx[,2],yy[,2], id[,1])
head(Mex_pop_centroids)

Mex_pop_centroids<-as.data.frame(Mex_pop_centroids)

Mexico_latlon <- data.frame(lat=Mex_pop_centroids[,2], long=Mex_pop_centroids[,1], ID=Mex_pop_centroids[,3])

Mexico_latlon$lat<-as.numeric(Mexico_latlon$lat)
Mexico_latlon$long<-as.numeric(Mexico_latlon$long)

Mexico_latlon$ent<- substring(Mexico_latlon$ID, 1, 2)
Mexico_latlon$mun<- substring(Mexico_latlon$ID, 3, 6)

##removing leading 0s
Mexico_latlon$mun<- sub("^0+", "", Mexico_latlon$mun)
Mexico_latlon$ent<- sub("^0+", "", Mexico_latlon$ent)

## Create GID with same format as other datset
Mexico_latlon$GID<-paste0("MEX.", Mexico_latlon$ent, ".", Mexico_latlon$mun)

coords <-Mexico_latlon


##  Map population weighted centroids
# print(ggplot()  +
#         geom_point(data = coords,aes(x=long, y=lat))  +
#         geom_polygon(data=shapefile_reproj, aes(x=long, y=lat, group=group),
#                      colour="black", fill = NA) )



bar <- fread(file.path(outdir1, "results", paste0(dataset, "_specific.csv")))
#bar <- fread(file.path(outdir1, "results", paste0(dataset, "_specific_abs.csv")))

avg_deaths<- DF %>%
 dplyr:: group_by(GID) %>%
  dplyr::summarise(deaths_avg=mean(deaths),
           deaths_tot=sum(deaths))
            

bar<-merge(bar, avg_deaths, by= "GID" )

# 500 was used for all analyses in RR and RR_inc2020 folders
bar<- bar[bar$deaths_tot>500,]


# check excluded municipalities
#bar_mis<- bar[bar$deaths_tot<500,]
#missing <- merge(bar_mis, census, by="GID", all.x=TRUE)
#sd(missing$POB1)

#not excluded
nonmissing <- merge(bar, census, by="GID", all.x=TRUE)
summary(nonmissing$POB1)
mean(nonmissing$POB1)
sd(nonmissing$POB1, na.rm = T)


#-------------------------------------Bayesian Spatial Model-analysis-----------------------------#

methods <- c("month_wt")

outdir1<-"D:/Lara/Mexico_heat_2024/RR"

outdir3 <- file.path(outdir1, "spatial")
if (!dir.exists(outdir3)) dir.create(outdir3)


for (m in methods) {
  
  for (exposure_ in c("hw_99")) {
   #, "hw_95", "hw_99_min","hw_95_min", "hw_30abs", "hw_35abs"
    
if (!dir.exists(file.path(outdir3))) dir.create(file.path(outdir3))
sink(file.path(outdir3,  paste0( exposure_, "summary of running spatial bayesian.txt")), split=TRUE)

  
  fail.GID <- unique(fail$GID[fail$method==m & fail$exposure==exposure_])
  baz <- bar[!(bar$GID %in% fail.GID), ]
  cat("\n\n", length(fail.GID), "GID removed due to failure in", m, "\n")
  
    baz[, paste0(m, exposure_):=eval(as.name(paste0(m, "_rr_", exposure_)))]
  
  baz <- baz[, c("GID", paste0(m, exposure_)), with=FALSE]
  

  ## create spatial data frame
  bayesDF <- merge(baz, coords, by= "GID", na.rm=TRUE ) #took out all=TRUE
  bayesDF<-subset(bayesDF, GID %in% coords$GID)
  
  names(bayesDF)[2] <- "est"
  
  ## focusing on excess relative risk 
  
  bayesDF$est<- bayesDF$est-1
  
 
  # projection datum
  crs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  spdf <- SpatialPointsDataFrame(coords = bayesDF[,.(long, lat)],
                              #  proj4string = crs,
                                 data = bayesDF)

  
  v1 <- variogram(est~1,  data = spdf)
  
  

   print(plot(v1, main=paste0(dataset, "_", m, nrow(bayesDF), "GID"), cex=1.5))
   

  png(file.path(outdir3,  paste0(m, nrow(bayesDF), "GID", exposure_, "_variogram.png")))
  print(plot(v1, main=paste0(dataset, "_", m, nrow(bayesDF), "GID"), cex=1.5))
  # #Assumes Isotropy
  dev.off()


## mapping
dt <- merge(shape, bayesDF, by="GID", all.y=TRUE) 

png(file.path(outdir3, paste0(m, nrow(bayesDF), "GID", exposure_, "_raw_rr.png")))


print(ggplot()  +
        geom_sf(data = dt, aes(fill = est), colour = NA)  +
        geom_polygon(data=Mexico_mun,aes(x=long, y=lat, group=group),
                     colour="black", fill = NA) +
        scale_fill_gradient2(low = "blue", mid = "white",
                             high="red", midpoint = 0, limits=c(-1, 8), breaks=c(0,  2,  4, 6), 
                             na.value = "darkgrey") +
        guides(fill = guide_colourbar(barheight = unit( 3 , "in" ),
                                      ticks.colour = "black",
                                      ticks.linewidth = 1, frame.colour = "black",
                                      frame.linewidth = 1))+
        labs(fill = exposure_) + theme( panel.background = element_rect(fill = 'lightgrey'),
                                        panel.grid = element_blank(),text = element_text(size=10),
                                        axis.title.x=element_blank(), axis.title.y = element_blank(),
                                        aspect.ratio = 1, axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))
)
dev.off()
#}}
  ## conduct Bayesian model 

  n.samples = 10000

                   
    bef.sp <- spLM(est ~ 1, data = bayesDF, coords = as.matrix(bayesDF[,.(long, lat)]),
                   starting = list("phi" = 8, "sigma.sq" = 0.45, "tau.sq" = 0.35), 
                    tuning = list("phi" = 0.4, "sigma.sq" = 0.0225, "tau.sq" = 0.0175),
                    priors = list("phi.Unif" = c(0.001, 10), "sigma.sq.IG" = c(0.001, 0.001) 
                            , "tau.sq.IG" = c(0.001, 0.001) 
                   
              
                #hw99     starting = list("phi" = 8, "sigma.sq" = 0.45, "tau.sq" = 0.35), 
                #hw99     tuning = list("phi" = 0.4, "sigma.sq" = 0.0225, "tau.sq" = 0.0175),
                #hw99     priors = list("phi.Unif" = c(0.001, 10), "sigma.sq.IG" = c(0.001, 0.001) 
                #hw99              , "tau.sq.IG" = c(0.001, 0.001) 

                #hw95     starting = list("phi" = 8, "sigma.sq" = 0.14, "tau.sq" = 0.1), 
                #hw95     tuning = list("phi" = 0.4, "sigma.sq" = 0.007, "tau.sq" = 0.005),
                #hw95     priors = list("phi.Unif" = c(0.001, 10), "sigma.sq.IG" = c(0.001, 0.001) 
                #hw95                            , "tau.sq.IG" = c(0.001, 0.001) 
                                  
                #hw99min  starting = list("phi" = 8, "sigma.sq" = 0.5, "tau.sq" = 0.3), 
                #hw99min  tuning = list("phi" = 0.4, "sigma.sq" = 0.025, "tau.sq" = 0.015),
                #hw99min  priors = list("phi.Unif" = c(0.001, 10), "sigma.sq.IG" = c(0.001, 0.001) 
                #hw99min                              , "tau.sq.IG" = c(0.001, 0.001) 
                                      
                #hw95min  starting = list("phi" = 8, "sigma.sq" = 0.13, "tau.sq" = 0.08), 
                #hw95min  tuning = list("phi" = 0.4, "sigma.sq" = 0.0065, "tau.sq" = 0.004),
                #hw95min  priors = list("phi.Unif" = c(0.001, 10), "sigma.sq.IG" = c(0.001, 0.001) 
                #hw95min                                    , "tau.sq.IG" = c(0.001, 0.001) 
                                                    
                #hw_30abs starting = list("phi" = 9, "sigma.sq" = 0.07, "tau.sq" = 0.15), 
                #hw_30abs tuning = list("phi" = 0.45, "sigma.sq" = 0.0035, "tau.sq" = 0.0075),
                #hw_30abs priors = list("phi.Unif" = c(0.001, 10), "sigma.sq.IG" = c(0.001, 0.001) 
                #hw_30abs                                 , "tau.sq.IG" = c(0.001, 0.001)  
                                   
                #hw_35abs starting = list("phi" = 10, "sigma.sq" = 1.7, "tau.sq" = 1.8), 
                #hw_35abs tuning = list("phi" = 0.5, "sigma.sq" = 0.08, "tau.sq" = 0.09),
                #hw_35abs priors = list("phi.Unif" = c(0.001, 11), "sigma.sq.IG" = c(0.001, 0.001) 
                #hw_35abs                          , "tau.sq.IG" = c(0.001, 0.001)                               
                                   
                 ),
                   cov.model = "spherical", n.samples = n.samples, verbose = TRUE, n.report=3000)
  
    saveRDS(bef.sp, file = file.path(outdir1, "results", paste0( exposure_, m, nrow(bayesDF), "GID", "BHM_res.Rds")))
    
  
  cat("summary of thetas before burn-in", "\n")
  print(round(summary(mcmc(bef.sp$p.theta.samples))$quantiles, 3))
  png(file.path(outdir3,  paste0(m, nrow(bayesDF), "GID_", exposure_, "_mcmctrace.png")))
  plot(bef.sp$p.theta.samples)
  dev.off()
  
  ## exclude 75% samples as burn-in
  burn.in <- floor(0.75*n.samples)
  bef.sp <- spRecover(bef.sp, start = burn.in, n.report=1000)
  
  cat("summary of thetas after burn-in", "\n")
  print(round(summary(mcmc(bef.sp$p.theta.recover.samples))$quantiles, 3))
  png(file.path(outdir3,  paste0(m, nrow(bayesDF), "GID_", exposure_, "_mcmctrace_afterburnin.png")))
  plot(bef.sp$p.theta.recover.samples)
  dev.off()
  
  beta.samples <- bef.sp$p.beta.recover.samples
  
  print(summary(beta.samples))
  
  
  w.samples <- bef.sp$p.w.recover.samples
  
  bayesDF$w_hat_mu <- apply(w.samples, 1, mean)
  bayesDF$w_hat_sd <- apply(w.samples, 1, sd)
  bayesDF$SNR <- bayesDF$w_hat_mu/bayesDF$w_hat_sd
  bayesDF$truncSNR <- ifelse(bayesDF$SNR < 2 &  bayesDF$SNR > -2, NA, bayesDF$SNR )
  
  bayesDF$overall_mu <- mean(beta.samples)
  bayesDF$overall_sd <- sd(beta.samples)
  
  

  print(summary(bayesDF$est))
  print(summary(bayesDF$w_hat_mu))
  print(summary(bayesDF$w_hat_sd))
  print(summary(bayesDF$SNR))
  print(summary(bayesDF$truncSNR))
  write.csv(bayesDF, file.path(outdir1, "results", paste0( exposure_, m, nrow(bayesDF), "GID.csv")))
 
  
  png(file.path(outdir3, paste0(m, nrow(bayesDF), "GID_", exposure_, "_original_resolved.png")),
      width = 6.5, height = 6, units = "in", res = 300)

  ## plot interpolated Surface
  par(mfrow = c(1,1))
  surf <- mba.surf(cbind(bayesDF[,.(long, lat)], bayesDF$est), no.X = 300, no.Y = 300, extend = TRUE
                   ,
                   b.box=c(min(coords$long), max(coords$long), min(coords$lat), max(coords$lat))
  )$xyz.est
  z.lim <- range(surf[[3]], na.rm = T)
  
  ## specifying colors for image.plot
  colorTable<- designer.colors(10, c("#ABDDA4", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142", "brown", "chocolate4" ,  "purple4", "black") )
  
  # breaks
  brks<- c( -1, 0, 1, 2, 3, 4, 5, 6, 7, 8,  12) 
   #brks<- c( -1, 0, 1, 2, 3, 4, 5, 6) 
  

  print(image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, breaks = brks, col=colorTable,  main = paste0(dataset, exposure_ ,"_original_", m, nrow(bayesDF))))
  
print(plot(Mexico_mun, border="black", bg="gray1", add = T))



  dev.off()
  
  # png(file.path(outdir3, dataset, paste0(m, nrow(bayesDF), "zcta_reri_bayes_resolved.png")), ### the tiger product
  png(file.path(outdir3,  paste0(m, nrow(bayesDF), "GID_", exposure_, "_bayes_resolved.png")),
      width = 6.5, height = 6, units = "in", res = 300)
  ## plot interpolated Surface
  par(mfrow = c(1,1))
  surf <- mba.surf(cbind(bayesDF[,.(long, lat)], bayesDF$w_hat_mu), no.X = 300, no.Y = 300, extend = TRUE,
                   b.box=c(min(coords$long), max(coords$long), min(coords$lat), max(coords$lat)))$xyz.est
  z.lim <- range(surf[[3]], na.rm = T)
  
  ## specifying colors for image.plot
  colorTable<- designer.colors(7, c("#3288BD", "#ABDDA4","#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F") )
  #, "#9E0142",  "brown", "darkred"
  # breaks with a gap of 10 to 17 assigned the white color
  brks<- c( -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1) 
  #, 1.2, 1.4, 1.6
  
  print(image.plot(surf, xaxs = "r", yaxs = "r", zlim = z.lim, breaks = brks, col=colorTable,  main = paste0(dataset, exposure_, "_bayes_", m, nrow(bayesDF))))
  print(plot(Mexico_mun, add = T, bg="grey"))
  dev.off()
  

  dt <- merge(shape, bayesDF, by="GID", all.y=TRUE) 
  

  png(file.path(outdir3,  paste0(m, nrow(bayesDF), "GID", exposure_, "_bayes_grey.png")),
      width = 6.5, height = 6, units = "in", res = 300)
  print(ggplot()  +
          geom_sf(data = dt, aes(fill = w_hat_mu), colour = NA)  +
          geom_polygon(data=Mexico_mun,aes(x=long, y=lat, group=group),
                       colour="black", fill = NA) +
          scale_fill_gradient2(low = "blue", mid = "white",
                               high="red", midpoint = 0, na.value = "darkgrey") +
          guides(fill = guide_colourbar(barheight = unit( 2.25 , "in" ),
                                        ticks.colour = "black",
                                        ticks.linewidth = 1, frame.colour = "black",
                                        frame.linewidth = 1))+
          labs(fill = exposure_) + theme( panel.background = element_rect(fill = 'lightgrey'),
                                        panel.grid = element_blank(),text = element_text(size=10),
                                        axis.title.x=element_blank(), axis.title.y = element_blank(),
                                        aspect.ratio = 1, axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))
  )
  dev.off()

  
    png(file.path(outdir3,  paste0(m, nrow(bayesDF), "GID", exposure_,  "_SNR_grey.png")),
      width = 6.5, height = 6, units = "in", res = 300)
  print(
    ggplot()  +
      geom_sf(data = dt, aes(fill = SNR), colour = NA)  +
     geom_polygon(data=Mexico_mun, aes(x=long, y=lat, group=group),
                   colour="black", fill = NA) +
      scale_fill_gradient2(low = "blue", mid = "white",
                           high="red", midpoint = 0, na.value = "darkgrey") +
      guides(fill = guide_colourbar(barheight = unit( 2.25 , "in" ),
                                    ticks.colour = "black",
                                    ticks.linewidth = 1, frame.colour = "black",
                                    frame.linewidth = 1))+
      labs(fill = "SNR") + theme(# panel.background = element_rect(fill = 'lightgrey'),
                                  panel.grid = element_blank(),text = element_text(size=10),
                                  axis.title.x=element_blank(), axis.title.y = element_blank(),
                                  aspect.ratio = 1, axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))
  )
  dev.off()
  
  
  if (sum(!is.na(bayesDF$truncSNR))>0) {
    png(file.path(outdir3,  paste0(m, nrow(bayesDF), "GID", exposure_, "_truncSNR_grey.png")),
        width = 6.5, height = 6, units = "in", res = 300)
    print(
      ggplot()  +
        geom_sf(data = dt, aes(fill = truncSNR), colour = NA)  +
        geom_polygon(data=Mexico_mun, aes(x=long, y=lat, group=group),
                     colour="black", fill = NA) +
        scale_fill_gradient2(low = "blue", mid = "white",
                             high="red", midpoint = 0, na.value = "darkgrey") +
        guides(fill = guide_colourbar(barheight = unit( 2.25 , "in" ),
                                      ticks.colour = "black",
                                      ticks.linewidth = 1, frame.colour = "black",
                                      frame.linewidth = 1))+
      #  ggtitle("99th maximum temperature")+
      #  ggtitle("95th maximum temperature")+
      #  ggtitle("95th minimum temperature")+

        ggtitle("99th minimum temperature")+
        labs(fill = "SNR") + theme( panel.background = element_rect(fill = 'lightgrey'),
                                    panel.grid = element_blank(),text = element_text(size=10),
                                    axis.title.x=element_blank(), axis.title.y = element_blank(),
                                    aspect.ratio = 1, axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))
    )
    dev.off()
  }
  ## binned estimates
  lv <- 7
  temp <-bayesDF$w_hat_mu
  intv <- c(floor(min(temp)*100)/100,
            round(quantile(temp, probs=c(1:(lv-1))/lv), digits=2),
            ceiling(max(temp)*100)/100)
  if (max(intv)*min(intv)<0) {  # force to have 0 in the intervals
    loc <- which(abs(intv)==min(abs(intv)))
    if (loc==1) {
      loc <- 2
    } else if (loc==8) {
      loc <- 7
    }
    intv[loc] <- 0
  }
  intv_lable <- sapply(1:lv, function(a) {
    paste(intv[a], intv[a+1], sep=" to ")
  })
  bayesDF$cat <- cut(bayesDF[, w_hat_mu], breaks = intv, include.lowest = TRUE)
  levels(bayesDF$cat) <- intv_lable

  
  dt <- merge(shape, bayesDF, by = "GID" )
  dt$cat <- as.character(dt$cat)
  dt$cat[is.na(dt$cat)] <- "Not used"
  dt$cat <- factor(dt$cat, levels = c(intv_lable, "Not used"))
  
  if (max(intv)*min(intv)<0) {
    col.values <- c(rev(brewer.pal(n = max(sum(intv<0)+1, 4), name = "Blues")[2:(sum(intv<0)+1)]),
                    brewer.pal(n = max(sum(intv>0), 3), name = "YlOrRd")[1:sum(intv>0)],
                    "darkgrey")
  } else if (max(intv)>0) {
    col.values <- c(brewer.pal(n = length(temp), name = "YlOrRd"),
                    "darkgrey")
  } else if (max(intv)<=0) {
    col.values <- c(rev(brewer.pal(n = length(temp)+1, name = "Blues")[-1]),
                    "darkgrey")
  }

    png(file.path(outdir3,  paste0(m, nrow(bayesDF), "GID", exposure_, "_bayes_bin.png")),
      width = 6, height = 5, units = "in", res = 300)
  print(ggplot()  +
          geom_sf(data = dt, aes(fill = cat), colour = NA)  +
          geom_polygon(data=Mexico_mun, aes(x=long, y=lat, group=group),
                       colour="black", fill = NA) +
          scale_fill_manual(values = col.values, name="95th %ile Tmin") +
          guides(fill = guide_legend(barheight = unit( 2.25 , "in" ),
                                     ticks.colour = "black",
                                     ticks.linewidth = 1, frame.colour = "black",
                                     frame.linewidth = 1))+
          #ggtitle("Bayesian resolved estimates of ERR")+
          labs(fill = exposure_) + theme( panel.grid = element_blank(),text = element_text(size=10),
                                        axis.title.x=element_blank(), axis.title.y = element_blank(),
                                        aspect.ratio = 1, axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))
  )
  dev.off()
  
  
}
}

sink()

#-------------------------------------Meta-regression-----------------------------#
## This is using the census data for Mexico at the municipality level for 2010
## From INEGI
## Code to clean census data stored in the following folder
## D:\Lara\Border\Code\R_shapefiles_mexico\Mexico_census_data.R

### Data prep ###

outdir1<-"D:/Lara/Mexico_heat_2024/RR"

exposure_<-("hw_95_min")
m<- c("month_wt")
methods <- c("month_wt")

## read in variables for meta regression
census <- read.csv("D:/Lara/Border/Data/Mexico_social/Mex_census_2010.csv")
## some variables have negative values- change these to 0
census$VIV30_R[census$VIV30_R < 0] <- 0
census$VIV15_R[census$VIV15_R < 0] <- 0
census$ECO25_R[census$ECO25_R < 0] <- 0
census$POB31_R[census$POB31_R < 0] <- 0
#census$ac_sh_state_wgt[census$ac_sh_state_wgt < 0] <- 0
#is.na(census$ac_sh_state_wgt) <- 0


## Census variables
#POB1: Población total
#POB31_R: Porcentaje de población femenina
#ECO25_R: Porcentaje de población desocupad
#ECO26_R: Porcentaje de población femenina desocupada
#ECO27_R: Porcentaje de población masculina desocupada
#EDU31_R: Porcentaje de población de 15 años y más sin escolaridad
#EDU28_R: Porcentaje de población de 15 años y más analfabeta
#EDU49_R: Grado promedio de escolaridad
#VIV13_R: Porcentaje de viviendas particulares habitadas con más de 3 ocupantes por cuarto
#VIV15_R: Porcentaje de viviendas particulares habitadas que no disponen de luz eléctrica
#VIV30_R: Porcentaje de viviendas particulares habitadas que no disponen de refrigerador, lavadora ni automóvil o camioneta
#VIV41_R: Porcentaje de viviendas particulares habitadas sin ningún bien
#POB24_R: Porcentaje de población de 65 años y más
#POB30_R: Edad mediana
#MIG7_R: Porcentaje de población nacida en otro país
#DISC1_R: Porcentaje de población con discapacidad.
#INDI13_R: Porcentaje de población de 5 años y más que habla alguna lengua indígena y no habla español
#GRADO_MARG_NUM: Grado de Marginación CONAPO in numeric form (1- muy bajo, 2-bajo, 3-medio, 4-alto, 5-muy alto)
#ac_sh_state_wgt: AC estimates 
mun_vbs <- c("POB1", "POB31_R", "ECO25_R", "EDU31_R","EDU28_R",
             "EDU49_R","VIV13_R", "VIV15_R", "VIV30_R","VIV41_R", "POB24_R","POB30_R", 
              "GRADO_MARG_NUM", "ac_sh_state_wgt" )



# ## read in raw estimates before bayesian spatial pooling
 bar <- fread(file.path(outdir1, "results", paste0(dataset, "_specific.csv")))
 
 
### meta-regression analysis
 
out <- numeric()
for (m in methods) {
  
  for (exposure_ in c( "hw_95_min")) {
   # , "hw_95", "hw_99"
  

   bayesDF <- fread(file.path(outdir1, "results", paste0( exposure_, m, "1740", "GID.csv"))) 
  
   bayesDF <- merge(bayesDF, census, by="GID", all.x=TRUE)
   bayesDF <- bayesDF[!is.na(bayesDF$long), ]
  
res_cen <- merge(bar, census, by="GID", all.x=TRUE)
 

  
  nms <- paste(rep(c("lm", "meta"), each=2), rep(c("coef", "se"), time=2), sep="_")
  estimate <- data.frame(vb=mun_vbs,
                         min=NA, q1=NA, median=NA, q3=NA, max=NA, 
                         setNames(replicate(length(nms), NA, simplify = FALSE), nms))
  for (i in 1:nrow(estimate)) {
    ## linear regression of associations after bayesian spatial pooling
    g <- lm(as.formula(paste0("w_hat_mu ~ ", estimate$vb[i])), data=bayesDF)
    g <- lm(as.formula(paste0("month_wt_rr_hw_95_min ~ ", estimate$vb[i])), data=res_cen)
    #g <- lm(as.formula(paste0(methods, exposure_, " ~ ", estimate$vb[i])), data=res_cen)
    estimate[i, 2:6] <- quantile(bayesDF[, eval(as.name(estimate$vb[i]))], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE)
    estimate[i, nms[1:2]] <- summary(g)$coefficients[2, 1:2]
    
  
    
    ## meta-regression after bayesian spatial pooling
   m2 <- metagen(TE = w_hat_mu, seTE = w_hat_sd, studlab = GID, data = bayesDF)
   g2 <- metareg(m2, as.formula(paste0(" ~ ", estimate$vb[i])))
    estimate[i, nms[3:4]] <- c(g2$beta[2], g2$se[2])
  }
  estimate$iqr <- estimate$q3 - estimate$q1
  temp <- data.frame(method=m, GID5.used=nrow(bayesDF))
  temp <- cbind(rep(temp), estimate)
  out <- rbind(out, temp)
  estimate <- nms <- g <- g2 <- m2 <- temp <- NULL
}
}

write.csv(out, file.path(outdir1, "results",  paste0( exposure_, "_metaregression.csv")), row.names = FALSE)

### Making graphs ###

out<- read.csv(file.path(outdir1, "results",  paste0(dataset, exposure_, "_metaregression.csv")), row.names = FALSE)


out$meta_lci= out$meta_coef-(1.96*out$meta_se)
out$meta_uci= out$meta_coef+(1.96*out$meta_se)

out$lm_lci= out$lm_coef-(1.96*out$lm_se)
out$lm_uci= out$lm_coef+(1.96*out$lm_se)


## graphing meta-regression results
c("POB1", "ECO25_R","ECO26_R", "ECO27_R", "EDU31_R","EDU28_R",
  "EDU49_R","VIV13_R", "VIV15_R", "VIV30_R","VIV41_R", "POB24_R","POB30_R", 
  "GRADO_MARG_NUM", "ac_sh_state_wgt" )


out$vb<-recode_factor(out$vb, POB1="population",
                      POB31_R="% women",
              ECO25_R="% unemployed", 
              EDU31_R="% without schooling",
              EDU28_R="% illiterate",
              EDU49_R="median schooling",
              VIV13_R="overcrowding", 
              VIV15_R="% no electricity", 
              VIV30_R="% no refrigerator or tv",
              VIV41_R="% no amenities", 
              POB24_R="% 65+ years",
              POB30_R="median age", 
              GRADO_MARG_NUM ="marginalization",
              ac_sh_state_wgt= "AC")


p <- 
  out |>
  ggplot(aes(y = fct_reorder(vb, meta_coef))) + 
  theme_bw()+
  theme(plot.margin = unit(c(1.5,0.2,0.2,0.2), "lines"), axis.text=element_text(size=12))+
  geom_point(aes(x=meta_coef), shape=15, size=3) +
  geom_linerange(aes(xmin=meta_lci, xmax=meta_uci)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(x="Change in Excess Risk Ratio", y="For IQR increase in:", size=12)+
  coord_cartesian(ylim=c(1,14), xlim=c(-0.04, .04), clip="off")+
  #coord_cartesian(ylim=c(1,14), xlim=c(-0.02, .02), clip="off")+
  
  annotate("text", x = -.02, y = 15, label = "Less effects of heat", color="blue", fontface = "bold", size=5) +
  annotate("text", x = .02, y = 15, label = "Greater effects of heat", color="red", fontface = "bold", size=5)

p
