### script to make Whittaker plot of ForestGEO sites
### Kristina Anderson-Teixeira
### August 2025

## 1. INSTALL AND LOAD NECESSARY PACKAGES
#install.packages("readr")
#install.packages("devtools")
#devtools::install_github("valentinitnelav/plotbiomes") 
#install.packages("ggplot2")

library(readr)
library(plotbiomes)
library(ggplot2)
library(raster)
library(dplyr)
library(sp)

## 2. SET WORKING DIRECTORY
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## 3. READ FORESTGEO SITE DATA FROM GITHUB (ForestGEO_site_data.csv; this repository)

# Read ForestGEO_site_data.csv file directly from GitHub
site_data <- read_csv("https://raw.githubusercontent.com/forestgeo/Site-Data/master/basic%20site%20data/ForestGEO_site_data.csv")


## 4. FILL MISSING MAT AND MAP FROM RASTER BASED ON LAT/LONG
raster_path <- system.file("extdata", "temp_pp.tif", package = "plotbiomes")
climate_stack <- raster::stack(raster_path)
names(climate_stack) <- c("temperature", "precipitation")  # °C x10, mm

fill_missing_climate <- function(df, raster_stack) {
  if (!all(c("Latitude", "Longitude") %in% names(df))) {
    stop("Missing 'Latitude' or 'Longitude' columns.")
  }
  
  # Prepare coordinates
  coords <- df %>% select(Longitude, Latitude)
  sp_points <- sp::SpatialPoints(coords, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  
  # Extract raster values (temperature and precipitation)
  extracted <- raster::extract(raster_stack, sp_points)
  
  # Fill missing values only
  if ("MAT" %in% names(df)) {
    df$MAT[is.na(df$MAT)] <- extracted[is.na(df$MAT), "temperature"] / 10  # °C
  } else {
    df$MAT <- extracted[, "temperature"] / 10
  }
  
  if ("MAP" %in% names(df)) {
    df$MAP[is.na(df$MAP)] <- extracted[is.na(df$MAP), "precipitation"]     # mm
  } else {
    df$MAP <- extracted[, "precipitation"]
  }
  
  return(df)
}

# Apply the function to fill missing MAT and MAP
site_data_filled <- fill_missing_climate(site_data, climate_stack)


## 5. CREATE & SAVE WHITTAKER PLOT

whittaker_plot<- whittaker_base_plot() +
  # add the temperature - precipitation data points
  geom_point(data = site_data_filled, 
             aes(x = MAT, 
                 y = MAP/10), # division by 10 conversts mm to cm
             size   = 3,
             shape  = 21,
             colour = "gray95", 
             fill   = "black",
             stroke = 1,
             alpha  = 0.5) +
  theme_bw()

ggsave("ForestGEO_Whittaker_Plot.png", plot = whittaker_plot,
       width = 10, height = 7, dpi = 300)
