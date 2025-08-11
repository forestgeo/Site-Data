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

message("✅  Whittaker diaagram saved: ForestGEO_Whittaker_Plot.png'")


## 6. CREATE & SAVE CLIMATE MAPS
# prepare site coordinates
site_coords <- site_data_filled %>%
  dplyr::select(Longitude, Latitude) %>%
  na.omit()

# Plot global temperature map with site points
# Convert rasters to data frames
temp_raster <- climate_stack[[1]]
temp_df <- as.data.frame(temp_raster, xy = TRUE)
names(temp_df)[3] <- "MAT"
temp_df$MAT <- temp_df$MAT / 10  # Convert from tenths of °C to °C

prec_raster <- climate_stack[[2]]
prec_df <- as.data.frame(prec_raster, xy = TRUE)
names(prec_df)[3] <- "MAP"

# create plots 
## temperature
gg_temp <- ggplot() +
  geom_raster(data = temp_df, aes(x = x, y = y, fill = MAT)) +
  scale_fill_viridis_c(name = "Temp (°C)", na.value = "transparent") +
  geom_point(data = site_coords, aes(x = Longitude, y = Latitude), color = "red", size = 2) +
  labs(title = "Global Mean Annual Temperature with ForestGEO Sites",
       x = "Longitude", y = "Latitude") +
  coord_fixed() +
  theme_minimal()

## precipitation 
# clip high values for visualization
prec_df$MAP_capped <- pmin(prec_df$MAP, 4500)  # cap at 5000 mm


gg_prec <- ggplot() +
  geom_raster(data = prec_df, aes(x = x, y = y, fill = MAP_capped)) +
  scale_fill_viridis_c(
    name = "Precip (mm)",
    na.value = "transparent"
  ) +
  geom_point(data = site_coords, aes(x = Longitude, y = Latitude), color = "red", size = 2) +
  labs(
    title = "Global Mean Annual Precipitation with ForestGEO sites",
    x = "Longitude", y = "Latitude"
  ) +
  coord_fixed() +
  theme_minimal()

# save plots 
ggsave("global_temperature_map.png", plot = gg_temp, width = 10, height = 6, dpi = 300)
ggsave("global_precipitation_map.png", plot = gg_prec, width = 10, height = 6, dpi = 300)

message("✅ Climate maps saved: 'global_temperature_map.png' and 'global_precipitation_map.png'")
