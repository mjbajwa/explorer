# Load Libs ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(tmap)
library(ggthemes)
library(viridis)
library(rasterVis)
library(ggspatial)
library(raster)
library(tmap)

# Load Data ---------------------------------------------------------------

files <- list.files(path = "./data/drilling_database")
files <- files[!(files %>% str_detect("geojson"))] # leave json file out of the loop

for(i in 1:length(files)){
  name <- str_remove(files[[i]], ".csv")
  assign(name, value = readr::read_csv(file = paste0("./data/drilling_database/",files[[i]])))
}

# Analyze Each Data Table -------------------------------------------------

alteration %>% glimpse()
alteration_codes %>% glimpse()
assays %>% glimpse()
collars %>% glimpse() # contains easting and northing data - worth plotting

ggplot(collars) + 
  geom_point(aes(x = East, y = North, color = Azimuth))

lithology %>% glimpse()
lithology_codes %>% glimpse()
magnetic_susceptibility %>% glimpse()
rock_quality %>% glimpse()
specific_gravity %>% glimpse()
structure %>% glimpse()
survey %>% glimpse()

# Load Tiffs for a Given Map ---------------------------------------------

files <- list.files(path = "./data/train/")
name <- files[[7]]
files.nest <- lapply(as.list(files), function(x){list.files(path = paste0("./data/train/", x))})
crs <- readr::read_file(paste0("./data/train/", name, "/proj4_projection_definition.txt"))

# Geophysics ------------------------------------------------------------------------

breakname <- paste0("./data/train/", name, "/geophysics/")

res_gravity_bouger <- raster::raster(paste0(breakname, "gravity/bouger_gravity_anomaly.tif"))
res_gravity_residual <- raster::raster(paste0(breakname, "gravity/isostatic_residual_gravity_anomaly.tif"))
res_magnetics_intensity <- raster::raster(paste0(breakname,"magnetics/total_magnetic_intensity.tif"))
res_reduction_pole <- raster::raster(paste0(breakname,"magnetics/variable_reduction_to_pole.tif"))
res_potassium <- raster::raster(paste0(breakname, "radiometrics/filtered_potassium_pct.tif"))
res_terrestrial <- raster::raster(paste0(breakname,"radiometrics/filtered_terrestrial_dose.tif"))
res_thorium <- raster::raster(paste0(breakname, "radiometrics/filtered_thorium_ppm.tif"))
res_uranium <- raster::raster(paste0(breakname, "radiometrics/filtered_uranium_ppm.tif"))
raster::crs(res_gravity_bouger) <- crs
raster::crs(res_gravity_residual) <- crs
raster::crs(res_magnetics_intensity) <- crs
raster::crs(res_reduction_pole) <- crs
raster::crs(res_terrestrial) <- crs
raster::crs(res_thorium) <- crs
raster::crs(res_potassium) <- crs
raster::crs(res_uranium) <- crs

# Sample Stacking of Rasters into multi dimensional arrays

k_geophysics <- stack(
  res_gravity_bouger,
  res_gravity_residual,
  res_magnetics_intensity,
  res_reduction_pole,
  res_potassium,
  res_terrestrial,
  res_thorium,
  res_uranium
)

raster::crs(k_geophysics) <- crs

# Analyze the rasters

tmap_mode("plot")

geophysics_plot <- tm_shape(k_geophysics) + 
  tm_raster() + 
  tm_facets(as.layers = TRUE) + 
  tm_layout(main.title = paste0("Geophysics Data - ", name))

# Remote Sensing Data ---------------------------------------------------

breakname <- paste0("./data/train/", name, "/remote_sensing/aster/")
list.names <- list.files(breakname)
k_rs <- NULL

for(i in 1:length(list.names)){
  assign(list.names[[i]] %>% str_remove(".tif"), raster::raster(paste0(breakname, list.names[[i]])))
  if(i == 1){
    k_rs <- stack(get(list.names[[i]] %>% str_remove(".tif")))
  } else {  
    k_rs <- stack(k_rs, get(list.names[[i]] %>% str_remove(".tif")))
  }
}

raster::crs(k_rs) <- crs

tmap_mode("plot")

remote_sensing_plot <- tm_shape(k_rs) + 
  tm_raster() + 
  tm_facets(as.layers = TRUE) + 
  tm_layout(main.title = paste0("Remote Sensing Data - ", name))

combined_array <- abind::abind(as.array(k_geophysics), as.array(k_rs))
# write_rds(combined_array, path = paste0("./data/train/", name, "/combined_array.rds"))

# Geo.JSON Data --------------------------------------------------------

library(geojsonsf)

breakname <- paste0("./data/train/", name, "/geology/")
list.names <- list.files(breakname)

for(i in 1:length(list.names)){
  assign(list.names[[i]] %>% str_remove(".geo.json"), geojson_sf(paste0(breakname, list.names[[i]])))
}

tmap_mode("view")
tm_shape(shears_and_faults) + 
  tm_lines()


# Load Output ------------------

output <- read_csv(paste0("./data/train/", name, "/commodities.csv"))
