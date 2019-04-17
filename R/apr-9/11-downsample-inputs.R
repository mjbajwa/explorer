#' In this script, I am going to downsample the inputs to a more reasonable 128x128 raster so 
#' we end up with (128x128x19 == 350k) gridded inputs as opposed to dealing with insane sized inputs
#' (500x500x19 = 4.75 million) 

#' we need to analyze the samples to be sure that downsample does not materially cause a loss of info
#' aggregation factor = 3.90625

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

options(device = "RStudioGD")

# Load Data ---------------------------------------------------------------

# Load Tiffs for a Given Map ---------------------------------------------

files <- list.files(path = "./data/train/train/")
name <- files[[124]]

write_inputs <- function(name, write_to_disk = T, return_object = F){
  
  message(paste0("Running for File = ", name, "\n\n"))
  
  files.nest <- lapply(as.list(files), function(x){list.files(path = paste0("./data/train/train/", x))})
  crs <- readr::read_file(paste0("./data/train/train/", name, "/proj4_projection_definition.txt"))
  
  # Geophysics ------------------------------------------------------------------------
  
  breakname <- paste0("./data/train/train/", name, "/geophysics/")
  
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
  k_geophysics_aggregated <- aggregate(k_geophysics, fact = 500/128)
  
  # Analyze the rasters
  
  # tmap_mode("plot")
  # 
  # tm_shape(k_geophysics) +
  #   tm_raster() +
  #   tm_facets(as.layers = TRUE) +
  #   tm_layout(main.title = paste0("Geophysics Data - ", name))
  # 
  # tm_shape(k_geophysics_aggregated) +
  #   tm_raster() +
  #   tm_facets(as.layers = TRUE) +
  #   tm_layout(main.title = paste0("Geophysics Data - ", name))
  
  # Remote Sensing Data ---------------------------------------------------
  
  breakname <- paste0("./data/train/train/", name, "/remote_sensing/aster/")
  list.names <- list.files(breakname)
  k_rs <- NULL
  
  if(!(identical(list.names, character(0)))){
    
    for(i in 1:length(list.names)){
      temp <- raster::raster(paste0(breakname, list.names[[i]]))
      crs(temp) <- crs
      assign(list.names[[i]] %>% str_remove(".tif"), temp)
      if(i == 1){
        k_rs <- stack(get(list.names[[i]] %>% str_remove(".tif")))
      } else {  
        k_rs <- stack(k_rs, get(list.names[[i]] %>% str_remove(".tif")))
      }
    }
    
    raster::crs(k_rs) <- crs
    
    k_rs_aggregated <- aggregate(k_rs, fact = 500/128, fun = mean)
    
    # tm_shape(k_rs) +
    #   tm_raster(palette = "viridis") +
    #   tm_facets(as.layers = TRUE) +
    #   tm_layout(main.title = paste0("Remote Sensing Data - ", name)) #+
    # 
    # tm_shape(k_rs_aggregated) +
    #   tm_raster(palette = "viridis") +
    #   tm_facets(as.layers = TRUE)
    
    if(write_to_disk == T){
      combined_array <- abind::abind(as.array(k_geophysics_aggregated), as.array(k_rs_aggregated))
      # write_rds(combined_array, path = paste0("./cache/input_combined_sample_2.rds"))
      write_rds(combined_array, path = paste0("./data/train/train/", name, "/input_combined_array_g_rm_2.rds"))
    }
    
  }
  
  if(return_object == T){
    return(list(k_geophysics = k_geophysics_aggregated, k_rs = k_rs_aggregated))
  }
  
}

# INPUTS HAVE NOW CHANGED TO "input_combined_array_g_rm_2"

re_write <- T
error <- c()
j <- 1

if(re_write){
  for(i in 1:length(files)){
    name <- files[[i]]
    tryCatch({
      write_inputs(name = name)
    }, 
    error = function(){error[[j]] <- name; j <- j + 1})
  }
}
