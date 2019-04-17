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
library(geojsonsf)
library(jsonlite)
library(gridExtra)

# Load Outputs to see which ones have data available -----------------------------------------------

df.final <- read_rds("./cache/df_final.rds")
names.list <- df.final$stamp_id %>% unique()
names.samples <- names.list %>% sample(9)

generate_images <- function(name){

  outputs <- df.final %>% filter(stamp_id == name) %>% mutate_at(vars(x,y), as.numeric)
  op_sf <- st_as_sf(outputs, coords = c("x", "y"), crs = 4326)
  
  # Load Input Data ---------------------------------------------------------------
  
  files <- list.files(path = "./data/train/train/")
  # name <- files[[150]]
  breakname <- paste0("./data/train/train/", name, "/geology/")
  list.names <- list.files(breakname)
  k_geology <- NULL
    
  for(i in 1:length(list.names)){
      assign(list.names[[i]] %>% str_remove(".geo.json"), 
             geojsonsf::geojson_sf(paste0("./data/train/train/", name, "/geology/", list.names[[i]])))
  }
  
  # Create Combined Plot ----------------------------------------------------
  
  ggplot() + 
    geom_sf(data = unit_contacts, color = "red2") +
    # geom_sf(data = geological_unit_polygons, color = "blue2", alpha = 0.2) + 
    geom_sf(data = shears_and_faults, color = "green3") + 
    geom_sf(data = misc_lines, color = "gold2") + 
    geom_sf(data = op_sf, color = "black", size = 2) + 
    theme_bw() + 
    ggtitle(label = name)

}

results <- purrr::map(names.samples, generate_images)
n <- length(results)
nCol <- floor(sqrt(n))
grDevices::dev.new()
do.call("grid.arrange", c(results, ncol = nCol))

# Convert Sample SF Polygons to Raster Grids ------------------------------

name <- names.samples[[3]]

write_geo_data_to_disk <- function(name){
  # name <- "00132399"
  message(paste0("Running for File = ", name, "\n"))

  # Create Template
  
  breakname <- paste0("./data/train/train/", name, "/geophysics/")
  raster_template <- raster::raster(paste0(breakname,"magnetics/total_magnetic_intensity.tif"))
  
  # Load Geology Files
  
  breakname <- paste0("./data/train/train/", name, "/geology/")
  list.names <- list.files(breakname)
  crs <- readr::read_file(file = paste0("./data/train/train/", name, "/proj4_projection_definition.txt"))
  
  for(i in 1:length(list.names)){
    temp <- geojsonsf::geojson_sf(paste0("./data/train/train/", name, "/geology/", list.names[[i]]))
    temp <- st_set_crs(temp, crs)
    assign(list.names[[i]] %>% str_remove(".geo.json"), temp)
  }
  
  # unit_contacts conversion -------------
  
  # unit_contacts <- unit_contacts %>% mutate(value = 1) %>% dplyr::select(geometry, value)
  # unit_contacts_cast <- st_cast(unit_contacts, to = "MULTILINESTRING") %>% st_cast("MULTIPOLYGON")
  # raster_template2 <- raster(extent(raster_template), resolution = res(raster_template),
  #                           crs = st_crs(unit_contacts_cast)$proj4string)
  # unit_contacts_rst <- fasterize::fasterize(unit_contacts_cast, raster_template2, field = "value") 
  # unit_contacts_rst <- rasterize(unit_contacts_cast, raster_template2, field = "value")
  # unit_contacts_rst[is.na(unit_contacts_rst[])] <- 0 
  # unit_contacts_rst %>% as.array() %>% dim()
  # 
  # # Aggregate 
  # 
  # unit_contact_aggregate <- unit_contacts_rst %>% aggregate(fact = 500/128, fun = max)
  # unit_contact_aggregate %>% as.array() %>% dim()
  
  # View Plot for Comparison
  
  # par(mfrow = c(1,2))
  # plot(unit_contacts_rst)
  # plot(unit_contact_aggregate)
  
  # Shears & Faults Conversion ---------------------------------------------------
  
  shears_and_faults <- shears_and_faults %>% mutate(value = 1) %>% dplyr::select(geometry, value)
  shears_and_faults_cast <- st_cast(shears_and_faults, to = "MULTILINESTRING")
  raster_template2 <- raster(extent(raster_template), resolution = res(raster_template),
                             crs = st_crs(shears_and_faults_cast)$proj4string)
  
  if(nrow(shears_and_faults_cast) > 0){
    shears_and_faults_rst <- rasterize(shears_and_faults_cast, raster_template2, field = "value") 
    shears_and_faults_rst[is.na(shears_and_faults_rst[])] <- 0 
  } else {
    shears_and_faults_rst[] <- 0
  }
  
  # Aggregate 
  
  shears_and_faults_aggregate <- shears_and_faults_rst %>% aggregate(fact = 500/128, fun = max)
  snf_array <- shears_and_faults_aggregate %>% as.array()
  write_rds(snf_array, path = paste0("./data/train/train/", name, "/input_geology_shears_2.rds"))
 
  # Visualize

  # par(mfrow = c(1,2))
  # plot(shears_and_faults_rst)
  # plot(shears_and_faults_aggregate)
   
}

files <- list.files(path = "./data/train/train/")

re_write <- T

if(re_write){
  for(i in 1:length(files)){
    name <- files[[i]]
    tryCatch({
      write_geo_data_to_disk(name = name)
    }, 
    error = function(){})
  }
}


