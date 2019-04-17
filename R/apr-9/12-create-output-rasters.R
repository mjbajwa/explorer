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
library(RColorBrewer)

# Load and Convert Outputs ------------------------------------------------

#' We want to be able to predict all output classes. 

op_results <- list.files(path = "./data/train/train/") %>% 
  purrr::map(.f = function(x){
    s <- read_csv(file = paste0("./data/train/train/", x,"/commodities.csv"), col_types = "cccc")
    return(s)}) %>% 
  bind_rows()

op_results$commodity %>% str_flatten(collapse = ";") %>% str_replace_all(";", " ") %>% tibble()

# create function to generate redundant rows if multiple commodities are present in the same location

make_redundant_rows <- function(s){
  
  count <- str_count(s$commodity, ";") # count + 1 = number of unique commodities
  string <- rep("(.*?);", count+1) %>% str_flatten() # generates regex string to match files 
  temp <- s$commodity %>% c(";") %>% str_flatten() %>% str_match(pattern = string) %>% as.character()
  temp <- temp[-1]
  print(temp)
  temp.df <- tibble(stamp_id = rep(s$stamp_id, count+1), 
                    x = rep(s$x, count+1), 
                    y = rep(s$y, count+1), 
                    commodity = temp)
  return(temp.df)
  
}

s <- op_results %>% 
  mutate(flag = str_count(commodity, ";")) %>% 
  arrange(stamp_id) %>% 
  filter(flag >= 1) %>% 
  sample_n(1)

make_redundant_rows(s) # unit test

# Split-Apply-Reduce Workflow ---------------------------------------------

df <- op_results 
df.dirty <- op_results %>% filter(str_count(commodity, ";") >= 1)
df.clean <- op_results %>% filter(str_count(commodity, ";") == 0)

# Iterate over all "dirty" rows

temp <- list()

for(i in 1:nrow(df.dirty)){
  temp[[i]] <- make_redundant_rows(s = df.dirty[i, ])
}

df.temp <- bind_rows(temp)
df.final <- bind_rows(df.temp, df.clean) # final df that is important

df.final$commodity %>% unique() %>% sort()

# Create Grid -------------------------------------------------------------

files <- list.files(path = "./data/train/train/")
df.final <- df.final %>% mutate(value = 1) %>% mutate_at(vars(x:y), funs(as.numeric)) %>% dplyr::select(-commodity)

write_output_mask <- function(name){
  
  message(paste0("Running for File = ", name, "\n"))

  temp <- df.final %>% filter(stamp_id == name) %>% distinct() 
  crs <- readr::read_file(paste0("./data/train/train/", name, "/proj4_projection_definition.txt"))
  output.sf <- st_as_sf(temp, coords = c("x", "y"), crs = crs)
  
  # Load Template Raster
  
  breakname <- paste0("./data/train/train/", name, "/geophysics/")
  raster_template <- raster::raster(paste0(breakname,"magnetics/total_magnetic_intensity.tif"))

  # Rasterize Output

  if(nrow(output.sf) == 0){
    
    outputs_rst <- raster_template %>% aggregate(fact = 500/128, fun = max)
    outputs_rst[] <- 0
    outputs_rst_array <- outputs_rst_aggregate %>% as.array()
  
    } else {
    
    raster_template2 <- raster(extent(raster_template), resolution = res(raster_template),
                               crs = st_crs(output.sf)$proj4string)
    
    outputs_rst <- rasterize(output.sf %>% dplyr::select(-stamp_id), raster_template2, field = "value") 
    outputs_rst[is.na(outputs_rst[])] <- 0 
    
    # tm_shape(outputs_rst) +
    #   tm_raster(palette = "viridis") +
    #   tm_shape(output.sf) +
    #   tm_dots(size = 1, col = "blue2", alpha = 0.5)
    # 
    # cuts = c(0,0.9,1) #set breaks
    # pal <- colorRampPalette(c("white","black"))
    # 
    # plot(outputs_rst, breaks = cuts, col = pal(3), size = ) #plot with defined breaks
    
    # Aggregate -----------------------------
    
    outputs_rst_aggregate <- outputs_rst %>% aggregate(fact = 500/128, fun = max)
    outputs_rst_array <- outputs_rst_aggregate %>% as.array()
    
    # tm_shape(outputs_rst_aggregate) +
    #   tm_raster(palette = "viridis") +
    #   tm_shape(output.sf) +
    #   tm_dots(size = 1, col = "blue2", alpha = 0.5)
  
  }

  # Save ---------------------------------

  write_rds(outputs_rst_array, path = paste0("./data/train/train/", name, "/output_mask.rds"))

}

re_write <- T

if(re_write){
  for(i in 1:length(files)){
    name <- files[[i]]
    tryCatch({
      write_output_mask(name = name)
    }, 
    error = function(){})
  }
}



