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
  
  # Analyze the rasters
  
  # tmap_mode("plot")
  # 
  # geophysics_plot <- tm_shape(k_geophysics) +
  #   tm_raster() +
  #   tm_facets(as.layers = TRUE) +
  #   tm_layout(main.title = paste0("Geophysics Data - ", name))
  
  # Remote Sensing Data ---------------------------------------------------
  
  breakname <- paste0("./data/train/train/", name, "/remote_sensing/aster/")
  list.names <- list.files(breakname)
  k_rs <- NULL
  
  if(!(identical(list.names, character(0)))){
  
  for(i in 1:length(list.names)){
    assign(list.names[[i]] %>% str_remove(".tif"), raster::raster(paste0(breakname, list.names[[i]])))
    if(i == 1){
      k_rs <- stack(get(list.names[[i]] %>% str_remove(".tif")))
    } else {  
      k_rs <- stack(k_rs, get(list.names[[i]] %>% str_remove(".tif")))
    }
  }
  
  raster::crs(k_rs) <- crs
  
  if(write_to_disk == T){
      combined_array <- abind::abind(as.array(k_geophysics), as.array(k_rs))
      write_rds(combined_array, path = paste0("./data/train/train/", name, "/input_combined_array.rds"))
    }
  
  }
  
  if(return_object == T){
    return(list(k_geophysics = k_geophysics, k_rs = k_rs))
  }

}

# lapply(files, write_inputs) # Run this once and you should never need to run this again

re_write <- F

if(re_write){
  for(i in 1:length(files)){
    name <- files[[i]]
    tryCatch({
    write_inputs(name = name)
    }, 
    error = function(){})
  }
}

# Geo.JSON Data --------------------------------------------------------

# library(geojsonsf)
# 
# breakname <- paste0("./data/train/", name, "/geology/")
# list.names <- list.files(breakname)
# 
# for(i in 1:length(list.names)){
#   assign(list.names[[i]] %>% str_remove(".geo.json"), geojson_sf(paste0(breakname, list.names[[i]])))
# }
# 
# tmap_mode("view")
# tm_shape(shears_and_faults) + 
#   tm_lines()

# Load and Convert Output to useful form ------------------

write_images_io <- function(name){
  
  message(paste0("Running for File = ", name, "\n"))
  
  crs <-
    readr::read_file(paste0(
      "./data/train/train/",
      name,
      "/proj4_projection_definition.txt"
    ))
  
  output <- read_csv(paste0("./data/train/train/", name, "/commodities.csv"))
  
  if(!(nrow(output) == 0)){
    
    output.sf <- st_as_sf(output, coords = c("x", "y"), crs = crs)
    
    inputs <- write_inputs(name = name,
                   write_to_disk = F,
                   return_object = T)
    
    geophysics_plot <- tm_shape(inputs$k_geophysics) +
      tm_raster(palette = "viridis") +
      tm_shape(output.sf) +
      tm_dots(size = 2, labels = "commodity") +
      tm_facets(as.layers = TRUE) +
      tm_layout(main.title = paste0("Geophysics Data - ", name))
    
    remote_sensing_plot <- tm_shape(inputs$k_geophysics) +
      tm_raster(palette = "viridis") +
      tm_shape(output.sf) +
      tm_dots(size = 3, labels = "commodity") +
      tm_facets(as.layers = TRUE) +
      tm_layout(main.title = paste0("Remote Sensing Data - ", name))
    
    tmap_save(
      tm = geophysics_plot,
      filename = paste0("./images/", name, "_geophysics.jpg"),
      width = 8.5,
      height = 11,
      units = "in"
    )
    
    tmap_save(
      tm = remote_sensing_plot,
      filename = paste0("./images/", name, "_rm_sensing.jpg"),
      width = 8.5,
      height = 11,
      units = "in"
    )
  
  }
  
}

re_write_op <- F

if(re_write_op == T){
  for(i in 1:length(files)){
    name <- files[[i]]
    tryCatch({
      write_images_io(name = name)
    }, 
    error = function(){})
  }
}

# load all commodities and append to each other

all.outputs <- list.files(path = "./data/train/train/") %>% 
  purrr::map(.f = function(x){read_csv(file = paste0("./data/train/train/", x,"/commodities.csv"), col_types = "cccc")
    }) %>% 
  bind_rows()

rand <- all.outputs %>% slice(n = 962)

# Load the total number of rows in each OUTPUT table --------------------------

op_results <- list.files(path = "./data/train/train/") %>% 
  purrr::map(.f = function(x){
    s <- read_csv(file = paste0("./data/train/train/", x,"/commodities.csv"), col_types = "cccc")
    return(nrow(s))}
  )

op.df <- tibble(output_stamp = list.files(path = "./data/train/train") %>% unlist(), 
                results = op_results %>% unlist()) %>% 
  mutate(results = ifelse(results > 0, 1, 0))

ggplot(op.df %>% mutate(results = factor(results))) + 
  geom_histogram(aes(x = results, fill = results), stat = "count", color = "black", alpha = 0.7) + 
  scale_fill_viridis(discrete = T) + 
  theme_bw()

saveRDS(op.df, file = "./cache/output_binary_rev_1.rds")

# Load combined_rows array from inputs to understand the speed ------------

#' - understand which files do not have remote sensing data so they can be ignored. If not, use John's

fnames <- list.files(path = "./data/train/train")
fnames_sample <- fnames %>% sample(1)
sample_array <- read_rds(path = paste0("./data/train/train/", fnames_sample, "/input_combined_array.rds"))

check_if_data_exists <- function(name){
  breakname <- paste0("./data/train/train/", name, "/remote_sensing/aster/")
  list.names <- list.files(breakname)
  
  if(!(identical(list.names, character(0)))){
    return(1) # 0 indicates data does not exist
  }else{
    return(0) # 0 indicates data does not exist 
  }
}

# breakname <- paste0("./data/train/train/", name, "/remote_sensing/aster/")
# list.names <- list.files(breakname)
# 
check_if_data_exists <- function(name){
  breakname <- paste0("./data/train/train/", name, "/remote_sensing/aster/")
  list.names <- list.files(breakname)
  return(list.names %>% length())
}

df.flag <- tibble(names = fnames %>% unlist(), 
                  flag = fnames %>% purrr::map(.f = check_if_data_exists) %>% unlist())

saveRDS(df.flag, file = "./cache/missing_remote_sensing_flag_df.rds")

# Define New Set of Outputs (2.0) for multi-class classification -----------------------------------------------

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

# Some simple ideas -------------------------------------------------------

# 1) plot class imbalance

extract_plot <- ggplot(df.final, aes(x = reorder(commodity, commodity, function(x) - length(x)))) +  
  geom_bar(aes(fill = reorder(commodity, commodity, function(x) - length(x))), alpha = 0.8, color = "black") +
  scale_fill_viridis(discrete = TRUE) + 
  labs(title = "Class Distribution", x = "commodity", y = "count") + 
  ggthemes::theme_fivethirtyeight() + 
  theme(legend.position = "none")

extract_plot

# ggsave(filename = "./cache/results.jpg", extract_plot, width = 11.5, height = 8.5, units = "in")

# 2) plot cross-correlation between commodities

wide_df <- df.final %>% mutate(value = 1) %>% 
  dplyr::select(stamp_id, commodity, value) %>% 
  reshape2::dcast(formula = stamp_id ~ commodity, value.var = "value", fun.aggregate = sum) %>% 
  as_tibble()

df.final %>% filter(stamp_id == "00516789") %>% dplyr::select(stamp_id, x, y) %>% distinct()

# write_rds(df.final, path = "./cache/df_final.rds")

corr <- cor(wide_df %>% dplyr::select(-stamp_id)) %>% round(1) 
ggcorrplot::ggcorrplot(corr, hc.order = TRUE, outline.col = "white", lab = TRUE)

# 3) convert to probability distributions

temp <- wide_df %>% 
  dplyr::select(-stamp_id) %>% 
  # mutate(row_sum = rowSums(.)) %>% 
  # rowwise() %>% 
  mutate_at(vars(Ag:Zr), funs(ifelse(. > 0, 1, 0))) %>% 
  # dplyr::select(-row_sum) %>% 
  bind_cols(wide_df %>% dplyr::select(stamp_id)) %>% 
  dplyr::select(stamp_id, everything())

corr <- cor(temp %>% dplyr::select(-stamp_id)) %>% round(1) 
ggcorrplot::ggcorrplot(corr, hc.order = TRUE, outline.col = "white", lab = TRUE)

temp %>% reshape2::melt(id.vars = "stamp_id") %>% filter(value > 0) %>% 
  ggplot() + 
  geom_histogram(aes(x = value, fill = variable), alpha = 0.9, color = "black") + 
  facet_wrap(variable~., scales = "free") +
  theme_bw() + 
  theme(legend.position = "bottom") 

temp <- temp %>% ungroup()

# Merge with other stamp_ids that are 0 and missing...

stamp_ids <- list.files(path = "./data/train/train/")
stamp_ids <- stamp_ids[!(stamp_ids %in% unique(temp$stamp_id))]
zeros <- matrix(0, nrow = length(stamp_ids), ncol = ncol(temp) - 1) %>% 
  data.frame() %>% 
  as.tibble() %>% 
  mutate(stamp_id = stamp_ids) %>% 
  dplyr::select(stamp_id, everything())
names(zeros) <- names(temp)

temp <- bind_rows(temp, zeros)

# write_rds(temp, path = paste0("./cache/output_2_multiclass.rds")) # this contains the multiclass outputs: 
# but need to create vectors on the fly. Do it in a second script. 

# 3.0: Create Cluster of Outputs for Simplicity in Presence Detection --------------

cluster_group <- list("Sb", 
                      "Au",
                      c("Ag", "Pb", "Zn", "Cu"),
                      "Mo",
                      c("PGE", "Co", "Ni"),
                      "Mn",
                      "Ta",
                      c("Ti", "Zr", "REE", "Th"),
                      "V",
                      "Fe", 
                      c("Bi", "Sn", "W"),
                      "U")

temp_clusters <- temp %>% 
  mutate(cluster_ag_pb_zn_cu = Ag + Pb + Zn + Cu, 
         cluster_pge_co_ni = PGE + Co + Ni, 
         cluster_ti_zr_ree_th = Ti + Zr + REE + Th, 
         cluster_bi_sn_w = Bi + Sn + W) %>% 
  dplyr::select(-Ag, -Pb, -Zn, -Cu, -PGE, -Co, -Ni, -Ti, -Zr, -REE, -Th, -Bi, -Sn, -W) %>% 
  mutate_at(vars(cluster_ag_pb_zn_cu:cluster_bi_sn_w), funs(ifelse(. > 0, 1, 0)))

corr <- cor(temp_clusters %>% dplyr::select(-stamp_id)) %>% round(1)
ggcorrplot::ggcorrplot(corr, hc.order = TRUE, outline.col = "white", lab = TRUE) 
write_rds(temp_clusters, path = "./cache/df_clusters.rds")
