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
library(keras)

# Loading Data + Generators -----------------------------------------------

# Load output binary classifier

df.flag <- read_rds(path = "./cache/missing_remote_sensing_flag_df.rds")
remove.names <- df.flag %>% filter(flag == 0) %>% pull(names)

df.output <- read_rds(path = "./cache/output_binary_rev_1.rds") %>% 
  rename(names = output_stamp)

ggplot(df.output %>% mutate(results = factor(results))) + 
  geom_histogram(aes(x = results, fill = results), stat = "count", color = "black", alpha = 0.7) + 
  scale_fill_viridis(discrete = T) + 
  theme_bw()

#' input_combined_array contains the key files

fnames <- df.output %>% pull(names)
# fnames_sample <- fnames %>% sample(1)
# sample_array <- read_rds(path = paste0("./data/train/train/", fnames_sample, "/input_combined_array.rds"))

scale_function <- function(sample_array){
  
  for(i in 1:dim(sample_array)[[3]]){
    sample_array[,,i] <- scale(sample_array[,,i])
  }
  
  return(sample_array)
  
}

# Load all arrays into memory for easy reading ----------------------------

inputs_repository <- fnames %>% as.list() %>%
  purrr::map(.f = function(x){
    temp <- read_rds(path = paste0("./data/train/train/", x, "/input_combined_array_g_rm_2.rds"))
    temp[is.infinite(temp)] <- 0 # Remember this
    temp[abs(temp) > 1e+10] <- 0
    return(temp)
  })

names(inputs_repository) <- fnames

# write_rds(inputs_repository, path = "./cache/inputs_repository_unscaled.rds")

# Improve Scaling Function ------------------------------------------------

med_value <- matrix(0, nrow = 19, ncol = length(inputs_repository))
sd_value <- med_value

for(j in 1:dim(inputs_repository[[1]])[[3]]){

  for(i in 1:length(inputs_repository)){
    temp <- inputs_repository[[i]][,,j] %>% as.numeric()
    med_value[j, i] <- median(temp)
    sd_value[j, i] <- sd(temp)
  }

}

med_value_vec <- apply(med_value, 1, mean)
sd_value_vec <- apply(sd_value, 1, mean)
min_max_tibble <- tibble(med = med_value_vec, sd = sd_value_vec)
min_max_tibble

# write_rds(x = min_max_tibble, path = paste0("./cache/min_max_tibble.rds"))

# Min Max Scaling ---------------------------------------------------------

s <- inputs_repository[[1]]

for(j in 1:length(inputs_repository)){
  for(i in 1:dim(s)[[3]]){
    sd <- min_max_tibble[i, 2] %>% as.numeric()
    med <- min_max_tibble[i, 1] %>% as.numeric()
    inputs_repository[[j]][,,i] <- (inputs_repository[[j]][,,i] - med)/(sd)
  }
}

# Create Generator of Inputs and Outputs ----------------------------------

batch_size <- 16
ignore_index <- c(2,4)
set.seed(3948765)
train_names <- fnames %>% sample(0.70*length(fnames))
val_names <- fnames[!(fnames %in% train_names)]

generator <- function(batch_size, train = TRUE){
  
  #' Configure Generator for Train and Test
  
  if(train == TRUE){
    names <- train_names
  } else {
    names <- val_names
  }
  
  function(){
    
    # Obtain Input Rasters (500, 500, 18)
    
    # batch_names <- names %>% sample(batch_size) %>% as.list() 
    # inputs <- batch_names %>% purrr::map(.f = function(x){read_rds(path = paste0("./data/train/train/", x, "/input_combined_array.rds"))})
    
    # New Way (loaded in memory)
    
    batch_names <- names %>% sample(batch_size)
    inputs <- inputs_repository[batch_names]
    for(i in 1:length(inputs)){
      inputs[[i]] <- inputs[[i]][,,-ignore_index]
    }
    inputs <- abind::abind(inputs, along = 0)  
    
    # INPUTS NEED TO BE NORMALIZED AND NORMALIZATION NEEDS TO BE STORED.
  
    # Obtain Output Point
  
    outputs <- df.output %>% filter(names %in% unlist(batch_names)) %>% pull(results) %>% as.numeric()
  
  # Return results
  
    list(inputs, outputs)
  }
}
  
train_gen <- generator(batch_size = 64, train = TRUE)
val_gen <- generator(batch_size = 16, train = FALSE)

a <- train_gen()

##--------------------------
model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 128, kernel_size = c(5,5), activation = "relu", 
                input_shape = c(NULL, 125, 125, 19 - length(ignore_index)), data_format = "channels_last") %>% 
  layer_batch_normalization() %>% 
  layer_max_pooling_2d(pool_size = c(3,3)) %>% 
  layer_conv_2d(filters = 256, kernel_size = c(5,5), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(3,3)) %>% 
  layer_conv_2d(filters = 512, kernel_size = c(5,5), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(3,3)) %>% 
  layer_flatten() %>% 
  # layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 512, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(lr = 1e-2),
  metrics = c("acc")
)

history <- model %>% fit_generator(
  train_gen, 
  steps_per_epoch = 50,
  epochs = 50,  
  validation_data = val_gen,
  validation_steps = 20
)

# Create Keras Model ------------------------------------------------------

conv_base <- application_vgg19(include_top = FALSE, 
                               weights = "imagenet", 
                               input_shape = c(128,128,3))

model_autoencoder <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = 'relu',padding='same', 
                input_shape = c(NULL, 125, 125, 19 - length(ignore_index)), data_format = "channels_last") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2),padding='same') %>% 
  layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',padding='same') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2),padding='same') %>% 
  layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',padding='same') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2),padding='same') %>% 
  layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',padding='same') %>% 
  layer_upsampling_2d(size = c(2, 2))%>%
  layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',padding='same') %>% 
  layer_upsampling_2d(size = c(2, 2))%>%
  layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = 'relu',padding='same') %>% 
  layer_upsampling_2d(size = c(2, 2))%>%
  layer_conv_2d(filters = 3, kernel_size = c(3,3), activation = 'relu',padding='same') 

model.new <- model_autoencoder %>% 
  conv_base %>% 
  layer_flatten() %>% 
  layer_dense(units = 512, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

model.new$trainable_weights %>% length()
freeze_weights(conv_base)
model.new$trainable_weights %>% length()

model.new %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(lr = 2e-5),
  metrics = c("acc")
)

history <- model.new %>% fit_generator(
  train_gen, 
  steps_per_epoch = 50,
  epochs = 50,  
  validation_data = val_gen,
  validation_steps = 20
)
