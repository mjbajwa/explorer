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

df.output <- read_rds(path = "./cache/output_2_multiclass.rds") %>% 
  rename(names = stamp_id) #%>% 
  # filter(!(names %in% remove.names))

#' input_combined_array contains the key files

fnames <- df.output %>% pull(names)
# fnames_sample <- fnames %>% sample(1)
# sample_array <- read_rds(path = paste0("./data/train/train/", fnames_sample, "/input_combined_array.rds"))

weight_vector <- df.output %>% 
  mutate_at(vars(Ag:Zr), funs(sum(.))) %>% 
  slice(1) %>% 
  dplyr::select(-names) %>% 
  rowwise() %>% 
  as.numeric()

weight_vector <- ((1/weight_vector)^1.1)/(max((1/weight_vector)^1.1))

# Load all arrays into memory for easy reading ----------------------------

inputs_repository <- fnames %>% as.list() %>%
  purrr::map(.f = function(x){
    temp <- read_rds(path = paste0("./data/train/train/", x, "/input_combined_array_g_rm_2.rds"))
    temp_geo <- read_rds(path = paste0("./data/train/train/", x, "/input_geology_shears_2.rds"))
    temp <- abind::abind(temp, temp_geo)
    temp[is.infinite(temp)] <- 0 # Remember this
    temp[abs(temp) > 1e+10] <- 0
    return(temp)
  })

names(inputs_repository) <- fnames

write_rds(inputs_repository, path = "./cache/inputs_repository_unscaled.rds")

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

# Med-SD Scaling ---------------------------------------------------------

s <- inputs_repository[[1]]

for(j in 1:length(inputs_repository)){
  for(i in 1:dim(s)[[3]]){
    sd <- min_max_tibble[i, 2] %>% as.numeric()
    med <- min_max_tibble[i, 1] %>% as.numeric()
    inputs_repository[[j]][,,i] <- (inputs_repository[[j]][,,i] - med)/(sd)
  }
}

write_rds(inputs_repository, path = "./cache/inputs_repository_scaled.rds")

# Create Generator of Inputs and Outputs ----------------------------------

batch_size <- 16
ignore_index <- c(2)
set.seed(3948765)
train_names <- fnames %>% sample(0.70*length(fnames) %>% floor())
val_names <- fnames[!(fnames %in% train_names)]

generator <- function(batch_size, train = TRUE, predict = FALSE){
  
  #' Configure Generator for Train and Test
  
  if(train == TRUE){
    names <- train_names
  } else {
    names <- val_names
  }
  
  function(){
    
    # Obtain Input Rasters (500, 500, 18)

    # batch_names <- names %>% sample(batch_size) %>% as.list() 
    # inputs <- batch_names %>% purrr::map(.f = function(x){
    #   temp <- read_rds(path = paste0("./data/train/train/", x, "/input_combined_array_g_rm_2.rds"))
    #   scale_function(temp)
    #   })
    # inputs <- abind::abind(inputs, along = 0)
    
    # New Way (loaded in memory)
    
    batch_names <- names %>% sample(batch_size)
    inputs <- inputs_repository[batch_names]
    for(i in 1:length(inputs)){
      inputs[[i]] <- inputs[[i]][,,-ignore_index]
    }
    inputs <- abind::abind(inputs, along = 0)  
  
    # Obtain Output Point (single class)
    
    if(predict == FALSE){
      outputs <- df.output %>% filter(names %in% unlist(batch_names)) %>% dplyr::select(-names) %>% as.matrix()
      return(list(inputs, outputs))
    } else {
      return(list(inputs))
    }

  }
}
  
train_gen <- generator(batch_size = 64, train = TRUE)
val_gen <- generator(batch_size = 16, train = FALSE)

# Check devices -----------------------------------------

library(tensorflow)

with(tf$device("/gpu:0"), {const <- tf$constant(1)})

sess <- tf$Session()
sess$run(const)

# a <- train_gen()

# Create Keras Model ------------------------------------------------------

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu", 
                input_shape = c(NULL, 125, 125, 19), data_format = "channels_last") %>% 
  layer_batch_normalization() %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_flatten() %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 256, activation = "relu") %>% 
  layer_dense(units = 22, activation = "softmax")

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(lr = 1e-2),
  metrics = c("accuracy")
)
  
history <- model %>% fit_generator(
  train_gen, 
  steps_per_epoch = 100,
  epochs = 50, 
  validation_data = val_gen,
  validation_steps = 30, 
  class_weight = list(weight_vector)
)

# Make Predictions

pred_gen <- generator(batch_size = 8, train = TRUE, predict = TRUE)
results <- model %>% predict_generator(pred_gen, steps = 2)
results

# Keras Model 2.0: Frozen Convolutional Base ------------------------------

# Create Keras Model ------------------------------------------------------

conv_base <- application_vgg19(include_top = FALSE, 
                               weights = "imagenet", 
                               input_shape = c(128,128,3))

model_autoencoder <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu',padding='same', 
                input_shape = c(NULL, 125, 125, 19 - length(ignore_index)), data_format = "channels_last") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2),padding='same') %>% 
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = 'relu',padding='same') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2),padding='same') %>% 
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = 'relu',padding='same') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2),padding='same') %>% 
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = 'relu',padding='same') %>% 
  layer_upsampling_2d(size = c(2, 2))%>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = 'relu',padding='same') %>% 
  layer_upsampling_2d(size = c(2, 2))%>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu',padding='same') %>% 
  layer_upsampling_2d(size = c(2, 2))%>%
  layer_conv_2d(filters = 3, kernel_size = c(3,3), activation = 'relu',padding='same') 

model.new <- model_autoencoder %>% 
  conv_base %>% 
  layer_flatten() %>% 
  layer_dense(units = 512, activation = "relu") %>% 
  layer_dense(units = 22, activation = "softmax")

model.new$trainable_weights %>% length()
freeze_weights(conv_base)
model.new$trainable_weights %>% length()

model.new %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(lr = 1e-1),
  metrics = c("acc")
)

history <- model.new %>% fit_generator(
  train_gen, 
  steps_per_epoch = 50,
  epochs = 50,  
  validation_data = val_gen,
  validation_steps = 20, 
  class_weight = list(weight_vector)
)

