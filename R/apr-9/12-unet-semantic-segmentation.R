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

# Load Output Data -----------------------------------------------

df.output <- read_rds(path = "./cache/df_clusters.rds") %>% 
  rename(names = stamp_id) %>% 
  dplyr::select(names, Au)

fnames <- df.output %>% pull(names)

outputs_repository <- fnames %>% as.list() %>%
  purrr::map(.f = function(x){
    temp <- read_rds(path = paste0("./data/train/train/", x, "/output_mask.rds"))
    temp[is.infinite(temp)] <- 0 # Remember this
    return(temp)
  })

names(outputs_repository) <- fnames

write_rds(outputs_repository, path = "./cache/outputs_repository.rds")

# Load Input Data ---------------------------------------------------------

inputs_repository <- read_rds(path = "./cache/inputs_repository_scaled.rds")

# input_random <- inputs_repository[[1]]
# s <- extend(x = brick(input_random), y = 1.5, value = 0) %>% as.array()
# 
# input_random <- outputs_repository[[1]]
# s <- extend(x = brick(input_random), y = 1.5, value = 0) %>% as.matrix()

# Create Generator of Inputs and Outputs ----------------------------------

batch_size <- 16
ignore_index <- c(1:3) # 2,4
set.seed(3948765)
train_names <- fnames %>% sample(0.70*length(fnames) %>% floor())
val_names <- fnames[!(fnames %in% train_names)]

generator <- function(batch_size, train = TRUE, predict = FALSE, n_max = NULL){
  
  #' predict = TRUE will enable generation of a test set (validation only)
  
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
    
    # New Way (loaded in memory) for Training Set
    
    if(predict == FALSE){
      batch_names <- names %>% sample(batch_size)
    } else {
      if(!is.null(n_max)){
        batch_names <- names %>% sample(n_max)
      } else {
        batch_names <- names # this will be val_names (batch_Size not necessary)
      }
    }
    
    
    inputs <- inputs_repository[batch_names]
    for(i in 1:length(inputs)){
      inputs[[i]] <- extend(x = brick(inputs[[i]]), y = 1.5, value = 0) %>% as.array()
      #inputs[[i]] <- inputs[[i]]#[,,ignore_index]
    }
    
    inputs <- abind::abind(inputs, along = 0)  
    
    # Obtain Output Point (single class)
    
    if(predict == FALSE){
      outputs <- outputs_repository[batch_names]
      for(i in 1:length(outputs)){
        outputs[[i]] <- extend(x = brick(outputs[[i]]), y = 1.5, value = 0) %>% as.array()
      }
      outputs <- abind::abind(outputs, along = 0)  
      return(list(inputs, outputs))
    } else {
      return(list(inputs))
    }
  }
}

train_gen <- generator(batch_size = 8, train = TRUE, predict = FALSE)
val_gen <- generator(batch_size = 8, train = FALSE, predict = FALSE)

a <- train_gen()

# UNET segmentation -------------------------------------------------------

# Loss function -----------------------------------------------------

K <- backend()

dice_coef <- function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  result <- (2 * intersection + smooth) / 
    (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
  return(result)
}

bce_dice_loss <- function(y_true, y_pred) {
  result <- loss_binary_crossentropy(y_true, y_pred) 
  return(result)
}

# U-net 128 -----------------------------------------------------

height <- 128
width <- 128
channels <- 20 #length(ignore_index)

get_unet_128 <- function(input_shape = c(height, width, channels),
                         num_classes = 1) {
  
  inputs <- layer_input(shape = c(128, 128, channels))
  # 128
  
  down1 <- inputs %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") 
  down1_pool <- down1 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  # 64
  
  down2 <- down1_pool %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") 
  down2_pool <- down2 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  # 32
  
  down3 <- down2_pool %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") 
  down3_pool <- down3 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  # 16
  
  down4 <- down3_pool %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") 
  down4_pool <- down4 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  # 8
  
  center <- down4_pool %>%
    layer_conv_2d(filters = 1024, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 1024, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") 
  # center
  
  up4 <- center %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down4, .), axis = 3)} %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  # 16
  
  up3 <- up4 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down3, .), axis = 3)} %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  # 32
  
  up2 <- up3 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down2, .), axis = 3)} %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  # 64
  
  up1 <- up2 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down1, .), axis = 3)} %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  # 128
  
  classify <- layer_conv_2d(up1,
                            filters = num_classes, 
                            kernel_size = c(1, 1),
                            activation = "sigmoid")
  
  
  model <- keras_model(
    inputs = inputs,
    outputs = classify
  )
  
  model %>% compile(
    optimizer = optimizer_rmsprop(lr = 0.01),
    loss = bce_dice_loss
  )
  
  return(model)
}

model <- get_unet_128()
  
tensorboard("logs_r")

callbacks_list <- list(
  callback_tensorboard("logs_r"),
  callback_early_stopping(monitor = "val_python_function",
                          min_delta = 1e-4,
                          patience = 8,
                          verbose = 1,
                          mode = "max"),
  callback_reduce_lr_on_plateau(monitor = "val_python_function",
                                factor = 0.1,
                                patience = 4,
                                verbose = 1,
                                mode = "max"),
  callback_model_checkpoint(filepath = "weights_r/unet128_{epoch:02d}.h5",
                            monitor = "val_python_function",
                            save_best_only = TRUE,
                            save_weights_only = TRUE,
                            mode = "max" )
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 100,
  epochs = 10,
  validation_data = val_gen,
  validation_steps = 30,
  callbacks = callbacks_list, 
  class_weight = list(0, 1e6)
)

# predictions =========================================

pred_gen <- generator(batch_size = NULL, train = TRUE, predict = TRUE, n_max = 10)
results <- model %>% predict_generator(pred_gen, steps = 10)
res <- list()
inds <- dim(results)

for(i in 1:inds[[1]]){
  temp <- results[i,,,] %>% as.matrix()
  res[[i]] <- raster(temp)
}

tm_shape(res[[10]]) + 
  tm_raster(legend.show = TRUE, palette = "viridis") + 
  tm_legend(legend.position = c("center","bottom"))

# Histogram of Probabilities ----------------------------------------------

s <- results %>% as.numeric()

hist(s, col = "red2")

