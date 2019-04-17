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

# Single Output

# df.output <- read_rds(path = "./cache/output_binary_rev_1.rds") %>% 
#   rename(names = output_stamp)

# Multiclass-Multilabel Output

# df.output <- read_rds(path = "./cache/output_2_multiclass.rds") %>% 
#  rename(names = stamp_id) #%>% 
#  dplyr::select(names, Ag)
#  filter(!(names %in% remove.names))

df.output <- read_rds(path = "./cache/df_clusters.rds") %>% 
  rename(names = stamp_id) %>% 
  dplyr::select(names, Au)

df.output %>% dplyr::select(-names) %>% reshape2::melt() %>% mutate(value = factor(value)) %>% 
  ggplot() + 
  geom_bar(aes(x = value, fill = variable), color = "black", alpha = 0.8) +
  scale_fill_viridis(discrete = TRUE) + 
  facet_wrap(variable~.) + 
  theme_bw()

class_imbalance <- df.output %>% 
  dplyr::select(-names) %>% 
  reshape2::melt() %>% 
  #mutate(value = factor(value)) %>% 
  group_by(variable) %>% 
  summarise_all(funs(sum)) %>% 
  ungroup() %>%  
  mutate(value_0 = nrow(df.output) - value) %>%  
  mutate_at(vars(value:value_0), funs(./nrow(df.output)))

class_imbalance_list <- as.list(class_imbalance %>% dplyr::select(value:value_0) %>% t() %>% as_tibble())
names(class_imbalance_list) <- names(df.output)[-1]

#' input_combined_array contains the key files

fnames <- df.output %>% pull(names)
# fnames_sample <- fnames %>% sample(1)
# sample_array <- read_rds(path = paste0("./data/train/train/", fnames_sample, "/input_combined_array.rds"))

coefficient <- 1

# weight_vector <- df.output %>% 
#   mutate_at(vars(Ag:Zr), funs(sum(.))) %>% 
#   slice(1) %>% 
#   dplyr::select(-names) %>% 
#   rowwise() %>% 
#   as.numeric()

weight_vector <- df.output %>%
  mutate_at(vars(Au:cluster_bi_sn_w), funs(sum(.))) %>%
  slice(1) %>%
  dplyr::select(-names) %>%
  rowwise() %>%
  as.numeric()

weight_vector <- ((1/weight_vector)^coefficient)/(max((1/weight_vector)^coefficient))

hist(weight_vector, col = "red2")

# Load Input Data ---------------------------------------------------------

inputs_repository <- read_rds(path = "./cache/inputs_repository_scaled.rds")

# Create Generator of Inputs and Outputs ----------------------------------

batch_size <- 16
ignore_index <- c(1:20) # 2,4
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
      inputs[[i]] <- inputs[[i]][,,ignore_index]
    }
    inputs <- abind::abind(inputs, along = 0)  

    
    # Obtain Output Point (single class)
    
    if(predict == FALSE){
      outputs <- df.output %>% filter(names %in% unlist(batch_names)) %>% dplyr::select(-names) %>% as.matrix() # %>% mutate_all(factor) 
      return(list(inputs, outputs))
    } else {
      return(list(inputs))
    }
    
  }
}

train_gen <- generator(batch_size = 32, train = TRUE, predict = FALSE)
val_gen <- generator(batch_size = 16, train = FALSE, predict = FALSE)

a <- train_gen()

# Check devices -----------------------------------------

library(tensorflow)

with(tf$device("/gpu:0"), {const <- tf$constant(1)})

sess <- tf$Session()
sess$run(const)

# a <- train_gen()

# Depth-Wise Separable Convolution ----------------------------------------

height <- 125
width <- 125
channels <- length(ignore_index)
num_classes <- base::ncol(df.output) - 1

model <- keras_model_sequential() %>%
  layer_separable_conv_2d(filters = 256, kernel_size = 3,
                          activation = "relu",
                          input_shape = c(height, width, channels), 
                          data_format = "channels_last") %>%
  layer_batch_normalization() %>% 
  layer_separable_conv_2d(filters = 256, kernel_size = 3,
                          activation = "relu") %>%
  layer_batch_normalization() %>% 
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_separable_conv_2d(filters = 256, kernel_size = 3,
                          activation = "relu") %>%
  layer_batch_normalization() %>% 
  layer_separable_conv_2d(filters = 256, kernel_size = 3,
                          activation = "relu") %>%
  layer_batch_normalization() %>% 
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_separable_conv_2d(filters = 256, kernel_size = 3,
                          activation = "relu") %>%
  layer_batch_normalization() %>% 
  layer_separable_conv_2d(filters = 256, kernel_size = 3,
                          activation = "relu") %>%
  layer_batch_normalization() %>% 
  layer_global_average_pooling_2d() %>%
  layer_batch_normalization() %>% 
  layer_dense(units = 256, activation = "relu") %>%
  layer_batch_normalization() %>% 
  layer_dense(units = num_classes, activation = "sigmoid")

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_sgd(clipvalue = 0.5),
  metrics = c("accuracy")
)

imbalance <- list()
imbalance["1"] <- class_imbalance$value_0 %>% mean() #class_imbalance_list[[1]][[1]]
imbalance["0"] <- class_imbalance$value %>% mean() #class_imbalance_list[[1]][[2]]
# imbalance["2"] <- 0
# imbalance["3"] <- 0
# imbalance["4"] <- 0
# imbalance["5"] <- 0
# imbalance["6"] <- 0
# imbalance["7"] <- 0
# imbalance["8"] <- 0
# imbalance["9"] <- 0
# imbalance["10"] <- 0
# imbalance["11"] <- 0
# imbalance["12"] <- 0

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 30,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = 30,
  class_weight = list("0" = 1, "1" = 2)
)

# Create Keras Model ------------------------------------------------------

# model <- keras_model_sequential() %>%
#   layer_conv_2d(filters = 256, kernel_size = c(5,5), activation = "relu",
#                 input_shape = c(NULL, 125, 125, 20 - length(ignore_index)), data_format = "channels_last") %>%
#   layer_batch_normalization() %>%
#   layer_max_pooling_2d(pool_size = c(3,3)) %>%
#   layer_conv_2d(filters = 512, kernel_size = c(5,5), activation = "relu") %>%
#   layer_batch_normalization() %>%
#   layer_max_pooling_2d(pool_size = c(3,3)) %>%
#   layer_conv_2d(filters = 1028, kernel_size = c(5,5), activation = "relu") %>%
#   layer_batch_normalization() %>%
#   layer_max_pooling_2d(pool_size = c(3,3)) %>%
#   layer_flatten() %>%
#   # layer_dropout(rate = 0.05) %>%
#   layer_dense(units = 512, activation = "sigmoid") %>%
#   # layer_dropout(rate = 0.5) %>% 
#   # layer_dense(units = 512, activation = "sigmoid") %>% 
#   # layer_dropout(rate = 0.5) %>% 
#   layer_dense(units = 22, activation = "sigmoid")
# 
# model %>% compile(
#   loss = "binary_crossentropy",
#   optimizer = optimizer_adam(),
#   metrics = c("accuracy")
# )
# 
# history <- model %>% fit_generator(
#   train_gen,
#   steps_per_epoch = 50,
#   epochs = 30,
#   validation_data = val_gen,
#   validation_steps = 30,
#   class_weight = list(weight_vector)
# )

# Keras Model 2.0: Frozen Convolutional Base -----------------------------------

# conv_base <- application_vgg19(include_top = FALSE, 
#                                weights = "imagenet", 
#                                input_shape = c(128,128,3))

model_autoencoder <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = 'relu',padding='same', 
                input_shape = c(NULL, 125, 125, 20 - length(ignore_index)), data_format = "channels_last") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2),padding='same') %>% 
  layer_conv_2d(filters = 256, kernel_size = c(3,3), activation = 'relu',padding='same') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2),padding='same') %>% 
  layer_conv_2d(filters = 512, kernel_size = c(3,3), activation = 'relu',padding='same') %>% 
  layer_max_pooling_2d(pool_size = c(3, 3),padding='same') %>% 
  layer_conv_2d(filters = 512, kernel_size = c(3,3), activation = 'relu',padding='same') %>%
  layer_upsampling_2d(size = c(3, 3)) %>%
  layer_conv_2d(filters = 256, kernel_size = c(3,3), activation = 'relu',padding='same') %>%
  layer_upsampling_2d(size = c(3, 3))%>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = 'relu',padding='same') %>% 
  layer_upsampling_2d(size = c(3, 3)) %>%
  layer_conv_2d(filters = 3, kernel_size = c(3,3), activation = 'relu',padding='same')

model_autoencoder

# conv_base <- application_vgg19(include_top = FALSE,
#                                input_shape = c(128,128,3), 
#                                weights = "imagenet")

conv_base <- application_inception_v3(include_top = FALSE,
                                  input_shape = c(297, 297, 3), 
                                  weights = "imagenet")

model.new <- model_autoencoder %>% 
  conv_base %>% 
  layer_flatten() %>% 
  layer_dense(units = 256, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

model.new$trainable_weights %>% length()
freeze_weights(conv_base)
model.new$trainable_weights %>% length()

model.new %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)

history <- model.new %>% fit_generator(
  train_gen, 
  steps_per_epoch = 50,
  epochs = 20,  
  validation_data = val_gen,
  validation_steps = 20 #, 
  #class_weight = list(weight_vector)
)

# Predictions --------------------------------------------------------

pred_gen <- generator(batch_size = NULL, train = FALSE, predict = TRUE, n_max = 10)
results <- model %>% predict_generator(pred_gen, steps = 10)

results %>% as_tibble() %>% reshape2::melt() %>% 
  ggplot() + 
  geom_histogram(aes(x = value, fill = variable)) + 
  scale_fill_discrete() + 
  facet_grid(variable~., scales = "free_y") + 
  theme(axis.text.y = element_blank())
