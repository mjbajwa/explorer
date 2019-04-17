# Load Libraries ----------------------------------------------------------

library(tidyverse)

# Load Data ---------------------------------------------------------------

inputs_repository <- fnames %>% as.list() %>%
  purrr::map(.f = function(x){
    temp <- read_rds(path = paste0("./data/train/train/", x, "/input_combined_array_g_rm_2.rds"))
    temp[is.infinite(temp)] <- 0 # Remember this
    temp[temp > 1e+10] <- 0
    return(temp)
  })

names(inputs_repository) <- fnames

# Create Distribution for Each Relevant Array -----------------------------

k <- 1

get_distribution_data <- function(k){

  length_sample <- length(inputs_repository)*0.25 %>% floor()
  temp_max <- c()
  temp_min <- c()
  
  for(i in 1:length(inputs_repository)){
    temp_max <- c(temp_max, inputs_repository[[i]][,,k] %>% max())
    temp_min <- c(temp_min, inputs_repository[[i]][,,k] %>% min())
  }
  
  df <- tibble(max = temp_max, min = temp_min, k = as.character(k))
}

combined_df <- lapply(as.list(1:19), get_distribution_data) %>% bind_rows()
combined_df_long <- combined_df %>% reshape2::melt(id.vars = "k")

ggplot(combined_df_long %>% mutate(k = factor(k))) + 
  geom_histogram(aes(x = value, fill = variable), color = "black", alpha = 0.2) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_wrap(k~., scales = "free") + 
  ggtitle(label = "Peak Distribution Analysis")
