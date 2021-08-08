library(UpSetR)
library(tidyverse)

cc_data <- read_csv("data/cc_data.csv")

cc_data[grep("Katerina", cc_data$location),2] <- "Katerina's Cafe"

cc_data_2 <- cc_data %>% 
  distinct(location, last4ccnum) %>%
  mutate(present = 1) 

cc_pivot <- cc_data_2 %>% 
  pivot_wider(last4ccnum, names_from = location, values_from = present, values_fill = 0) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  lapply(as.integer) %>%
  glimpse()

as.data.frame(cc_pivot)

UpSetR::upset(cc_pivot)