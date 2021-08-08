library(UpSetR)
library(tidyverse)

cc_data1 <- read.csv("data/cc_data.csv")

cc_data[grep("Katerina", cc_data$location),2] <- "Katerina's Cafe"

cc_data_2 <- cc_data %>% 
  distinct(location, last4ccnum) %>%
  mutate(present = 1) 

cc_pivot <- cc_data_2 %>% 
  pivot_wider(last4ccnum, names_from = location, values_from = present, values_fill = 0) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate_all(as.integer) %>%
  as.data.frame()
  
rownames(cc_pivot) <- NULL

UpSetR::upset(cc_pivot, nsets = 6, order.by = "freq")

#write.csv(cc_pivot,'cc_pivot.csv')

cc_csv <- read.csv("cc_pivot.csv", header=T, sep="," ) %>%
  dplyr::select(-X)

#UpSetR::upset(cc_csv)

UpSetR::upset(data = cc_pivot, 
              nsets = 6, 
              nintersects = 40, 
              order.by = "freq",
              decreasing = TRUE)
