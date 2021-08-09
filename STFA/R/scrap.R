library(UpSetR)
library(tidyverse)
library(ggstatsplot)
library(plotly)

cc_data <- read.csv("data/cc_data.csv")

cc_data[grep("Katerina", cc_data$location),2] <- "Katerina's Cafe"

# Distinct visitor and location for upset R 
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

# ANOVA test to see if spend across locations are significantly different 

set.seed(123)

# Difference in mean spend across locations
cc_anova <- cc_data %>% 
  filter(location %in% c("Kalami Kafenion", "Guy's Gyros","Brew've Been Served","Hallowed Grounds","Gelatogalore"))
  #filter(last4ccnum %in% c("4795","7108","6816","9551"))

# Plot ANOVA
ggbetweenstats(
  data = cc_anova,
  x = location,
  y = price,
  # p for parametric, np for non-parametric, r for robust and bf for Bayes Factor
  type= "bayes",
  conf.level = 0.5,
  #outlier.tagging = TRUE,
  #outlier.label = last4ccnum, 
  title = "Difference in mean spend across locations",
  results.subtitle = TRUE,
  xlab = "Location",
  ylab = "Price ($)"
)

# Plot ANOVA
ggbetweenstats(
  data = cc_anova,
  x = last4ccnum,
  y = price,
  # p for parametric, np for non-parametric, r for robust and bf for Bayes Factor
  type= "np",
  conf.level = 0.95,
  #outlier.tagging = TRUE,
  #outlier.label = last4ccnum, 
  title = "Difference in mean spend across Credit Card numbers",
  results.subtitle = TRUE,
  xlab = "Credit Card Num",
  ylab = "Price ($)"
)

# Plot histogram
hist <- ggplot(cc_anova, aes(x=price)) +
  geom_histogram(bins = 10) +
  facet_wrap(~location) +
  labs(title = "Distribution of Spend Amount by Location",
       y = "Frequency", x = "Price ($)")

ggplotly(hist)

# Statistical test Results
location_list <- cc_data %>%
  distinct(location) %>%  
  as.list(location)


cc_list <- cc_data %>%
  distinct(last4ccnum) %>%  
  as.list(last4ccnum)
