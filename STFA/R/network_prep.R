#1.Call Libs
library(shiny)
library(visNetwork)
library(igraph)
library(lubridate)
library(DT)
library(ggiraph)
library(plotly)
library(tidyverse)
library(raster)
library(sf)
library(clock)
library(rgdal)
library(dplyr)
library(hms) 
library(crosstalk)
library(timetk) 
library(tidygraph)
library(ggraph)
library(ggrepel) 
library(htmltools) 
library(sugrrants)
library(igraph)
library(shinythemes)


cc_graph <- read_rds("data/cc_graph.rds")
lc_graph <- read_rds("data/lc_graph.rds")

#lc

lc_graph <- lc_graph %>%
  mutate(ec = centrality_eigen()) #%>%
#mutate(closec_c = centrality_closeness())


lc_11 <- toVisNetworkData(
  lc_graph,
  idToLabel = FALSE)

lc_nodes21 <- lc_11$nodes
names(lc_nodes21)[names(lc_nodes21) == "label1"] <- "label"

lc_edges21 <- lc_11$edges

d_lc_nodes <- lc_11$nodes
names(d_lc_nodes)[names(d_lc_nodes) == "label1"] <- "label"

lc_eigennodes <- lc_11$nodes
names(lc_eigennodes)[names(lc_eigennodes) == "label1"] <- "label"

#degree(lc_graph)
#degree(lc_graph) >= 50
degreePal <- factor(degree(lc_graph) >= 50, labels = c("#D3D3D3", "#225560"))
d_lc_nodes$color <- degreePal


#Do this for eigen
# Create continuous color palette. 
eigScalePal <- colorRampPalette(c('#E0F4FF','#003049')) 
# Match palette to centrality vector.
lc_eigennodes$color <- eigScalePal(7)[cut(eigen_centrality(lc_graph)$vector, breaks = 7)]



#cc
cc_graph <- cc_graph %>%
  mutate(ec = centrality_eigen()) #%>%
#mutate(closec_c = centrality_closeness()) 


g11 <- toVisNetworkData(
  cc_graph,
  idToLabel = FALSE)

nodes21 <- g11$nodes
edges21 <- g11$edges
dnodes <- g11$nodes
eigennodes <- g11$nodes

#Do this for degree
#degree(cc_graph)
#degree(cc_graph) >= 50
degreePal <- factor(degree(cc_graph) >= 50, labels = c("#D3D3D3", "#225560"))
dnodes$color <- degreePal


#Do this for eigen
# Create continuous color palette. 
eigScalePal <- colorRampPalette(c('#E0F4FF','#003049')) 
# Match palette to centrality vector.
eigennodes$color <- eigScalePal(7)[cut(eigen_centrality(cc_graph)$vector, breaks = 7)]