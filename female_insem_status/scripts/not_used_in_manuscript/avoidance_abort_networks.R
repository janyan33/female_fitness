## setwd("C:/Users/jy33/OneDrive/Desktop/R/female_rep_status")

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(stats)
library(igraph)

# Script that allows igraph plots to change arrow size
source("scripts/igraphplot2.R")
environment(plot.igraph2) <- asNamespace('igraph')
environment(igraph.Arrows2) <- asNamespace('igraph')

###################### INPUTTING AND ORGANIZING DATA ########################
avoidance_data <- read.csv("data/avoidance_edgelists.csv")
abort_data <- read.csv("data/abort_edgelists.csv")

rep_list_avoid <- split(avoidance_data, avoidance_data$replicate) # creates a list of replicates

rep_list_abort <- split(abort_data, abort_data$replicate)

# List of nodes
nodes <- read.csv("data/id_key.csv") %>% 
  filter(replicate == 1) %>% 
  select(ID)

################# CREATING MOUNTING NETWORKS ######################
# Function that creates igraph objects
func_igraph <- function(rep_list){
  
  edge_list <- rep_list %>% 
    select(male_ID, female_ID, weight) # turns dataframes for each rep into edgelists
  
  igraph <- graph_from_data_frame(d = edge_list, vertices = nodes, directed = FALSE) # creates igraph objects
  
  igraph <- set_vertex_attr(igraph, "treatment", 
                            value = ifelse(V(igraph)$name %in% LETTERS[1:12], "Male",  
                                           ifelse(V(igraph)$name %in% LETTERS[13:18], "recent", "distant"))) # assign treatment as vertex attributes
  strength <- strength(igraph) # calculate strength
  igraph <- set_vertex_attr(igraph, "strength", value = strength) # assign strength as vertex attributes
  
  igraph <- simplify(igraph, remove.multiple = TRUE, edge.attr.comb = sum) # remove double edges
  
  return(igraph)
}

igraph_objects_avoid <- lapply(rep_list_avoid, func_igraph) # creates one igraph object per rep
igraph_objects_abort <- lapply(rep_list_abort, func_igraph) # creates one igraph object per rep


# Function that assigns igraph attributes
func_plot_igraph <- function(igraph){
  V(igraph)$color <- ifelse(V(igraph)$treatment == "Male", "skyblue3", ifelse(V(igraph)$treatment == "recent", "red", "orange"))
  V(igraph)$size <- ifelse(V(igraph)$treatment == "Male", 8, (V(igraph)$strength+2)*4)
  V(igraph)$label.color <- "white"
  E(igraph)$color <- "black"
  E(igraph)$width <- E(igraph)$weight*6
  return(igraph)
}

igraph_objects_avoid <- lapply(igraph_objects_avoid, func_plot_igraph) # adds visual attributes to igraph objects
igraph_objects_abort <- lapply(igraph_objects_abort, func_plot_igraph) # adds visual attributes to igraph objects


plot(igraph_objects_abort[[5]], vertex.label = NA)

tkplot(igraph_objects_abort[[5]])


