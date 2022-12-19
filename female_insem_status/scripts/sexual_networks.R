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
all_data <- read.csv("data/rep_status_all_data.csv") %>% 
            filter(notes != "exclude") %>% 
            filter(behaviour == "mating")# | behaviour == "mount")  # CHANGE BASED ON WHETHER YOU WANT TO MAKE MOUNT OR INSEMINATION NETWORKS

rep_list <- split(all_data, all_data$replicate) # creates a list of replicates

# List of nodes
nodes <- read.csv("data/id_key.csv") %>% 
         filter(replicate == 1) %>% 
         select(ID)

################# CREATING MOUNTING NETWORKS ######################
# Function that creates igraph objects
func_igraph <- function(rep_list){
               
               edge_list <- rep_list %>% 
                            select(male_ID, female_ID) %>% 
                            mutate(weight = 1) # turns dataframes for each rep into edgelists
               
               igraph <- graph_from_data_frame(d = edge_list, vertices = nodes) # creates igraph objects
               
               igraph <- set_vertex_attr(igraph, "treatment", 
                         value = ifelse(V(igraph)$name %in% LETTERS[1:12], "Male",  
                                 ifelse(V(igraph)$name %in% LETTERS[13:18], "recent", "distant"))) # assign treatment as vertex attributes
               strength <- strength(igraph) # calculate strength
               igraph <- set_vertex_attr(igraph, "strength", value = strength) # assign strength as vertex attributes
               
               igraph <- simplify(igraph, remove.multiple = TRUE, edge.attr.comb = sum) # remove double edges
               
return(igraph)
}

igraph_objects <- lapply(rep_list, func_igraph) # creates one igraph object per rep

# Function that assigns igraph attributes
func_plot_igraph <- function(igraph){
    V(igraph)$color <- ifelse(V(igraph)$treatment == "Male", "#0072B5FF", ifelse(V(igraph)$treatment == "recent", "#D44E3A", "#E69946"))
    V(igraph)$size <- (V(igraph)$strength+1.8)*5
    V(igraph)$label.color <- "white"
    E(igraph)$color <- "black"
    E(igraph)$width <- E(igraph)$weight*3
return(igraph)
}

igraph_objects <- lapply(igraph_objects, func_plot_igraph) # adds visual attributes to igraph objects

plot(igraph_objects[[5]], edge.arrow.size = 0.5, weighted = TRUE)

rep_5_mount_net <- igraph_objects[[5]]
rep_5_insem_net <- igraph_objects[[5]]


## CODE USED TO KEEP NODE POSITIONS CONSISTENT ACROSS NETWORKS
id <- tkplot(rep_5_insem_net, vertex.label.family = "Helvetica")
coords_5 <- tk_coords(id) # Saves coordinates of trial #1
tk_set_coords(id, coords_5) # Set stored coordinates to trial #1
tkplot.fit.to.screen(id)


write.csv(coords_5, "coords_5.csv")

