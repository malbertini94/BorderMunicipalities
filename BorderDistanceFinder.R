# Packages
library(tidyverse)
library(magrittr)
library(sf) #key package to handle spatial operations
library(readxl)
library(leaflet)

#Set directory
wd <- "yourWD"
setwd(wd)

#####################
# Compute centroids #
#####################

# Load Municipality Boundaries  (shape files): creates object called "mun"
load(paste0(wd,"/municipalities.RData"))

#Create a dataset for the centroids and the mun id.
mun_centroids <- mun$mun_id

#Drop geometry of mun_simple which is the same as mun and we want to replace it with the centroids
st_drop_geometry(mun_centroids)

# Compute centroids
centroids <- st_centroid(mun$geometry)

#Add centroids to data with id
st_geometry(mun_centroids) <- centroids

#########################################
# Compute minimum distance from border  #
#########################################

#load custom offices: creates a dataset called custom
load(paste0(wd,"/custom.RData"))

#reproject custom crs system to match the centroid's crs system
custom_proj <- st_transform(custom, st_crs(mun_centroids))

#Compute a matrix of distances: distance between a single centroid and all custom offices
distance_matrix <- data.frame(st_distance(mun_centroids, custom_proj, by_element= FALSE))

#For each id, identify the closes custom office
closest_custom <- data.frame(apply(distance_matrix[,],1,min))
colnames(closes_custom) <- c("min.distance")

#Identify the row number in the dataframe of closest custom office
min_col_index <- apply(dist, 1, which.min)
min_col_df <- data.frame(min_col_index = min_col_index)

#Add to original municiaplity dataset the closest distance to custom office and the custom office row number in the original custom office dataset custom_proj
mun$closes_custom <- closes_custom[,1]
mun$rownumber <- min_col_df[,1]



#Add the row number as an identifier in custom office dataset
custom_proj$row_number <- seq_len(nrow(custom_proj))

#Drop geometry no longer needed
mun <- st_drop_geometry(mun)
custom_proj<-st_drop_geometry(custom_proj)

#Merge the data by row number to obtain other than the distance information, the border office information
mun<-merge(mun, custom_proj, by="row_number", all.x= TRUE)

#Save the data


