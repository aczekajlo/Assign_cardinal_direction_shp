#Author: Agatha Czekajlo
#Date: July 6, 2022
#Description: This script is to split a polygon into 4 parts corresponding with cardinal directions


#############     load library      ########

# install.packages(c("rgdal","tidyverse","sf","spatialEco))
library(rgdal)
library(tidyverse)
library(sf)

# install.packages(c("doParallel","doSNOW","foreach","snow"))
library(doParallel)
library(doSNOW)
library(foreach)
library(snow)

#############     load spatial polygon + clean-up     ########

polygon <- read_sf() # original polygon
buffers = st_buffer(polygon, 10) # make extra-large buffer for rotating


#############     create cardinal split function     ########

cardinal_split_function <- function(polygon,
                                    buffers,
                                    outfilename,
                                    plot = TRUE,
                                    cores = 5)
{ #--- create parallel processing structure ---#
  
  cl <- snow::makeCluster(spec = cores, type = "SOCK")
  doSNOW::registerDoSNOW(cl)
  
  #--- create progress text bar ---#
  
  iterations <- nrow(polygon)
  pb <- utils::txtProgressBar(max = iterations, style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  #--- iterate parallel processing of splitting buffers into 4 cardinal-based sides ---#
  
  out_df <- foreach::foreach(i = 1:iterations, .combine = "rbind", .options.snow = opts, .packages = c("sf","dplyr")) %dopar% {
    
    polygon_carindal_directions <-data.frame()
    
    bid <- buffers[i,] #use extra-large buffer to start
    
    poly_coords <- sf::st_coordinates(bid)
    
    centroid <- st_centroid(bid)
    centroid_coords <- sf::st_coordinates(centroid)
    centroid_X <- centroid_coords[1,1]
    centroid_Y <- centroid_coords[1,2]
    
    vertices <- st_multipoint(poly_coords) %>%
      st_zm() %>% 
      st_geometry() %>% 
      st_cast('POINT') %>% 
      st_set_crs(st_crs(bid))

    a <- as.data.frame(poly_coords)
    
    Xmax <- max(a$X)
    Xmin <- min(a$X)
    
    Ymax <- max(a$Y) 
    Ymin <- min(a$Y) 
    
    ##creating bounding boxes - cardinal directions
    N_coords = matrix(c(centroid_X, centroid_Y,
                        Xmin, Ymax,
                        Xmax, Ymax,
                        centroid_X, centroid_Y), 
                      ncol = 2, byrow = TRUE)
    
    N_box <- st_polygon(x = list(N_coords[, 1:2])) %>%  # just X and Y please
      st_sfc() %>%  # from sfg to sfc
      st_sf() %>% # from sfc to sf
      st_set_crs(st_crs(bid))
    
    E_coords = matrix(c(centroid_X, centroid_Y,
                        Xmax, Ymax,
                        Xmax, Ymin,
                        centroid_X, centroid_Y), 
                      ncol = 2, byrow = TRUE)
    
    E_box <- st_polygon(x = list(E_coords[, 1:2])) %>%  # just X and Y please
      st_sfc() %>%  # from sfg to sfc
      st_sf() %>% # from sfc to sf
      st_set_crs(st_crs(bid))
    
    S_coords = matrix(c(centroid_X, centroid_Y,
                        Xmax, Ymin,
                        Xmin, Ymin,
                        centroid_X, centroid_Y), 
                      ncol = 2, byrow = TRUE)
    
    S_box <- st_polygon(x = list(S_coords[, 1:2])) %>%  # just X and Y please
      st_sfc() %>%  # from sfg to sfc
      st_sf() %>% # from sfc to sf
      st_set_crs(st_crs(bid))
    
    W_coords = matrix(c(centroid_X, centroid_Y,
                        Xmin, Ymin,
                        Xmin, Ymax,
                        centroid_X, centroid_Y), 
                      ncol = 2, byrow = TRUE)
    
    W_box <- st_polygon(x = list(W_coords[, 1:2])) %>%  # just X and Y please
      st_sfc() %>%  # from sfg to sfc
      st_sf() %>% # from sfc to sf
      st_set_crs(st_crs(bid))
    
    pid <- polygon[i,] #use original polygon
    
    bid_N <- sf::st_intersection(pid, N_box)
    bid_E <- sf::st_intersection(pid, E_box)
    bid_S <- sf::st_intersection(pid, S_box)
    bid_W <- sf::st_intersection(pid, W_box)
    
    ### apply cardinal label
    pid_N$cardinal <- "North"
    pid_E$cardinal <- "East"
    pid_S$cardinal <- "South"
    pid_W$cardinal <- "West"
    
    polygon_carindal_directions <- rbind(polygon_carindal_directions, bid_N)
    polygon_carindal_directions <- rbind(polygon_carindal_directions, bid_E)
    polygon_carindal_directions <- rbind(polygon_carindal_directions, bid_S)
    polygon_carindal_directions <- rbind(polygon_carindal_directions, bid_W)
    
  }
  
  close(pb)
  
  #--- End parallel ---#
  snow::stopCluster(cl)
  
  write_sf(out_df, outfilename)
  
}

#############     run cardinal split function     ########

cardinal_split_function(polygon = polygon,
                        buffers = buffers,
                        outfilename = paste0())