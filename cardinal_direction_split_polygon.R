#Author: Agatha Czekajlo
#Date: Nov. 8, 2022
#Description: This script is to split a polygon into 8 parts corresponding with cardinal directions:
#North-West-West, North-North-West, North-North-East, North-East-East, South-East-East, South-South-East, South-South-West, South-West-West


#############     load library      ########

# install.packages(c("rgdal","tidyverse","sf"))
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
buffers = st_buffer(polygon, 10) # make extra-large buffer for rotating, in this case it's 10 (units of sf crs)


#############     create cardinal split function     ########

cardinal_split_function <- function(polygon,
                                    buffers,
                                    outfilename,
                                    plot = TRUE,
                                    cores = 5) #can change cores
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
    Xmid <- (Xmax+Xmin)/2
    
    Ymax <- max(a$Y) 
    Ymin <- min(a$Y)
    Ymid <- (Ymax+Ymin)/2
    
    ##creating bounding boxes - 8 cardinal directions
    ##creating bounding boxes - cardinal directions NWW, NNW, NNE, NEE, SEE, SSE, SSW, SWW
    NWW_coords = matrix(c(centroid_X, centroid_Y,
                          Xmin, Ymid,
                          Xmin, Ymax,
                          centroid_X, centroid_Y), 
                        ncol = 2, byrow = TRUE)
    
    NWW_box <- st_polygon(x = list(NWW_coords[, 1:2])) %>%  # just X and Y please
      st_sfc() %>%  # from sfg to sfc
      st_sf() %>% # from sfc to sf
      st_set_crs(st_crs(bid))
    
    NNW_coords = matrix(c(centroid_X, centroid_Y,
                          Xmin, Ymax,
                          Xmid, Ymax,
                          centroid_X, centroid_Y), 
                        ncol = 2, byrow = TRUE)
    
    NNW_box <- st_polygon(x = list(NNW_coords[, 1:2])) %>%  # just X and Y please
      st_sfc() %>%  # from sfg to sfc
      st_sf() %>% # from sfc to sf
      st_set_crs(st_crs(bid))
    
    NNE_coords = matrix(c(centroid_X, centroid_Y,
                          Xmid, Ymax,
                          Xmax, Ymax,
                          centroid_X, centroid_Y), 
                        ncol = 2, byrow = TRUE)
    
    NNE_box <- st_polygon(x = list(NNE_coords[, 1:2])) %>%  # just X and Y please
      st_sfc() %>%  # from sfg to sfc
      st_sf() %>% # from sfc to sf
      st_set_crs(st_crs(bid))
    
    NEE_coords = matrix(c(centroid_X, centroid_Y,
                          Xmax, Ymax,
                          Xmax, Ymid,
                          centroid_X, centroid_Y), 
                        ncol = 2, byrow = TRUE)
    
    NEE_box <- st_polygon(x = list(NEE_coords[, 1:2])) %>%  # just X and Y please
      st_sfc() %>%  # from sfg to sfc
      st_sf() %>% # from sfc to sf
      st_set_crs(st_crs(bid))
    
    SEE_coords = matrix(c(centroid_X, centroid_Y,
                          Xmax, Ymid,
                          Xmax, Ymin,
                          centroid_X, centroid_Y), 
                        ncol = 2, byrow = TRUE)
    
    SEE_box <- st_polygon(x = list(SEE_coords[, 1:2])) %>%  # just X and Y please
      st_sfc() %>%  # from sfg to sfc
      st_sf() %>% # from sfc to sf
      st_set_crs(st_crs(bid))
    
    SSE_coords = matrix(c(centroid_X, centroid_Y,
                          Xmax, Ymin,
                          Xmid, Ymin,
                          centroid_X, centroid_Y), 
                        ncol = 2, byrow = TRUE)
    
    SSE_box <- st_polygon(x = list(SSE_coords[, 1:2])) %>%  # just X and Y please
      st_sfc() %>%  # from sfg to sfc
      st_sf() %>% # from sfc to sf
      st_set_crs(st_crs(bid))
    
    SSW_coords = matrix(c(centroid_X, centroid_Y,
                          Xmid, Ymin,
                          Xmin, Ymin,
                          centroid_X, centroid_Y), 
                        ncol = 2, byrow = TRUE)
    
    SSW_box <- st_polygon(x = list(SSW_coords[, 1:2])) %>%  # just X and Y please
      st_sfc() %>%  # from sfg to sfc
      st_sf() %>% # from sfc to sf
      st_set_crs(st_crs(bid))
    
    SWW_coords = matrix(c(centroid_X, centroid_Y,
                          Xmin, Ymin,
                          Xmin, Ymid,
                          centroid_X, centroid_Y), 
                        ncol = 2, byrow = TRUE)
    
    SWW_box <- st_polygon(x = list(SWW_coords[, 1:2])) %>%  # just X and Y please
      st_sfc() %>%  # from sfg to sfc
      st_sf() %>% # from sfc to sf
      st_set_crs(st_crs(bid))
    
    pid <- polygon[i,] #use original polygon
    
    pid_NWW <- sf::st_intersection(pid, NWW_box)
    pid_NNW <- sf::st_intersection(pid, NNW_box)
    pid_NNE <- sf::st_intersection(pid, NNE_box)
    pid_NEE <- sf::st_intersection(pid, NEE_box)
    pid_SEE <- sf::st_intersection(pid, SEE_box)
    pid_SSE <- sf::st_intersection(pid, SSE_box)
    pid_SSW <- sf::st_intersection(pid, SSW_box)
    pid_SWW <- sf::st_intersection(pid, SWW_box)
    
    ### apply cardinal label
    pid_NWW$cardinal <- "North-West-West"
    pid_NNW$cardinal <- "North-North-West"
    pid_NNE$cardinal <- "North-North-East"
    pid_NEE$cardinal <- "North-East-East"
    pid_SEE$cardinal <- "South-East-East"
    pid_SSE$cardinal <- "South-South-East"
    pid_SSW$cardinal <- "South-South-West"
    pid_SWW$cardinal <- "South-West-West"
    
    polygon_carindal_directions <- rbind(polygon_carindal_directions, bid_NWW)
    polygon_carindal_directions <- rbind(polygon_carindal_directions, bid_NNW)
    polygon_carindal_directions <- rbind(polygon_carindal_directions, bid_NNE)
    polygon_carindal_directions <- rbind(polygon_carindal_directions, bid_NEE)
    polygon_carindal_directions <- rbind(polygon_carindal_directions, bid_SEE)
    polygon_carindal_directions <- rbind(polygon_carindal_directions, bid_SSE)
    polygon_carindal_directions <- rbind(polygon_carindal_directions, bid_SSW)
    polygon_carindal_directions <- rbind(polygon_carindal_directions, bid_SWW)
    
  }
  
  close(pb)
  
  #--- End parallel ---#
  snow::stopCluster(cl)
  
  write_sf(out_df, outfilename)
  
}

#############     run cardinal split function     ########
outpath <- "Z:\\"

cardinal_split_function(polygon = polygon,
                        buffers = buffers,
                        outfilename = paste0(outpath,"outname.shp"))
