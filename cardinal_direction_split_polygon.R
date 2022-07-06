cardinal_split_function <- function(buffers1,
                                    buffers2,
                                    outfilename,
                                    plot = TRUE,
                                    cores = 5)
{ #--- create parallel processing structure ---#
  
  cl <- snow::makeCluster(spec = cores, type = "SOCK")
  doSNOW::registerDoSNOW(cl)
  
  #--- create progress text bar ---#
  
  iterations <- nrow(buffers1)
  pb <- utils::txtProgressBar(max = iterations, style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  #--- iterate parallel processing of splitting buffers into 4 cardinal-based sides ---#
  
  out_df <- foreach::foreach(i = 1:iterations, .combine = "rbind", .options.snow = opts, .packages = c("sf","dplyr")) %dopar% {
    
    bldg_carindal_polys <-data.frame()
    
    bid <- buffers1[i,]
    
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
    
    bid17 <- buffers2[i,]
    
    bid_N <- sf::st_intersection(bid17, N_box)
    bid_E <- sf::st_intersection(bid17, E_box)
    bid_S <- sf::st_intersection(bid17, S_box)
    bid_W <- sf::st_intersection(bid17, W_box)
    
    ### apply cardinal label
    bid_N$cardinal <- "North"
    bid_E$cardinal <- "East"
    bid_S$cardinal <- "South"
    bid_W$cardinal <- "West"
    
    bldg_carindal_polys <- rbind(bldg_carindal_polys, bid_N)
    bldg_carindal_polys <- rbind(bldg_carindal_polys, bid_E)
    bldg_carindal_polys <- rbind(bldg_carindal_polys, bid_S)
    bldg_carindal_polys <- rbind(bldg_carindal_polys, bid_W)
    
  }
  
  close(pb)
  
  #--- End parallel ---#
  snow::stopCluster(cl)
  
  out_df <- out_df %>% dplyr::select(-UID)
  
  write_sf(out_df, outfilename)
  
}