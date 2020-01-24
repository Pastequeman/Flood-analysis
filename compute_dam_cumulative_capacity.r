#!/usr/bin/Rscript
#
#Purpose: calculate the cumulative capacity of a grid cell
#         reflects how many and how big are the dam(s) located above
#         the stream

## libraries
library("tibble")
suppressMessages(library("tidyr"))
suppressMessages(library("dplyr"))
library("readr")

## function
compute_cum_capacity <- function(watershed) {
  inv_cells        <- watershed %>% filter(dam > 0)

  for (i in 1:nrow(inv_cells)) {
    message(paste("Iteration", i, "of", nrow(inv_cells)))
    test <- inv_cells[i, ]  # the line currently checked
    if (test$jx == -9) {
      ## next cell is the sea
      watershed[watershed$L == test$L,]$cum_capacity <- watershed[watershed$L == test$L,]$cum_capacity + watershed[watershed$L == test$L,]$capacity

    } else {
      ## network
      riv_cells <- data.frame(lat          = c(test$lat, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$lat),
                              lon          = c(test$lon, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$lon),
                              dam          = c(test$dam, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$dam),
                              capacity     = c(test$capacity, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$capacity),
                              L            = c(test$L, watershed[watershed$nx == test$jx   & watershed$ny == test$jy,]$L),
                              jx           = c(test$jx, watershed[watershed$nx == test$jx  & watershed$ny == test$jy,]$jx),
                              jy           = c(test$jy, watershed[watershed$nx == test$jx  & watershed$ny == test$jy,]$jy),
                              cum_capacity = c(test$cum_capacity, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$cum_capacity))
      
      # print(riv_cells) ## DEBUG
      while (riv_cells[nrow(riv_cells),]$jx != -9) {
        # Iterate until the sea
        test <- riv_cells[nrow(riv_cells), ]
        # add the next cell
        riv_seq <- data.frame(lat          = c(test$lat, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$lat),
                              lon          = c(test$lon, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$lon),
                              dam          = c(test$dam, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$dam),
                              capacity     = c(test$capacity, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$capacity),
                              L            = c(test$L, watershed[watershed$nx == test$jx   & watershed$ny == test$jy,]$L),
                              jx           = c(test$jx, watershed[watershed$nx == test$jx  & watershed$ny == test$jy,]$jx),
                              jy           = c(test$jy, watershed[watershed$nx == test$jx  & watershed$ny == test$jy,]$jy),
                              cum_capacity = c(test$cum_capacity, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$cum_capacity))
        
        riv_cells <- rbind(riv_cells, riv_seq[2, ])
      } # while loop
      
      ## sequence finished
      # add the capacity of the dam to the sequence
      watershed[watershed$L %in% riv_cells$L, ]$cum_capacity <- watershed[watershed$L %in% riv_cells$L, ]$cum_capacity +
        watershed[watershed$L %in% riv_cells$L[1], ]$capacity

    }   # if 
  }     # for loop
  watershed <<- watershed
  return(watershed)
}
## End function


## preparation data
# dam
to.read <-  file(description = "./../../camaflood/map/GRanD_L_20000000.CaMa.btyswp.hlf", open = "rb")
t       <- readBin(con = to.read, 
                   what = "numeric", 
                   n = 259200,         #  360 * 720
                   size = 4,           # size?
                   endian = "little")     # here size is super important
close(to.read)

# river network
to.read <-  file(description = paste("./../../camaflood/map/global_30min/nextxy.bin", sep = ""), open = "rb")
next_x  <- readBin(con = to.read, what = "integer", n = 259200,  size = 4, endian = "little")
next_y  <- readBin(con = to.read, what = "integer", n = 259200,  size = 4, endian = "little")
close(to.read)

# capacity of dams
to.read <-  file(description = "./../../DAT/GRanD_L_20000000.CaMa_capacity.hlf", open = "rb")
cap <- readBin(con = to.read, 
                   what = "numeric", 
                   n = 259200,         #  360 * 720
                   size = 4,           # size?
                   endian = "big")     # here size is super important
close(to.read)

# final files
global_file <- tibble(lon = rep(seq(-179.75, 179.75, by = 0.5), times = 360),
                      lat = rep(seq(89.75, -89.75, by = -0.5), each = 720),
                      dam = ifelse(t == t[1], 0, t),
                      capacity = ifelse(cap == cap[1], 0, cap),
                      nx = rep(seq(1, 720, 1), times = 360),
                      ny = rep(seq(1, 360, 1), each = 720),
                      L = seq(1, 259200, 1),
                      jx = ifelse(next_x == next_x[1], NA, next_x),
                      jy = ifelse(next_y == next_y[1], NA, next_y),
                      cum_capacity = 0)
# clean
rm(t) ; rm(next_x) ; rm(next_y) ; rm(cap)

## run
global_file_v2 <- compute_cum_capacity(global_file)

## save
write_csv(global_file_v2, "./../../OUT/cumulative_dam_capacity.csv")
