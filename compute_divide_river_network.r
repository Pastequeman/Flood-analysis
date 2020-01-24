#!/usr/bin/Rscript
#
# Purpose: Location of the grid cell relative to dams
# 1 outside, 2 downstream, 3 upstream, 4 middlestream

## libraries
library("tibble")
suppressMessages(library("tidyr"))
suppressMessages(library("dplyr"))
library("readr")

## function
compute_division   <- function(watershed) {
  inv_cells        <- watershed %>% filter(dam > 0)
  # phase 1 #
  for (i in 1:nrow(inv_cells)) {
    message(paste("1st iteration", i, "of", nrow(inv_cells)))
    test <- inv_cells[i, ]  # the line currently checked
    if (test$jx == -9) {
      ## next cell is the sea
      watershed[watershed$L == test$L,]$position <- 2
    } else {
      ## network
      riv_cells <- data.frame(lat          = c(test$lat, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$lat),
                              lon          = c(test$lon, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$lon),
                              dam          = c(test$dam, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$dam),
                              L            = c(test$L, watershed[watershed$nx == test$jx   & watershed$ny == test$jy,]$L),
                              jx           = c(test$jx, watershed[watershed$nx == test$jx  & watershed$ny == test$jy,]$jx),
                              jy           = c(test$jy, watershed[watershed$nx == test$jx  & watershed$ny == test$jy,]$jy),
                              position     = c(test$position, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$position))
      
      # Print(riv_cells) ## DEBUG
      while (riv_cells[nrow(riv_cells),]$jx != -9) {
        # Iterate until the sea
        test <- riv_cells[nrow(riv_cells), ]
        # add the next cell
        riv_seq <- data.frame(lat          = c(test$lat, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$lat),
                              lon          = c(test$lon, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$lon),
                              dam          = c(test$dam, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$dam),
                              L            = c(test$L, watershed[watershed$nx == test$jx   & watershed$ny == test$jy,]$L),
                              jx           = c(test$jx, watershed[watershed$nx == test$jx  & watershed$ny == test$jy,]$jx),
                              jy           = c(test$jy, watershed[watershed$nx == test$jx  & watershed$ny == test$jy,]$jy),
                              position     = c(test$position, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$position))
        
        riv_cells <- rbind(riv_cells, riv_seq[2, ])
      } # while loop
      
      ## sequence finished
      # Separe between middle and downstream
      if (sum(riv_cells$dam >= 1, na.rm = TRUE) > 1) {
        # at least two dam cells
        
        # only dams cells
        dam_cells <- riv_cells %>% filter(dam >= 1)
        for (j in 1:(nrow(riv_cells))) {
          if (sum(riv_cells[1:j, ]$dam >= 1, na.rm = TRUE) == sum(dam_cells$dam)) {
            print("segment found!")
            break
          }
        }
        watershed[watershed$L %in% riv_cells[1:(j-1), ]$L, ]$position <- 4
        watershed[watershed$L %in% riv_cells[(j:nrow(riv_cells)), ]$L, ]$position <- 2
      } else {
        # a single dam cell
        watershed[watershed$L %in% riv_cells$L, ]$position <- 2
      }

    }   # if
  }     # for loop


  ## save the watershed
  watershed <<- watershed

  
  ## the sea
  watershed[watershed$L %in%  watershed[is.na(watershed$jx),]$L,]$position <- NA

  ## want only cells that are not already flagged, no dam and no sea
  inv_cells <- watershed %>% filter(dam == 0, is.na(jx) != TRUE, is.na(position) == TRUE)
  
  ## 2nd phase
    for (i in 1:nrow(inv_cells)) {
    message(paste("2nd iteration", i, "of", nrow(inv_cells)))
    test <- inv_cells[i, ]  # the line currently checked
#    if (is.na(test$jx) == TRUE) {
#      ## NAs
#      watershed[watershed$L %in% test$L, ]$position <- NA
    #    }else
    if (is.na(test$position) != TRUE) {
      next
    } else if (test$jx == -9) {
      ## no network
      watershed[watershed$L %in% test$L, ]$position <- 1
    } else {
      riv_cells <- data.frame(lat      = c(test$lat, watershed[watershed$nx      == test$jx & watershed$ny == test$jy,]$lat),
                              lon      = c(test$lon, watershed[watershed$nx      == test$jx & watershed$ny == test$jy,]$lon),
                              jx       = c(test$jx, watershed[watershed$nx       == test$jx & watershed$ny == test$jy,]$jx),
                              jy       = c(test$jy, watershed[watershed$nx       == test$jx & watershed$ny == test$jy,]$jy),
                              L        = c(test$L, watershed[watershed$nx        == test$jx & watershed$ny == test$jy,]$L),
                              dam      = c(test$dam, watershed[watershed$nx      == test$jx & watershed$ny == test$jy,]$dam),
                              position = c(test$position, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$position))

      while (riv_cells[nrow(riv_cells), ]$jx != -9 & is.na(riv_cells[nrow(riv_cells), ]$position) == TRUE) {
        # Iterate until the sea or an already flagged cells
        test <- riv_cells[nrow(riv_cells), ]
        
        if (test$jx == -9) {
          break
          # } else if (is.na(test$position) != TRUE) {
          #   # if reached an already check cells! stop!
          #   break
        } else {
          # add the next cell
          riv_seq <- data.frame(lat    = c(test$lat, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$lat),
                                lon      = c(test$lon, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$lon),
                                jx       = c(test$jx, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$jx),
                                jy       = c(test$jy, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$jy),
                                L        = c(test$L, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$L),
                                dam      = c(test$dam, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$dam),
                                position = c(test$position, watershed[watershed$nx == test$jx & watershed$ny == test$jy,]$position))
          
          riv_cells <- rbind(riv_cells, riv_seq[2, ])  
        }
        
      } # while loop

      if (is.na(riv_cells[nrow(riv_cells), ]$position) != TRUE) {
        # what type of cells detected
        ## 1 or E
        if (riv_cells[nrow(riv_cells), ]$position == 1) {
          watershed[watershed$L %in% riv_cells[is.na(riv_cells$position), ]$L, ]$position <- 1
          
        ## 2 of D
        } else if(riv_cells[nrow(riv_cells), ]$position == 2) {
          if (riv_cells[nrow(riv_cells), ]$dam > 0) {
            watershed[watershed$L %in% riv_cells[is.na(riv_cells$position), ]$L, ]$position <- 3
          } else {
            watershed[watershed$L %in% riv_cells[is.na(riv_cells$position), ]$L, ]$position <- 1  
          }
          
        ## 3 or U
        } else if (riv_cells[nrow(riv_cells), ]$position == 3) {
          watershed[watershed$L %in% riv_cells[is.na(riv_cells$position), ]$L, ]$position <- 3
          
        ## 4 or M
        } else if (riv_cells[nrow(riv_cells), ]$position == 4) {
          watershed[watershed$L %in% riv_cells[is.na(riv_cells$position), ]$L, ]$position <- 3
          
        }
      } else {
        ## no cells marked at all
        watershed[watershed$L %in% riv_cells[is.na(riv_cells$position), ]$L, ]$position <- 1
      }

    } # main if
  }   # for loop
  
  ## wrap-up
  watershed <<- watershed
  return(watershed)
}

## prep dataset
# the watershed file should contains:
#   lon   ;   lat    ;  dam    ;  nx   ; ny     ;   L   ;  jx    ; jy 
#
# create the file:
# dams
to.read <-  file(description = "/data01/julien/models/camaflood/map/GRanD_L_20000000.CaMa.btyswp.norunofriver.hlf", open = "rb")
t       <- readBin(con = to.read, 
                   what = "numeric", 
                   n = 259200,         #  360 * 720
                   size = 4,           # size?
                   endian = "little")     # here size is super important
close(to.read)

# river network
to.read <-  file(description = paste("/data01/julien/models/camaflood/map/global_30min/nextxy.bin", sep = ""), open = "rb")
next_x  <- readBin(con = to.read, what = "integer", n = 259200,  size = 4, endian = "little")
next_y  <- readBin(con = to.read, what = "integer", n = 259200,  size = 4, endian = "little")
close(to.read)

global_file <- tibble(lon = rep(seq(-179.75, 179.75, by = 0.5), times = 360),
                      lat = rep(seq(89.75, -89.75, by = -0.5), each = 720),
                      dam = ifelse(t == t[1], 0, t),
                      nx = rep(seq(1, 720, 1), times = 360),
                      ny = rep(seq(1, 360, 1), each = 720),
                      L = seq(1, 259200, 1),
                      jx = ifelse(next_x == next_x[1], NA, next_x),
                      jy = ifelse(next_y == next_y[1], NA, next_y),
                      position = NA)
# clean
rm(t) ; rm(next_x) ; rm(next_y)

## run
global_file_v2 <- compute_division(global_file)

## save
write_csv(global_file_v2, "./../OUT/up_middle_downstream.csv")
