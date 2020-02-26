#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

## purpose: do manually the downscaling
# from 0.5 degree ro 0.005 degree
# downscaled file include: basin, position, country and original L coordinate

## libraries
library("tibble")
library("readr")
suppressMessages(library("dplyr"))

# read cc
#region <- args[1] # "af1" ...
regions <- c("af1", "as1", "as2", "as3", "ca1", "eu1", "eu2",
             "eu3", "na1", "na2", "oc1", "sa1", "si1", "si2")

mask <- read_csv("/data01/julien/projects/camaflood/OUT/up_middle_downstream_v2.csv",
                 col_types = cols(lon = col_double(),
                                  lat = col_double(),
                                  dam = col_integer(),
                                  nx  = col_integer(),
                                  ny  = col_integer(),
                                  L   = col_integer(),
                                  jx  = col_integer(),
                                  jy  = col_integer(),
                                  position = col_integer(),
                                  country = col_factor())
                 )
# new ! added basins!
ff <- file("/data01/julien/models/camaflood/map/global_30min/basin.bin", open = "rb")
bas <- readBin(ff, what = "integer", n = 360*720, size = 4, endian = "little")
close(ff)

mask$basin <- bas
rm(bas) ; rm(ff)

# Original mask for the position of cells
pos <- tibble(L = seq(1, 360*720),
              lon = rep(seq(from = -179.75, by = 0.5, length.out = 720), times = 360),
              lat = rep(seq(from = 89.75,  by = -0.5, length.out = 360), each  = 720),
              dat = mask$position,
              cc  = mask$country,
              bas = mask$basin)

# Clean
rm(mask)

for (region in regions) {
# Regions specific
  if (region == "af1") {           # region1
    xdef   <- 11000 ; ydef   <- 14000
    xstart <- 5.000 ; ystart <- 35.000
  } else if (region == "as1") {    # region 2 
    xdef   <- 9000   ; ydef   <- 11000
    xstart <- 55.000 ; ystart <- 60.000
  } else if (region == "as2") {    # region 3
    xdef   <- 12000  ; ydef   <- 8000
    xstart <- 90.000 ; ystart <- 60.000
  } else if (region == "as3") {    # region 4
    xdef   <- 13000  ; ydef   <- 10000
    xstart <- 90.000 ; ystart <- 35.000
  } else if (region == "ca1") {    # region 5
    xdef   <- 12000    ; ydef   <- 7000
    xstart <- -120.000 ; ystart <- 40.000
  } else if (region == "eu1") {    # region 6
    xdef   <- 8000    ; ydef   <- 12000
    xstart <- -20.000 ; ystart <- 60.000
  } else if (region == "eu2") {    # region 7
    xdef   <- 13000 ; ydef   <- 8000
    xstart <- 5.000 ; ystart <- 60.000
  } else if (region == "eu3") {    # region 8
    xdef   <- 14000 ; ydef   <- 7000
    xstart <- 0.000 ; ystart <- 80.000
  } else if (region == "na1") {    # region 9
    xdef   <- 16000    ; ydef   <- 7000
    xstart <- -130.000 ; ystart <- 60.000
  } else if (region == "na2") {    # region 10
    xdef <- 23000      ; ydef <- 5000
    xstart <- -170.000 ; ystart <- 75.000
  } else if (region == "oc1") {    # region 11
    xdef <- 14000     ; ydef <- 8000
    xstart <- 110.000 ; ystart <- -10.000
  } else if (region == "sa1") {    # region 12
    xdef <- 11000    ; ydef <- 15000
    xstart <- -85.000 ; ystart <- 15.000
  } else if (region == "si1") {    # region 13
    xdef <- 12000    ; ydef <- 7000
    xstart <- 55.000 ; ystart <- 80.000
  } else if (region == "si2") {    # region 14
    xdef   <- 19000   ; ydef   <- 5000
    xstart <- 100.000 ; ystart <- 75.000
  }
  print(region)

  # check that the region does not loop around
  if ((xstart + 0.005 * xdef) > 180) {
    pos_v2 <- pos %>% filter(lon >= xstart & lon <= 180, lat <= ystart & lat >= (ystart - 0.005 * ydef))
    # add second sequence
    temp <- pos %>% filter(lon >= -180 & lon <= -165, lat <= ystart & lat >= (ystart - 0.005 * ydef))
    pos_v2 <- rbind(pos_v2, temp)
  } else {
    pos_v2 <- pos %>% filter(lon >= xstart & lon <= (xstart + 0.005 * xdef), lat <= ystart & lat >= (ystart - 0.005 * ydef))    
  }

#  pop_band <- lapply(pop_v2$dat, function(x) {if (x == -9999) {rep(-9999, times = 50)} else {rep(x/(50*50), times = 50)}})
  pos_band  <- lapply(pos_v2$dat, function(x) {if (is.na(x))   {rep(NA, times = 100)}   else {rep(x, times = 100)}})
  pos_cc    <- lapply(pos_v2$cc, function(x) {if (is.na(x))    {rep(NA, times = 100)}   else {rep(x, times = 100)}})
  pos_bas   <- lapply(pos_v2$bas, function(x) {if (is.na(x))   {rep(NA, times = 100)}   else {rep(x, times = 100)}})
  pos_l     <- lapply(pos_v2$L, function(x) {if (is.na(x))     {rep(NA, times = 100)}   else {rep(x, times = 100)}})

  pos_bands   <- unlist(pos_band)
  cc_bands    <- unlist(pos_cc, use.names = TRUE)
  basin_bands <- unlist(pos_bas, use.names = TRUE)
  l_bands     <- unlist(pos_l, use.names = TRUE)

  pos_matrix  <- matrix(data = pos_bands, ncol = xdef, nrow = ydef/100, byrow = TRUE)
  cc_matrix   <- matrix(data = cc_bands, ncol = xdef, nrow = ydef/100, byrow = TRUE)
  bas_matrix  <- matrix(data = basin_bands, ncol = xdef, nrow = ydef/100, byrow = TRUE)
  l_matrix    <- matrix(data = l_bands, ncol = xdef, nrow = ydef/100, byrow = TRUE)

  final_pos <- pos_matrix[rep(1:nrow(pos_matrix), each = 100), ]
  final_cc  <- cc_matrix[rep(1:nrow(cc_matrix), each = 100), ]
  final_bas <- bas_matrix[rep(1:nrow(bas_matrix), each = 100), ]
  final_l   <- l_matrix[rep(1:nrow(l_matrix), each = 100), ]
  
  to_write <- file(paste0("/data01/julien/projects/camaflood/OUT/", region, "_country_code_0.005deg.bin"), open = "wb")
  writeBin(as.vector(t(final_cc)), to_write, size = 4, endian = "little")
  close(to_write)

  to_write <- file(description = paste0("/data01/julien/projects/camaflood/OUT/", region, "_position_0.005deg.bin"), open = "wb")
  writeBin(as.vector(t(final_pos)), to_write, endian = "little")
  close(to_write)

  to_write <- file(description = paste0("/data01/julien/projects/camaflood/OUT/", region, "_basins_0.005deg.bin"), open = "wb")
  writeBin(as.vector(t(final_bas)), to_write, endian = "little")
  close(to_write)

  to_write <- file(description = paste0("/data01/julien/projects/camaflood/OUT/", region, "_L_0.005deg.bin"), open = "wb")
  writeBin(as.vector(t(final_l)), to_write, endian = "little")
  close(to_write)  
}
