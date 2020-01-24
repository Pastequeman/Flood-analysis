#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

## purpose: do manually the downscaling
# from 15 min (0.25 degree) to 0.005 degree

## libraries
library("tibble")
library("readr")
suppressMessages(library("dplyr"))

# read cc
#region <- args[1] # "af1" ...
regions <- c("af1", "as1", "as2", "as3", "ca1", "eu1", "eu2",
             "eu3", "na1", "na2", "oc1", "sa1", "si1", "si2")

population <- scan("/data01/julien/projects/camaflood/DAT/population/gpw_v4_population_count_rev11_2010_15_min_cleaned.asc",
                   quiet = TRUE)

pop <- tibble(L = seq(1, 720*1440),
              lon = rep(seq(from = -179.875, by = 0.25, length.out = 1440), times = 720),
              lat = rep(seq(from = -89.875,  by = 0.25, length.out = 720), each  = 1440),
              dat = population)


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

# Original mask for the position of cells
pos <- tibble(L = seq(1, 360*720),
              lon = rep(seq(from = -179.75, by = 0.5, length.out = 720), times = 360),
              lat = rep(seq(from = 89.75,  by = -0.5, length.out = 360), each  = 720),
              dat = mask$position,
              cc  = mask$country)

# Clean
rm(population) ; rm(mask)

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
    start <- -85.000 ; ystart <- 15.000
  } else if (region == "si1") {    # region 13
    xdef <- 12000    ; ydef <- 7000
    xstart <- 55.000 ; ystart <- 80.000
  } else if (region == "si2") {    # region 14
    xdef   <- 19000   ; ydef   <- 5000
    xstart <- 100.000 ; ystart <- 75.000
  }
  print(region)

  # check that the region dows not loop around
  if ((xstart + 0.005 * xdef) > 180) {
    # If correctly done, the size of the frame should be equal to: xdef*ydef/(50*50)
    original_seq <- seq(xstart, by = 0.25, length.out = xdef/50)
    new_lim <- original_seq[length(original_seq)] - 180

    pop_v2 <- tibble(lon = rep(c(seq((xstart-0.25/2), 180, 0.25), seq(-179.875, by = 0.25, length.out = new_lim/0.25)), times = 5000/50),
                     lat = rep(seq((ystart-0.25/2), by = -0.25, length.out = ydef/50), each = xdef/50))
    pop_v2 <- pop_v2 %>% left_join(pop, by = c("lon", "lat"))

    pos_v2 <- tibble(lon = rep(c(seq((xstart+0.5/2), to = 180, by = 0.5),
                                 seq(-179.75, by = 0.50, length.out = new_lim/0.50)), times = ydef/100),
                     lat = rep(seq((ystart-0.5/2), by = -0.5, length.out = ydef/100), each = xdef/100))
    pos_v2 <- pos_v2 %>% left_join(pos, by = c("lon", "lat"))
    
  } else {
    # Relevant population data
    pop_v2 <- pop %>% filter(lon >= xstart & lon <= (xstart + 0.005 * xdef), lat <= ystart & lat >= (ystart - 0.005 * ydef))
    pos_v2 <- pos %>% filter(lon >= xstart & lon <= (xstart + 0.005 * xdef), lat <= ystart & lat >= (ystart - 0.005 * ydef))    
  }
  

  
  
  pop_band <- lapply(pop_v2$dat, function(x) {if (x == -9999) {rep(-9999, times = 50)} else {rep(x/(50*50), times = 50)}})
  pos_band <- lapply(pos_v2$dat, function(x) {if (is.na(x))   {rep(NA, times = 50)}    else {rep(x, times = 50)}})
  pos_cc   <- lapply(pos_v2$cc, function(x) {if (is.na(x))   {rep(NA, times = 50)}    else {rep(x, times = 50)}})
  
  pop_bands <- unlist(pop_band)
  pos_bands <- unlist(pos_band)
  cc_bands  <- unlist(pos_cc, use.names = TRUE)

  # check that population count matches > OK
  b_matrix   <- matrix(data = pop_bands, ncol = xdef, nrow = ydef/50, byrow = TRUE)
  pos_matrix <- matrix(data = pos_bands, ncol = xdef, nrow = ydef/50, byrow = TRUE)
  cc_matrix  <- matrix(data = cc_bands, ncol = xdef, nrow = ydef/50, byrow = TRUE)
  
  final_data <- apply(b_matrix,  2, function(x) rep(x, each = 50))
  final_pos  <- apply(pos_matrix,2, function(x) rep(x, each = 50))
  final_cc   <- apply(cc_matrix, 2, function(x) rep(x, each = 50))

  cc <- as.vector(final_cc)
  # c_v2 <- levels(pos_v2$cc)[cc] I can use this directly to retreive th county same
  to_write <- file(paste0("/data01/julien/projects/camaflood/OUT/", region, "_country_code_0.005deg.bin"), open = "wb")
  writeBin(cc, to_write, size = 4, endian = "little")
  close(to_write)
  
  to_write <- file(description = paste0("/data01/julien/projects/camaflood/OUT/", region, "_pop_0.005deg.bin"), open = "wb")
  writeBin(as.vector(final_data), to_write, endian = "little")
  close(to_write)

  to_write <- file(description = paste0("/data01/julien/projects/camaflood/OUT/", region, "_position_0.005deg.bin"), open = "wb")
  writeBin(as.vector(final_pos), to_write, endian = "little")
  close(to_write)
}
