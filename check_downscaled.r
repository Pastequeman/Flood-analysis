#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)


## Libraries
library("tibble")
library("readr")
suppressMessages(library("dplyr"))

## read commanf line inputs
if (length(args) != 2) {
  stop("Required: GCM ; experiment")
} else {
  GCMS      <- args[1] #"G2C_"
  EXP       <- args[2] # "nodam_trim"

}

cells <- c(84661, 85860, 95581, 110002) # 0.5 resolution original cells
cor <- tibble(lon = c(30.25, -90.25, 90.25, 100.75),
              lat = c(31.25, 30.25, 23.75, 13.75))

# grid reference
aa <- tibble(L = seq(1:(720*360)), nx = rep(seq(1:720), times = 360), ny = rep(seq(1:360), each = 720))
reference <- aa[aa$L %in% cells,]

#ref_f <- file(paste0("/data01/julien/models/camaflood/map/global_30min/inpmat-0.5deg.bin"), open = "rb")
#reference_x <- readBin(ref_f, what = "integer", n = (360*720*27), size = 4, endian = "little") # 27 is the maximum possible overlap
#reference_y <- readBin(ref_f, what = "integer", n = (360*720*27), size = 4, endian = "little")
#close(ref_f) ; rm(ref_f)

#reference <- tibble(cell = cells,
#                    nx = reference_x[cells],
#                    ny = reference_y[cells])
#rm(reference_x) ; rm(reference_y)
# get the time period
if (GCMS %in% c("H2C_", "G2C_", "M2C_", "I2C_",
                "H3C_", "G3C_", "M3C_", "I3C_")) {
  YEAR_STA <- 2006
  YEAR_END <- 2036
} else {
  YEAR_STA <- 1861
  YEAR_END <- 2005
}

for (years in seq(YEAR_STA, YEAR_END, 1)) {
  # original flooded fraction
  File_fldfrc <- file(description = paste("/data01/julien/models/camaflood/out/global_", GCMS, EXP, "/fldfrc", years , ".bin", sep = ""), open = "rb")
  d <- seq(as.Date(paste(years, "/1/1", sep = "")), as.Date(paste(years, "/12/31", sep= "")), "day")
  yearly_frc <- matrix(data = 0, nrow = length(d), ncol = 4 )
  for (nb_days in 1:length(d)) {
    t  <- readBin(con = File_fldfrc, what = "numeric", n = 259200, size = 4, endian = "little")
    yearly_frc[nb_days,] <- t[cells]
  }
  close(File_fldfrc)
  max_frc <- apply(yearly_frc, 2, function(x) max(x, na.rm = TRUE))
  
  # now the downscaled data
  regions <- c("af1", "na1", "as1", "as3")
  ave_flood <- tibble(region = regions,
                        frc = 0)
  for (i in 1:4) {
    region <- regions[i]
    
       # Regions specific
  if (region == "af1") {           # region1
    xdef <- 11000   ; ydef <- 14000
    xstart <- 5.000 ; ystart <- 35.000
  } else if (region == "as1") {    # region 2 
    xdef <- 9000     ; ydef <- 11000
    xstart <- 55.000 ; ystart <- 60.000
  } else if (region == "as2") {    # region 3
    xdef <- 12000   ; ydef <- 8000
    xstart <- 90.000 ; ystart <- 60.000
  } else if (region == "as3") {    # region 4
    xdef <- 13000    ; ydef <- 10000
    xstart <- 90.000 ; ystart <- 35.000
  } else if (region == "ca1") {    # region 5
    xdef <- 12000   ; ydef <- 7000
    xstart <- -120.000 ; ystart <- 40.000
  } else if (region == "eu1") {    # region 6
    xdef <- 8000 ; ydef <- 12000
    xstart <- -20.000 ; ystart <- 60.000
  } else if (region == "eu2") {    # region 7
    xdef <- 13000 ; ydef <- 8000
    xstart <- 5.000 ; ystart <- 60.000
  } else if (region == "eu3") {    # region 8
    xdef <- 14000 ; ydef <- 7000
    xstart <- 0.000 ; ystart <- 80.000
  } else if (region == "na1") {    # region 9
    xdef <- 16000      ; ydef <- 7000
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
    xdef <- 19000     ; ydef <- 5000
    xstart <- 100.000 ; ystart <- 75.000
  }
    #

    down_f <- file(paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/fld_", region, "_", years, ".flood"), open = "rb")
    t <- readBin(down_f, what = "numeric", size = 4, n = (xdef*ydef))
    close(down_f) ; rm(down_f)

    # this is neccessary to know which sub-grid cells contribute to the original 0.5 resolution original cell
    hires_f <- file(paste0("/data01/julien/models/camaflood/map/global_30min/hires/", region, ".catmxy"), open = "rb")
    x_c <- readBin(hires_f, what = "integer", size = 2, n = (xdef*ydef), endian = "little")
    y_c <- readBin(hires_f, what = "integer", size = 2, n = (xdef*ydef), endian = "little")
    close(hires_f) ; rm(hires_f)

    # the original cell
    dw <- tibble(lon = rep(seq((xstart+0.0025), by = 0.005, length.out = xdef), times = ydef),
                 lat = rep(seq((ystart-0.0025), by = -0.005, length.out = ydef), each = xdef),
                 dat = t,
                 x = x_c,
                 y = y_c)
    rm(x_c) ; rm(y_c)
    # Extract relevant info
    a <- dw %>% filter(lon >= (cor$lon[i]-0.25)  & lon <= (cor$lon[i]+0.25),
                       lat >= (cor$lat[i]-0.25) & lat <= (cor$lat[i]+0.25))
    # now explore the 8 neighboring cells
    for (j in seq(1, 8)) {
      if (j == 1) {
        x_add <- 0.5
        y_add <- 0 
      } else if (j == 2) {
        x_add <- -0.5
        y_add <- 0         
      } else if (j == 3) {
        x_add <- 0.5
        y_add <- 0.5
      } else if (j == 4) {
        x_add <- 0.5
        y_add <- -0.5 
      } else if (j == 5) {
        x_add <- 0
        y_add <- -0.5        
      } else if (j == 6) {
        x_add <- 0
        y_add <- 0.5
      } else if (j == 7) {
        x_add <- -0.5
        y_add <- 0.5
      } else {
        x_add <- -0.5
        y_add <- -0.5
      }
      extra <-
      dw %>% filter(lon >= (cor$lon[i]-0.25+x_add)  & lon <= (cor$lon[i]+0.25+x_add),
                    lat >= (cor$lat[i]-0.25+y_add) & lat <= (cor$lat[i]+0.25+y_add))
      # add relevaent sub-cells
      if (nrow(extra) > 0) {
        a <- rbind(a, extra[extra$x == reference$nx[i] & extra$y == reference$ny[i],])        
      }
    } # end extra cells
    a$dat <- ifelse(a$dat == -9999, NA, a$dat)
    # flooded fraction approximatly:
    ave_flood$frc[i] <-  sum(!is.na(a$dat) & a$dat != 0 & a$x == reference$nx[i] & a$y == reference$ny[i]) / sum(!is.na(a$dat) & a$x == reference$nx[i] & a$y == reference$ny[i])
  }
  out <- tibble(region = rep(regions ,2), frc = c(max_frc, ave_flood$frc), scale = rep(c(0.5, 0.005), each = 4))
  # save?
  write_csv(out, paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/frc_check.csv"), append = TRUE)

}
