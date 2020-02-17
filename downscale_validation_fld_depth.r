#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)
## Purpose:
# downscale the maximum flooded depth for all grid cells for comparison purpose

## Libraries
library("tibble")
library("readr")
suppressMessages(library("dplyr"))

## read commanf line inputs
if (length(args) != 2) {
  stop("Required: GCM ; indice")
} else {
  GCMS      <- args[1] #"G2C_"
  EXP       <- args[2] # "nodam_trim"

}


# get the time period
if (GCMS %in% c("H2C_", "G2C_", "M2C_", "I2C_",
                "H3C_", "G3C_", "M3C_", "I3C_")) {
  YEAR_STA <- 2006
  YEAR_END <- 2036
} else {
  YEAR_STA <- 1861
  YEAR_END <- 1891
}


########## start inputs



for (years in seq(YEAR_STA, YEAR_END, 1)) {
  # Floodplain depth
  File_in2 <- file(description = paste("/data01/julien/models/camaflood/out/global_", GCMS, EXP, "/flddph", years , ".bin", sep = ""), open = "rb")

  d <- seq(as.Date(paste(years, "/1/1", sep = "")), as.Date(paste(years, "/12/31", sep= "")), "day")
  yearly_fld <- matrix(data = 0, nrow = length(d), ncol = 259200)
  for (nb_days in 1:length(d)) {
    t <- readBin(con = File_in2, what = "numeric", n = 259200, size = 4, endian = "little")
    #t <- ifelse(t == t[1], NA, t)
    yearly_fld[nb_days,] <- t
  }
  close(File_in2)

  # max
  annual_max_fldd <- apply(yearly_fld, 2, function(x) max(x, na.rm = TRUE))
  
  File_out <- file(paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/verification_max_fldh_", years, ".bin"), open = "wb")
  t  <- writeBin(temp$depth, con = File_out, size = 4, endian = "little")
  close(File_out)
  
}

