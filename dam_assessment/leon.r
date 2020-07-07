#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

## Purpose:
# extract daily flow at Nakhon Sawan
# for dam and no dam
# to prove validity of framework

## history:
# 25/06/2020 > first version

## libraries
library("tibble")
library("readr")
suppressMessages(library("dplyr"))

## input
if (length(args) != 2) {
  stop("Required: GCM ; indice")
} else {
  GCMS      <- args[1] #"G2C_"
  EXP       <- args[2] # "nodam_trim"

}

if (GCMS %in% c("H2C_", "G2C_", "M2C_", "I2C_",
                "H3C_", "G3C_", "M3C_", "I3C_")) {
  YEAR_STA <- 2006
  YEAR_END <- 2036
} else {
  YEAR_STA <- 1861
  YEAR_END <- 1891
}


for (years in seq(YEAR_STA, YEAR_END, 1)) {
  # Floodplain depth
  File_in2 <- file(description = paste("/data01/julien/models/camaflood/out/global_", GCMS, EXP, "/outflw", years , ".bin", sep = ""), open = "rb")

  d <- seq(as.Date(paste(years, "/1/1", sep = "")), as.Date(paste(years, "/12/31", sep= "")), "day")
  yearly_fld <- matrix(data = 0, nrow = length(d), ncol = 259200)
  for (nb_days in 1:length(d)) {
    t <- readBin(con = File_in2, what = "numeric", n = 259200, size = 4, endian = "little")
    #t <- ifelse(t == t[1], NA, t)
    yearly_fld[nb_days,] <- t
  }
  close(File_in2)
  # process the file
  # L = 107120
  write_csv(tibble(year = d, flow1 = yearly_fld[, 83687], flow2 = yearly_fld[,83688], flow3 = yearly_fld[,84407], flow4 = yearly_fld[, 84408]), 
            paste0("./../OUT/leon_", GCMS, EXP, ".csv"), append = TRUE)

}
