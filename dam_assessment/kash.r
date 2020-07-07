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
  write_csv(tibble(year = d,
                   flow1 = yearly_fld[,73621],
                   flow2 = yearly_fld[,73622],
                   flow3 = yearly_fld[,73623],
                   flow4 = yearly_fld[,73624],
                   flow5 = yearly_fld[,74339],
                   flow6 = yearly_fld[,74341],
                   flow7 = yearly_fld[,74342],
                   flow8 = yearly_fld[,75063],
                   flow9 = yearly_fld[,75779],
                   flow10 = yearly_fld[,75780],
                   flow11 = yearly_fld[,76499],
                   flow12 = yearly_fld[,76500],
                   flow13 = yearly_fld[,77220],
                   flow14 = yearly_fld[,77223]), 
            paste0("./../OUT/kash_", GCMS, EXP, ".csv"), append = TRUE)

}
