#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

## Purpose: assess the occurence of floods
# (100-y extreme discharge, hist)


## libraries:
library("tibble")
library("readr")
suppressMessages(library("dplyr"))

########### start inputs
if (length(args) != 3 ) {
  stop("error in cmd parameters > GCM | EXP | indice")
} else {
  GCMS     <- args[1] #"I2C_"
  EXP      <- args[2] #"nodam_trim"
  indice   <- args[3] # max or quantile
}



if (GCMS == "H0C_" | GCMS == "M0C_" | GCMS == "G0C_" | GCMS == "I0C_") {
  YEAR_STA <- 1900
  YEAR_END <- 2005
} else {
  YEAR_STA <- 2006
  YEAR_END <- 2099
}

if (GCMS %in% c("H0C_", "H2C_", "H3C_")) {
  ref <- "H0C_"
} else if (GCMS %in% c("M0C_", "M2C_", "M3C_")) {
  ref <- "M0C_"  
} else if (GCMS %in% c("I0C_", "I2C_", "I3C_")) {
  ref <- "I0C_"
} else if (GCMS %in% c("G0C_", "G2C_", "G3C_")) {
  ref <- "G0C_"
}

## open the reference file
baseline <- read_csv(paste0("/data01/julien/projects/camaflood/OUT/global_", ref,
                           "nodam_trim/global_reference_return_outflw",
                           "_Lmoment_gumbel_", indice, "_v1.csv"),
                     col_types = cols(L = col_integer(),
                                      return_30 = col_double(),
                                      return_100 = col_double(),
                                      stat  = col_double(),
                                      para1 = col_double(),
                                      para2 = col_double(),
                                      para3 = col_double(),
                                      para4 = col_double())
                     )

## mask
mask <- read_csv("/data01/julien/projects/camaflood/OUT/up_middle_downstream.csv",
                 col_types = cols(lon = col_double(),
                                  lat = col_double(),
                                  dam = col_integer(),
                                  nx  = col_integer(),
                                  ny  = col_integer(),
                                  L   = col_integer(),
                                  jx  = col_integer(),
                                  jy  = col_integer(),
                                  position = col_integer())
                 )
# remove the sea, not need to use the black listed
mask <- mask %>% filter(is.na(position) != TRUE)

# Koppen Geiger
koppen <- read_table2("/data01/julien/projects/camaflood/OUT/Koeppen-Geiger-ASCII.txt",
                      col_types = cols(Lat = col_double(),
                                       Lon = col_double(),
                                       Cls = col_character()))
# fix artifact in the dataset
koppen$Cls[koppen$Lat == 20.75 & koppen$Lon == 17.25] <- "BWh"
# remove "BWh" and "EF"
# add L

mask <- mask %>% left_join(koppen, by = c("lon" = "Lon","lat" =  "Lat"))
mask$y30 <- baseline$return_30

for (years in seq(YEAR_STA, YEAR_END, 1)) {
  File_in <- file(description = paste("/data01/julien/models/camaflood/out/global_", GCMS, EXP, "/outflw", years , ".bin", sep = ""), open = "rb")

  d <- seq(as.Date(paste(years, "/1/1", sep = "")), as.Date(paste(years, "/12/31", sep= "")), "day")
  yearly_matrix <- matrix(data = 0, nrow = length(d), ncol = nrow(mask))

  for (nb_days in 1:length(d)) {
    t  <- readBin(con = File_in, what = "numeric", n = 259200, size = 4, endian = "little")
    t <- ifelse(t == t[1], NA, t)
    yearly_matrix[nb_days,] <- t[mask$L]
  }
  close(File_in)

  # Compare to baseline and count nb of excedence
  nb_flood <- sapply(1:ncol(yearly_matrix), function(i) {sum(yearly_matrix[,i] >= baseline$return_100[i])})
  mask$fld <- nb_flood
  mask$yn <- ifelse(mask$fld > 0, 1, 0)

  # filter koppen region + very low flow
  temp <- mask %>% filter(!(Cls %in% c("BWk", "BWh", "EF") & y30 <= 5)) %>%
    group_by(position, yn) %>%
    summarise(n = n(),
             m = sum(fld, na.rm = TRUE))
  temp$year <- years ; temp$gcm <- GCMS ; temp$exp <- EXP
  if (years == YEAR_STA) {
    out <- temp
  } else {
    out <- rbind(out, temp)
  }
}

# write output
write_csv(out, paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/flood_occurence_yearly.csv"))
