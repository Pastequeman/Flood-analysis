#!/usr/bin/Rscript

## Purpose:
# for all dam locations, compute capacity over annual ave discharge
# this is used to latter identify run-of-the-river dams
# (low capacity over discharge)

## read inputs
# User defined
GCMS     <- "H2C_"
MODELS   <- "H08"
VARS     <- "outflw"
EXP      <- "dam"
YEAR_STA <- 2069 #2006 #2006
YEAR_END <- 2099 #2013 #2099

# dam capacity
File_max_cap <- file(description = paste("/data4/hanasaki/H08_20130501/map/dat/dam_cap_/GRanD_L_20000000.CaMa.hlf"), open = "rb")
max_cap <- readBin(con = File_max_cap,
                   what = "numeric",
                   n = 259200,
                   size = 4,
                   endian = "big")
close(File_max_cap)
max_cap <- ifelse(max_cap == max_cap[1], NA, max_cap)

# dam location
File_dam_loc <- file(description = paste("/data4/hanasaki/H08_20130501/map/dat/dam_num_/GRanD_L_20000000.CaMa.hlf"), open = "rb")
dam_loc <- readBin(con = File_dam_loc,
                   what = "numeric",
                   n = 259200,
                   size = 4,
                   endian = "big")
close(File_dam_loc)
dam_loc <- ifelse(dam_loc == dam_loc[1], NA, dam_loc)

# combined and clean
global_file <- data.frame(lon = rep(seq(-179.75, 179.75, by = 0.5), times = 360),
                          lat = rep(seq(89.75, -89.75, by = -0.5), each = 720),
                          nx = rep(seq(1, 720, 1), times = 360),
                          ny = rep(seq(1, 360, 1), each = 720),
                          L = seq(1, 259200, 1),
                          dam = dam_loc,
                          cap = max_cap
                          )
rm(dam_loc) ; rm(max_cap) ; rm(File_dam_loc) ; rm(File_max_cap)

# Compute annual average discharge
out <- matrix(0, nrow = nrow(global_file), ncol = (YEAR_END - YEAR_STA + 1))
count_y <- 0

for (years in seq(YEAR_STA, YEAR_END, 1)) {
  print(years)
  count_y <- count_y + 1
  File_in <- file(description = paste("/data4/julien2/camaflood/out/global_", GCMS, "nodam", "/", VARS, years , ".bin", sep = ""), open = "rb")
  d <- seq(as.Date(paste(years, "/1/1", sep = "")), as.Date(paste(years, "/12/31", sep= "")), "day")
  temp <- matrix(data = 0, ncol = 259200, nrow = length(d))
  count   <- 0
  for (nb_days in 1:length(d)) {
    count <- count + 1
    days      <- stringr::str_pad(lubridate::day(d[nb_days]), 2, side = "left", pad = "0")
    months    <- stringr::str_pad(lubridate::month(d[nb_days]), 2, side = "left", pad = "0")
    File_in   <- file(description = paste("/data4/hanasaki/H08_20130501/riv/out/riv_out_/", GCMS, "NRCD",years, months, days, ".hlf", sep = ""), open = "rb")
    
    t  <- readBin(con = File_in, 
                  what = "numeric", 
                  n = 259200,
                  size = 4,
                  endian = "big")
    
    t <- ifelse(t == t[1], NA, t)
    temp[count,] <- t
    close(File_in)
  }
  close(File_in)  
  # annual average
  out[,count_y] <- apply(temp, 2, function(x) sum(x, na.rm = TRUE))
}
# Annual average Q
Q <- apply(out, 1, function(x) mean(x, na.rm = TRUE))
global_file$Q <- Q * 60*60*24   # unit!
global_file$CovQ <-global_file$cap /global_file$Q
# Save the file
readr::write_csv(global_file, "/data4/julien2/OUT/C_over_Q_h08.csv")
