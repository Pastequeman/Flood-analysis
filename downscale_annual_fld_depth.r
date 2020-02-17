#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)
## Purpose:
# 1) check where flood occured during a year
#    historical 100-year discharge was exceeded
# 2) for these cell only create a map containing the annual maximum floodplain innundation depth 


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
  YEAR_END <- 2099
} else {
  YEAR_STA <- 1861
  YEAR_END <- 2005
}


########## start inputs



########### mask
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

mask <- mask %>% left_join(koppen, by = c("lon" = "Lon","lat" =  "Lat"))

########### reference file
if (GCMS == "M2C_" | GCMS == "M3C_" | GCMS == "M0C_") {
  GCM_base <- "M0C_"
} else if(GCMS == "G2C_" | GCMS == "G3C_" | GCMS == "G0C_") {
  GCM_base <- "G0C_"
} else if (GCMS == "H2C_" | GCMS == "H3C_" | GCMS == "H0C_") {
  GCM_base <- "H0C_"
} else if (GCMS == "I2C_" | GCMS == "I3C_" | GCMS == "I0C_") {
  GCM_base <- "I0C_"
}

## reference files
#reference <- read.csv(paste0("/data01/julien/projects/camaflood/OUT/global_", GCM_base, "nodam_trim/global_reference_return_outflw_Lmoment_gumbel_max_v1.csv"))
file_1_max <- file(paste("/data01/julien/projects/camaflood/OUT/global_", GCM_base,
                         "nodam_trim/global_gev_para1_outflw_Lmoment_max_v1.bin", sep = ""), open = "rb")
file_2_max <- file(paste("/data01/julien/projects/camaflood/OUT/global_", GCM_base,
                         "nodam_trim/global_gev_para2_outflw_Lmoment_max_v1.bin", sep = ""), open = "rb")

file_1_qua <- file(paste("/data01/julien/projects/camaflood/OUT/global_", GCM_base,
                         "nodam_trim/global_gev_para1_outflw_Lmoment_quantile_v1.bin", sep = ""), open = "rb")
file_2_qua <- file(paste("/data01/julien/projects/camaflood/OUT/global_", GCM_base,
                         "nodam_trim/global_gev_para2_outflw_Lmoment_quantile_v1.bin", sep = ""), open = "rb")

reference_matrix_max <- matrix(data = 0, nrow = 100, ncol = nrow(mask))
reference_matrix_qua <- matrix(data = 0, nrow = 100, ncol = nrow(mask))

# get all parameters
for (i in 1:100) {
  para_1_max <- readBin(file_1_max, what = "numeric", n = nrow(mask), endian = "little")
  para_2_max <- readBin(file_2_max, what = "numeric", n = nrow(mask), endian = "little")
  reference_matrix_max[i, ] <- ifelse(is.finite(para_2_max - para_1_max * log(-log(1 - 1/100))), ifelse(para_2_max - para_1_max * log(-log(1 - 1/100)) > 0, para_2_max - para_1_max * log(-log(1 - 1/100)), 0), 0)
  
  para_1_qua <- readBin(file_1_qua, what = "numeric", n = nrow(mask), endian = "little")
  para_2_qua <- readBin(file_2_qua, what = "numeric", n = nrow(mask), endian = "little")
  reference_matrix_qua[i, ] <- ifelse(is.finite(para_2_qua - para_1_qua * log(-log(1 - 1/100))), ifelse(para_2_qua - para_1_qua * log(-log(1 - 1/100)) > 0, para_2_qua - para_1_qua * log(-log(1 - 1/100)), 0), 0)
}

# close and clean
close(file_1_max) ; close(file_2_max)  ; close(file_1_qua) ; close(file_2_qua)
rm(file_1_max) ; rm(file_2_max) ; rm(file_1_qua) ; rm(file_2_qua) ;
rm(i) ; rm(para_1_max) ; rm(para_1_qua) ; rm(para_2_max) ; rm(para_2_qua)


count <- 1
for (years in seq(YEAR_STA, YEAR_END, 1)) {
  #print(years)

  # Discharge file to open 
  File_in1 <- file(description = paste("/data01/julien/models/camaflood/out/global_", GCMS, EXP, "/outflw", years , ".bin", sep = ""), open = "rb")
  # Floodplain depth
  File_in2 <- file(description = paste("/data01/julien/models/camaflood/out/global_", GCMS, EXP, "/flddph", years , ".bin", sep = ""), open = "rb")

  d <- seq(as.Date(paste(years, "/1/1", sep = "")), as.Date(paste(years, "/12/31", sep= "")), "day")
  yearly_flw <- matrix(data = 0, nrow = length(d), ncol = nrow(mask))
  yearly_fld <- matrix(data = 0, nrow = length(d), ncol = nrow(mask))
  for (nb_days in 1:length(d)) {
    t <- readBin(con = File_in1, what = "numeric", n = 259200, size = 4, endian = "little")
    t <- ifelse(t == t[1], NA, t)
    yearly_flw[nb_days,] <- t[mask$L]

    t <- readBin(con = File_in2, what = "numeric", n = 259200, size = 4, endian = "little")
    t <- ifelse(t == t[1], NA, t)
    yearly_fld[nb_days,] <- t[mask$L]
  }
  close(File_in1) ; close(File_in2)

  # flood indices
  y1 <- apply(yearly_flw, 2, function(x) max(x, na.rm = TRUE))
  y2 <- apply(yearly_flw, 2, function(x) quantile(x, 0.95, na.rm = TRUE, type = 8))

  potential_flood_max <- matrix(0, nrow = 100, ncol = nrow(mask))
  potential_flood_qua <- matrix(0, nrow = 100, ncol = nrow(mask))

  for (i in 1:100) {
    potential_flood_max[i, ] <- as.numeric(y1 > reference_matrix_max[i, ])
    potential_flood_qua[i, ] <- as.numeric(y2 > reference_matrix_qua[i, ])
  }

  # I know where flood occured
  # for these grid cell take the floodplain depth
  # 0 elsewhere
  overall_nb_flood_v1 <- apply(potential_flood_max, 2, function(x) sum(x, na.rm = TRUE))
  overall_nb_flood_v2 <- apply(potential_flood_qua, 2, function(x) sum(x, na.rm = TRUE))
  # max water elev
  annual_max_fldd <- apply(yearly_fld, 2, function(x) max(x, na.rm = TRUE)) # here I do not remove a certain quantile since this is water depth

  #flooded_depth <- ifelse(y1 >= reference$return_100, annual_max_fldd, 0)
  flooded_depth <- ifelse((overall_nb_flood_v1 + overall_nb_flood_v2)/2 >= 50, annual_max_fldd, 0)
  mask$depth <- flooded_depth
  a <- mask %>% select(L, lon, lat, depth)
  temp <- data.frame(L   = seq(from = 1, to = 360*720, 1),
                   lon = rep(seq(-179.75, 179.75, 0.5), times = 360),
                   lat = rep(seq(89.75, -89.75, -0.5), each = 720)
                   )
  temp <- temp %>% left_join(a, by = c("L", "lon", "lat"))
  temp$depth <- ifelse(is.na(temp$depth), -9999, temp$depth)
  ## 3) write to file the final map
  File_out <- file(paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/to_downscale_", years, ".bin"), open = "wb")
  t  <- writeBin(temp$depth, con = File_out, size = 4, endian = "little")
  close(File_out)
  
}
