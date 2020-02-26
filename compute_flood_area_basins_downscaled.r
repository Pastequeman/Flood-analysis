#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)
## Purpose:
# Extract maximum flooded area separatly for major basins and for downstream, middle stream and upstream
# uses the downscaled data wherenever possible

## libraries
library("tibble")
library("readr")
suppressMessages(library("dplyr"))

########### start inputs
if (length(args) != 2) {
  stop("error  > GCM | EXP")
} else {
  GCMS <- args[1]
  EXP  <- args[2]
}
YEAR_STA <- 2070
YEAR_END <- 2099

if (file.exists(paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/watershed.csv"))) {
  file.remove(paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/watershed.csv"))
}

list_basins <- c(5, 113, 23, 74, 49, 24, 181, 57, 229, 11, 26, 76, 10, 68)
regions <- c("af1", "as1", "as2", "as3", "ca1", "eu1", "eu2",
             "eu3", "na1", "na2", "oc1", "sa1", "si1", "si2")

########### reference file
if (GCMS == "M2C_" | GCMS == "M3C_" | GCMS == "M2C_GRdy" | GCMS == "M2C_NRCD" | GCMS == "M3C_GRdy" | GCMS == "M3C_NRCD") {
  GCM_ref <- "M0C_"
} else if (GCMS == "G2C_" | GCMS == "G3C_" | GCMS == "G2C_GRdy" | GCMS == "G2C_NRCD" | GCMS == "G3C_GRdy" | GCMS == "G3C_NRCD") {
  GCM_ref <- "G0C_"  
} else if (GCMS == "H2C_" | GCMS == "H3C_" | GCMS == "H2C_GRdy" | GCMS == "H2C_NRCD" | GCMS == "H3C_GRdy" | GCMS == "H3C_NRCD") {
  GCM_ref <- "H0C_"
} else if (GCMS == "I2C_" | GCMS == "I3C_" | GCMS == "I2C_GRdy" | GCMS == "I2C_NRCD" | GCMS == "I3C_GRdy" | GCMS == "I3C_NRCD") {
  GCM_ref <- "I0C_"
}

reference1 <- read_csv(paste("/data01/julien/projects/camaflood/OUT/global_", GCM_ref,
                            "nodam_trim/global_reference_return_outflw_Lmoment_gumbel_max_v1.csv", sep = ""),
                      col_types = cols(L = col_integer(),
                                       return_30 = col_double(),
                                       return_100 = col_double(),
                                       stat = col_double(),
                                       para1 = col_double(),
                                       para2 = col_double())
                      )
reference2 <- read_csv(paste("/data01/julien/projects/camaflood/OUT/global_", GCM_ref,
                            "nodam_trim/global_reference_return_outflw_Lmoment_gumbel_quantile_v1.csv", sep = ""),
                      col_types = cols(L = col_integer(),
                                       return_30 = col_double(),
                                       return_100 = col_double(),
                                       stat = col_double(),
                                       para1 = col_double(),
                                       para2 = col_double())
                      )

########### mask
mask <- read_csv("/data01/julien/projects/camaflood/OUT/up_middle_downstream.csv", col_types = cols(lon = col_double(),
                                                                                   lat = col_double(),
                                                                                   dam = col_integer(),
                                                                                   nx = col_integer(),
                                                                                   ny = col_integer(),
                                                                                   L = col_integer(),
                                                                                   jx = col_integer(),
                                                                                   jy = col_integer(),
                                                                                   position = col_integer())
                 )
# remove the sea
mask <- mask %>% filter(is.na(position) != TRUE)

########### basin file
to.read <-  file(description = paste("/data01/julien/models/camaflood/map/global_30min/basin.bin", sep = ""), open = "rb")
t <- readBin(con = to.read, what = "integer", n = 259200,  size = 4, endian = "little")
basin_global <- tibble(lat   = rep(seq(89.75, -89.75, by = -0.5), each = 720),
                       lon   = rep(seq(-179.75, 179.75, by = 0.5), times = 360),
                       basin = ifelse(t == t[1], NA, t)) #,
#clean
close(to.read) ; rm(to.read) ; rm(t) ; rm(GCM_ref)

mask <- mask %>% left_join(basin_global, by = c("lon", "lat"))
mask$reference_max <- reference1$return_100
mask$reference_qua <- reference2$return_100

rm(basin_global) ; rm(reference1) ; rm(reference2)

for (years in seq(YEAR_STA, YEAR_END, 1)) {
  #print(years)
  File_flw <- file(description = paste("/data01/julien/models/camaflood/out/global_", GCMS, EXP, "/", "outflw", years , ".bin", sep = ""), open = "rb")
  
  d <- seq(as.Date(paste(years, "/1/1", sep = "")), as.Date(paste(years, "/12/31", sep= "")), "day")
  yearly_flw <- matrix(data = 0, nrow = length(d), ncol = nrow(mask))
  for (nb_days in 1:length(d)) {
    # flow
    t  <- readBin(con = File_flw, what = "numeric", n = 259200, size = 4, endian = "little")
    t <- ifelse(t == t[1], NA, t)
    yearly_flw[nb_days,] <- t[mask$L]
  } # daily loop
  close(File_flw)
  
  # quantile
  y1 <- apply(yearly_flw, 2, function(x) max(x, na.rm = TRUE))
  y2 <- apply(yearly_flw, 2, function(x) quantile(x, 0.95, na.rm = TRUE, type = 8))
  
  # Higher than historical?
  potential_flood_1 <- sapply(1:ncol(yearly_flw), function(i) sum(yearly_flw[,i] >= mask$reference_max[i]))
  potential_flood_2 <- sapply(1:ncol(yearly_flw), function(i) sum(yearly_flw[,i] >= mask$reference_qua[i]))
  
  # The function
  for (bb in list_basins) {
    #print(c(years, bb))
    # relevant file and coordinates for the basin
    temp_frame <- mask %>% filter(basin == bb)
    temp_frame$potential_1    <- potential_flood_1[mask$basin == bb]
    temp_frame$potential_2    <- potential_flood_2[mask$basin == bb]
    # Add info
    temp_frame$max_flw <- y1[mask$basin == bb]
    temp_frame$qua_flw <- y2[mask$basin == bb]
    
    # Which file should we read?
    low_lon <- min(temp_frame$lon)
    hgt_lon <- max(temp_frame$lon)
    low_lat <- min(temp_frame$lat)
    hgt_lat <- max(temp_frame$lat)

    count <- 0
    for (region in regions) {
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
      if (((low_lon >= xstart & low_lon <= (xstart+xdef*0.005)) |
             (hgt_lon >= xstart & hgt_lon <= (xstart+xdef*0.005))) &
            ((hgt_lat <= ystart & hgt_lat >= (ystart-ydef*0.005)) |
               (low_lat <= ystart & low_lat >= (ystart-ydef*0.005)))
          ) {
        count <- count + 1
        # file to read
        fld_file <- file(paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/fld_", region, "_", years, ".flood"), open = "rb")
        da <- readBin(fld_file, what = "numeric", n = xdef*ydef, size = 4, endian = "little")
        close(fld_file) ; rm(fld_file)
        
        # basin in the regions
        basins <- file(paste0("/data01/julien/projects/camaflood/OUT/", region, "_basins_0.005deg.bin"), open = "rb")
        basin_list <- readBin(basins, what = "integer", n = xdef*ydef, size = 4, endian = "little")
        close(basins) ; rm(basins)

        # relation to dam position
        dams <- file(paste0("/data01/julien/projects/camaflood/OUT/", region, "_position_0.005deg.bin"), open = "rb")
        dam_list <- readBin(dams, what = "integer", n = xdef*ydef, size = 4, endian = "little")
        close(dams) ; rm(dams)
        
        # L id of the sub-cells
        l_file <- file(paste0("/data01/julien/projects/camaflood/OUT/", region, "_L_0.005deg.bin"), open = "rb")
        l_list <- readBin(l_file, what = "integer", n = xdef*ydef, size = 4, endian = "little")
        close(l_file) ; rm(l_file)

        if (count == 1) {
          # save relevent info
          sub_fld <- da[basin_list == bb]
          sub_l   <- l_list[basin_list == bb]
          sub_dam <- dam_list[basin_list == bb]
        } else {
          # append relevant info
          sub_fld <- append(sub_fld, da[basin_list == bb])
          sub_l   <- append(sub_l, l_list[basin_list == bb])
          sub_dam <- append(sub_dam, dam_list[basin_list == bb])
        }
      } # if region included
    }   # region loop

    # original 0.5 cell where flood occured
    flood <- tibble(position = seq(1, 4),
                    n_freq_max     = 0,
                    n_freq_qua     = 0,
                    freq_max       = 0,
                    freq_qua       = 0,
                    n_int_max       = 0,
                    n_int_qua       = 0,
                    frc_int_max     = 0,
                    frc_int_qua     = 0
                    )
    for (i in seq(1, 4)) {
      temp   <- sub_fld[sub_l %in% temp_frame[temp_frame$potential_1 >= 1,]$L & sub_dam == i]
      if (length(temp) == 0) {
        flood[flood$position == i,]$n_int_max <- 0
        flood[flood$position == i,]$frc_int_max <- 0
      } else {
        flood[flood$position == i,]$n_int_max <- length(temp[temp != -9999])
        flood[flood$position == i,]$frc_int_max <- sum(temp[temp != -9999]) / length(temp[temp != -9999])        
      }
      
      temp   <- sub_fld[sub_l %in% temp_frame[temp_frame$potential_2 >= 1,]$L & sub_dam == i]
      if (length(temp) == 0) {
        flood[flood$position == i,]$n_int_qua   <- 0
        flood[flood$position == i,]$frc_int_qua <- 0
      } else {
        flood[flood$position == i,]$n_int_qua <- length(temp[temp != -9999])
        flood[flood$position == i,]$frc_int_qua <- sum(temp[temp != -9999]) / length(temp[temp != -9999])        
      }

      temp <- temp_frame[temp_frame$position == i & temp_frame$potential_1 >= 1,]
      if (length(temp) == 0) {
        flood[flood$position == i,]$n_freq_max <- 0
        flood[flood$position == i,]$freq_max <- 0
      } else {
        flood[flood$position == i,]$n_freq_max <- length(temp$L)
        flood[flood$position == i,]$freq_max   <- sum(temp$potential_1)
      }

      temp <- temp_frame[temp_frame$position == i & temp_frame$potential_2 >= 1,]
      if (length(temp) == 0) {
        flood[flood$position == i,]$n_freq_qua <- 0
        flood[flood$position == i,]$freq_qua <- 0
      } else {
        flood[flood$position == i,]$n_freq_qua <- length(temp$L)
        flood[flood$position == i,]$freq_qua   <- sum(temp$potential_2)
      }      
    }
    # done with the sub-cells and 0.5 cells
    flood$years <- years
    flood$basin <- bb

    write_csv(flood, paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/watershed.csv"), append = TRUE)
  }     # basin
}       # year
