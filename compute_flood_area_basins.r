#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)
## Purpose:
# Extract maximum flooded area separatly for major basins and for downstream, middle stream and upstream
# updated 07/08/2019
# 
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

#GCMS     <- "G3C_"
VARS     <- "fldfrc"
#EXP      <- "dam_trim"
YEAR_STA <- 2006
YEAR_END <- 2099
baseline <- FALSE
########### remove files
list_basins <- c(5, 113, 23, 74, 49, 24, 181, 57, 229, 11, 26, 76, 10, 68)


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

## for overflow correction
#overflowed_cell <- read.table("./../../DAT/overflow_filter_depth_10.csv")
#overflowed_cell <- as.matrix(overflowed_cell)


##### JOB #####
#Case  1
if (baseline == TRUE) {
  # Establish baseline floodfraction globally
  count <- 0
  complete_serie <- data.frame(years = rep(seq(YEAR_STA, YEAR_END, 1), each = 4 * length(list_basins)),
                               basin = rep(rep(c(5, 113, 23, 74, 49, 24, 181, 57, 229, 11, 26, 76, 10, 68), each = 4), times = length(seq(YEAR_STA, YEAR_END, 1))),
                               position = rep(seq(1, 4, 1), times =  length(seq(YEAR_STA, YEAR_END, 1))* length(list_basins)),
                               n    = 0,
                               fld1 = 0,
                               fld2 = 0
                               )
  for (years in seq(YEAR_STA, YEAR_END, 1)) {

    print(years)

    File_in <- file(description = paste("/data4/julien2/camaflood/out/global_", GCMS, EXP, "/", VARS, years , ".bin", sep = ""), open = "rb")  # flood frac
    d <- seq(as.Date(paste(years, "/1/1", sep = "")), as.Date(paste(years, "/12/31", sep= "")), "day")
    yearly_matrix <- matrix(data = 0, nrow = length(d), ncol = nrow(mask))

      for (nb_days in 1:length(d)) {
        months <- stringr::str_pad(lubridate::month(d[nb_days]), 2, side = "left", pad = "0") 
        t  <- readBin(con = File_in, 
                      what = "numeric", 
                      n = 259200,          # 360 * 720
                      size = 4,            # size?
                      endian = "little")   # here size is super important
        t <- ifelse(t == t[1], NA, t)
        yearly_matrix[nb_days,] <- t[mask$L]

        # here ultimatly I will have to open two files
        # > keep upstream + other from the GRdy no dam
        # > keep downstream + middle for the NRCD no dam (that is a special simulation that force H08 discharge eveywhere)
        
      } # daily loop
    close(File_in)

    # compute 7ave annual
    tt <- apply(yearly_matrix, 2, function(x) zoo::rollsum(x, 7, align = "left", fill = NA))
    ## need to summarise for basins / position relative to dams
    # Here I could limit the output by focusing on some basins
    for (basin_id in c(5, 4, 28, 3, 7, 22, 60, 17, 24, 23)) {
      # there are 4 cases
      for (positions in seq(1, 4)) {
        count <- count + 1

        if (length(which(mask$basin == basin_id & mask$position == positions)) == 1) {
          max_flood <- max(tt[, which(mask$basin == basin_id & mask$position == positions)], na.rm = TRUE)
        } else if (length(which(mask$basin == basin_id & mask$position == positions)) == 0) {
          max_flood <- NA
          } else {
          max_flood <- max(apply(tt[, which(mask$basin == basin_id & mask$position == positions)], 2, function(x) mean(x, na.rm = TRUE)), na.rm = TRUE)  
        }
        complete_serie[count,] <- c(basin_id, years, max_flood, positions)
        
      } # position loop
    }   # basin_id loop
    ## done one year, then just repeat and fill the matrix
  } # year loop
  
} else {
  print("No baseline")
  complete_serie <- data.frame(years = rep(seq(YEAR_STA, YEAR_END, 1), each = 4 * length(list_basins)),
                               basin = rep(rep(list_basins, each = 4), times = length(seq(YEAR_STA, YEAR_END, 1))),
                               position = rep(seq(1, 4, 1), times =  length(seq(YEAR_STA, YEAR_END, 1))* length(list_basins)),
                               n  = 0,
                               n1 = 0,
                               n2 = 0,
                               n3 = 0,
                               fld1 = 0,
                               fld2 = 0
                               #qua_flow_derived = 0,
                               #hyb_flow_derived = 0
                               )
  for (years in seq(YEAR_STA, YEAR_END, 1)) {

    print(years)
    File_flw <- file(description = paste("/data01/julien/models/camaflood/out/global_", GCMS, EXP, "/", "outflw", years , ".bin", sep = ""), open = "rb")
    File_frc <- file(description = paste("/data01/julien/models/camaflood/out/global_", GCMS, EXP, "/fldare", years , ".bin", sep = ""), open = "rb")
    
    d <- seq(as.Date(paste(years, "/1/1", sep = "")), as.Date(paste(years, "/12/31", sep= "")), "day")
    yearly_flw <- matrix(data = 0, nrow = length(d), ncol = nrow(mask))
    yearly_frc <- matrix(data = 0, nrow = length(d), ncol = nrow(mask))

      for (nb_days in 1:length(d)) {
        # flow
        t  <- readBin(con = File_flw, what = "numeric", n = 259200, size = 4, endian = "little")
        t <- ifelse(t == t[1], NA, t)
        yearly_flw[nb_days,] <- t[mask$L]

        # fraction
        t  <- readBin(con = File_frc, what = "numeric", n = 259200, size = 4, endian = "little")
        t <- ifelse(t == t[1], NA, t)
        yearly_frc[nb_days,] <- t[mask$L]
      } # daily loop
    close(File_flw) ; close(File_frc)

    # quantile
    y1 <- apply(yearly_flw, 2, function(x) max(x, na.rm = TRUE))
    y2 <- apply(yearly_flw, 2, function(x) quantile(x, 0.95, na.rm = TRUE, type = 8))

    # higher than historical?
    #potential_flood_1 <- ifelse(y1 > mask$reference_max, 1, 0)
    potential_flood_1 <- sapply(1:ncol(yearly_flw), function(i) sum(yearly_flw[,i] >= mask$reference_max[i]))
    potential_flood_2 <- sapply(1:ncol(yearly_flw), function(i) sum(yearly_flw[,i] >= mask$reference_qua[i]))
    #potential_flood_2 <- ifelse(y2 > mask$reference_qua, 1, 0)
    #    here the tricky part: composite
    #    potential_flood_3 <- ifelse(y1 > reference$return_100, 1, 0)
    #    correct <- mask$L %in% overflowed_cell
    #    potential_flood_3[correct] <- ifelse(y2[correct] > reference$return_100[correct], 1, 0)

    max_day7 <- apply(yearly_frc, 2, function(x) max(zoo::rollmean(x, 7, align = "right", fill = NA), na.rm = TRUE))

    # Need to summarize the info for basin and position relative to dams
    #list_basins <- seq(1, 25, 1)
    list_basins <- c(5, 113, 23, 74, 49, 24, 181, 57, 229, 11, 26, 76, 10, 68)
    # the function
    for (bb in list_basins) {
      print(c(years, bb))
      temp_frame <- mask %>% filter(basin == bb)
      # add info
      temp_frame$max_flw <- y1[mask$basin == bb]
      temp_frame$qua_flw <- y2[mask$basin == bb]
      temp_frame$max_fld <- max_day7[mask$basin == bb]

      temp_frame$potential_1    <- potential_flood_1[mask$basin == bb]
      temp_frame$potential_2    <- potential_flood_2[mask$basin == bb]
      temp_frame$potential_fld1 <- ifelse(temp_frame$potential_1 >= 1, max_day7[mask$basin == bb], 0)
      temp_frame$potential_fld2 <- ifelse(temp_frame$potential_2 >= 1, max_day7[mask$basin == bb], 0)

      temp <- temp_frame %>% group_by(position) %>% summarise(n    = n(),
                                                              n1   = sum(potential_1, na.rm = TRUE),
                                                              n2   = sum(potential_2, na.rm = TRUE),
                                                              fld1 = sum(potential_fld1, na.rm = TRUE),
                                                              fld2 = sum(potential_fld2, na.rm = TRUE),
                                                              )
      # store_1
      complete_serie[complete_serie$basin == bb & complete_serie$years == years,]$n[temp$position]  <- temp$n
      complete_serie[complete_serie$basin == bb & complete_serie$years == years,]$n1[temp$position] <- temp$n1
      complete_serie[complete_serie$basin == bb & complete_serie$years == years,]$n2[temp$position] <- temp$n2
      complete_serie[complete_serie$basin == bb & complete_serie$years == years,]$fld1[temp$position] <- temp$fld1
      complete_serie[complete_serie$basin == bb & complete_serie$years == years,]$fld2[temp$position] <- temp$fld2
      
    }
  } # year loop
  write_csv(complete_serie, paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/", "basins_flood_v1.csv", sep = ""))
  
} # main loop end
