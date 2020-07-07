#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

# Purpose:
# every year > compute ave. nb of people exposed to flood
# using the reference, the grid cell is considered if discharge is higher than the historical 100y flood
# taking advantage of the bootstrap dataset!

# libraries
library("tibble")
library("readr")
suppressMessages(library("dplyr"))
library("ncdf4")

########### start inputs
if (length(args) != 3 ) {
  stop("error in cmd parameters > GCM | EXP | model")
} else {
  GCMS     <- args[1] #"I2C_"
  EXP      <- args[2] #"nodam_trim"
  MODEL    <- args[3] #"CAMA" # H08
  SSP      <- args[4] # no or ssp1 ... ssp5
}

########### mask
mask <- read_csv("/data01/julien/projects/camaflood/OUT/up_middle_downstream.csv",
                 col_types = cols(lon = col_double(),
                                  lat = col_double(),
                                  dam = col_integer(),
                                  nx = col_integer(),
                                  ny = col_integer(),
                                  L = col_integer(),
                                  jx = col_integer(),
                                  jy = col_integer(),
                                  position = col_integer())
                 )

final_mask <- read_csv("/data01/julien/projects/camaflood/OUT/mask_final.csv",
                       col_types = cols(lon    = col_double(),
                                        lat    = col_double(),
                                        L      = col_integer(),
                                        rank   = col_factor(),
                                        Cls    = col_character(),
                                        class1 = col_factor())
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

# remove BWh and EF. latter !!!
#mask <- mask %>% filter(!Cls %in% c("EF", "BWh"))

########### years
if (GCMS == "H0C_" | GCMS == "M0C_" | GCMS == "G0C_" | GCMS == "I0C_") {
  YEAR_STA <- 1861
  YEAR_END <- 2005
} else {
  YEAR_STA <- 2006
  YEAR_END <- 2099
}


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

if (MODEL == "CAMA") {
  # load the bootstrap dataset for the max and quantile method
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
} else {
  reference <- read_csv(paste("/data01/julien/projects/camaflood/OUT/global_", GCM_base,
                              "GRdy_nodam/global_reference_return_outflw_Lmoment_gumbel_qua_h08_v1.csv", sep = ""),
                        col_types = cols(L = col_integer(),
                                         return_30 = col_double(),
                                         return_100 = col_double(),
                                         stat = col_double(),
                                         para1 = col_double(),
                                         para2 = col_double(),
                                         para3 = col_double(),
                                         para4 = col_double())
                        )

}

########### population
population <- read_table("/data01/julien/projects/camaflood/OUT/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2005_30_min_v2.asc", col_names = "pop", col_types = cols(pop = col_double()))
population$pop <- ifelse(population$pop == population$pop[1], NA, population$pop)

# for 2005 population
population$lon <- rep(seq(-179.75, 179.75, by = 0.5), times = 360)
population$lat <- rev(rep(seq(-89.75, by = 0.5, length.out = 360), each = 720))
#

######## the main job
# population exposed to flood (sum)
complete_serie_pop_position <- data.frame(years    = rep(rep(seq(YEAR_STA, YEAR_END, 1), each = 4), each = 100),
                                          ite      = rep(rep(seq(1, 100), times = length(seq(YEAR_STA, YEAR_END, 1))), each = 4),
                                          position = rep(seq(1, 4), times = 100 * length(seq(YEAR_STA, YEAR_END, 1))),
                                          sum_max   = 0,
                                          sum_qua   = 0,
                                          sum_cmax  = 0,
                                          sum_cqua  = 0)
# time varying population
if (YEAR_STA == 2006) {
  pop_file <- "./../DAT/population/population_rcp26soc_0p5deg_annual_2006-2099.nc4"
  nc <- nc_open(pop_file)
}

count <- 1
for (years in seq(YEAR_STA, YEAR_END, 1)) {
  print(years)

  # take year
  id <- which(seq(YEAR_STA, YEAR_END) == years)
  pop_yrs <- ncvar_get(nc, "number_of_people", start = c(1, 1, id), count = c(-1, -1, 1))
  population$pop2  <- as.vector((pop_yrs))
  #print(c(years)) ## DEBUG
  if (MODEL == "CAMA") {
    File_in1 <- file(description = paste("/data01/julien/models/camaflood/out/global_", GCMS, EXP, "/outflw", years , ".bin", sep = ""), open = "rb")
  }
  
  File_in2 <- file(description = paste("/data01/julien/models/camaflood/out/global_", GCMS, EXP, "/fldfrc", years , ".bin", sep = ""), open = "rb")
  d <- seq(as.Date(paste(years, "/1/1", sep = "")), as.Date(paste(years, "/12/31", sep= "")), "day")

  yearly_flw <- matrix(data = 0, nrow = length(d), ncol = nrow(mask))
  yearly_frc <- matrix(data = 0, nrow = length(d), ncol = nrow(mask))  
  for (nb_days in 1:length(d)) {
    if (MODEL == "H08") {
      # day and month
      months <- stringr::str_pad(lubridate::month(d[nb_days]), 2, side = "left", pad = "0")
      days   <- stringr::str_pad(lubridate::day(d[nb_days]), 2, side = "left", pad = "0")
      File_in1 <- file(description = paste("/data01/julien/models/camaflood/Diss/H08__", GCMS, years, months, days, ".hlf", sep = ""), open = "rb")
      t  <- readBin(con = File_in1, what = "numeric", n = 259200, size = 4, endian = "little")
      t <- ifelse(t == t[1], NA, t / 1000)
    } else {
      t  <- readBin(con = File_in1, what = "numeric", n = 259200, size = 4, endian = "little")
      t <- ifelse(t == t[1], NA, t)
    }

    yearly_flw[nb_days,] <- t[mask$L]

    # frc
    t  <- readBin(con = File_in2, what = "numeric", n = 259200, size = 4, endian = "little")
    t <- ifelse(t == t[1], NA, t)
    yearly_frc[nb_days,] <- t[mask$L]
    if (MODEL == "H08") { close(File_in1) }
  }
  if (MODEL == "CAMA") { close(File_in1) }
  close(File_in2)

  # flood indices
  y1 <- apply(yearly_flw, 2, function(x) max(x, na.rm = TRUE))
  y2 <- apply(yearly_flw, 2, function(x) quantile(x, 0.95, na.rm = TRUE, type = 8))



  potential_flood_max <- matrix(0, nrow = 100, ncol = nrow(mask))
  potential_flood_qua <- matrix(0, nrow = 100, ncol = nrow(mask))

  count_flood_max <- matrix(0, nrow = 100, ncol = nrow(mask))
  count_flood_qua <- matrix(0, nrow = 100, ncol = nrow(mask))
  # since I use the MC from historical every case need to be assessed
  for (i in 1:100) {
    potential_flood_max[i, ] <- as.numeric(y1 > reference_matrix_max[i, ])
    potential_flood_qua[i, ] <- as.numeric(y2 > reference_matrix_qua[i, ])

    count_flood_max[i, ] <- sapply(1:nrow(mask), function(j) sum(yearly_flw[,j] >= reference_matrix_max[i,j], na.rm = TRUE))
    count_flood_qua[i, ] <- sapply(1:nrow(mask), function(j) sum(yearly_flw[,j] >= reference_matrix_qua[i,j], na.rm = TRUE))
  }

  
  # Keep flood fraction for these cell, max? 7day ave?
  #day7 <- apply(yearly_frc, 2, function(x) quantile(zoo::rollmean(x, 7, align = "right", fill = NA), 0.95, na.rm = TRUE, type = 8))
  day7 <- apply(yearly_frc, 2, function(x) max(x, na.rm = TRUE) - quantile(x, 0.25, na.rm = TRUE))

  # Population exposed for all cases
  flood_frac <- tibble(lon = mask$lon,
                       lat = mask$lat,
                       position = mask$position,
                       fld_frc = day7)

  flood_frac <- flood_frac %>% left_join(population, by = c("lon", "lat"))

  # I need 100 * 4 data for the max and qua cases
  ## first multiply the matrix by the population and the fraction
  for (i in 1:100) {
    #print(i) ## DEBUG
    potential_flood_max[i, ] <- potential_flood_max[i,] * flood_frac$pop2 * day7
    potential_flood_qua[i, ] <- potential_flood_qua[i,] * flood_frac$pop2 * day7
  }

  ## 
  max_fld <- apply(potential_flood_max, 2, function(x) mean(x, na.rm = TRUE))
  qua_fld <- apply(potential_flood_qua, 2, function(x) mean(x, na.rm = TRUE))
  # add the mask
  out <- mask
  out$max_fld <- max_fld
  out$qua_fld <- qua_fld

  # save yearly files
  # if folder does not exist, create it
  if (!dir.exists(paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, sep = ""))) {
    dir.create(paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, sep = ""))
  }
  write_csv(out, paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/yearly_", years, "_population.csv", sep = ""))
  
  ## Second summarize for the 4 positions
  for (i in 1:100) {

    flood_frac$max <- potential_flood_max[i, ]
    flood_frac$qua <- potential_flood_qua[i, ]

    flood_frac$cmax <- count_flood_max[i, ]
    flood_frac$cqua <- count_flood_qua[i, ]
    ## summarise
    temp <- flood_frac %>% group_by(position) %>% summarise(sum_max = sum(max, na.rm = TRUE),
                                                            sum_qua = sum(qua, na.rm = TRUE),
                                                            sum_cmax = sum(cmax, na.rm = TRUE),
                                                            sum_cqua = sum(cqua, na.rm = TRUE))

    ## save
    complete_serie_pop_position[count:(count+3), 4:7] <- temp[,-1]
    count <- count + 4
  }

} # year loop

nc_close(nc)

## write
write_csv(complete_serie_pop_position, paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/population_exposure_position_ts.csv", sep = ""))
