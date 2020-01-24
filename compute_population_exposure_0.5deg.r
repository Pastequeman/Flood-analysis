#!/usr/bin/Rscript

# Purpose:
# every year > compute ave. nb of people exposed to flood
# using the reference, the grid cell is considered if discharge is higher than the historical 100y flood

# libraries
library("tibble")
library("readr")
suppressMessages(library("dplyr"))

########### start inputs
GCMS     <- "G0C_GRdy_"
EXP      <- "nodam_rev"
MODEL    <- "CAMA" # H08
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

# remove BWh and EF
mask <- mask %>% filter(!Cls %in% c("EF", "BWh"))


## overflow correction
#overflowed_cell <- read.table("./../../DAT/overflow_filter_depth_10.csv")
#overflowed_cell <- as.matrix(overflowed_cell)


########### years
if (GCMS == "H0C_GRdy_" | GCMS == "M0C_GRdy_" | GCMS == "G0C_GRdy_" | GCMS == "I0C_GRdy_") {
  YEAR_STA <- 1861
  YEAR_END <- 2005
} else {
  YEAR_STA <- 2006
  YEAR_END <- 2099
}


########### reference file
if (GCMS == "M2C_" | GCMS == "M3C_" | GCMS == "M0C_GRdy_") {
  GCM_base <- "M0C_"
} else if(GCMS == "G2C_" | GCMS == "G3C_" | GCMS == "G0C_GRdy_") {
  GCM_base <- "G0C_"
} else if (GCMS == "H2C_" | GCMS == "H3C_" | GCMS == "H0C_GRdy_") {
  GCM_base <- "H0C_"
} else if (GCMS == "I2C_" | GCMS == "I3C_" | GCMS == "I0C_GRdy_") {
  GCM_base <- "I0C_"
}
if (MODEL == "CAMA") {
  reference1 <- read_csv(paste("/data01/julien/projects/camaflood//OUT/global_", GCM_base,
                              "GRdy_nodam_rev/global_reference_return_outflw_Lmoment_gumbel_max_v1.csv", sep = ""),
                        col_types = cols(L = col_integer(),
                                         return_30 = col_double(),
                                         return_100 = col_double(),
                                         stat = col_double(),
                                         para1 = col_double(),
                                         para2 = col_double(),
                                         para3 = col_double(),
                                         para4 = col_double())
                        )
  reference2 <- read_csv(paste("/data01/julien/projects/camaflood//OUT/global_", GCM_base,
                              "GRdy_nodam_rev/global_reference_return_outflw_Lmoment_gumbel_quantile_v1.csv", sep = ""),
                        col_types = cols(L = col_integer(),
                                         return_30 = col_double(),
                                         return_100 = col_double(),
                                         stat = col_double(),
                                         para1 = col_double(),
                                         para2 = col_double(),
                                         para3 = col_double(),
                                         para4 = col_double())
                        )
} else {
  reference <- read_csv(paste("/data01/julien/projects/camaflood//OUT/global_", GCM_base,
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

ref <-  data.frame(L = reference1$L, ref1 = reference1$para4, ref2 = reference2$para4)
mask <- mask %>% left_join(ref, by = "L")
# clean
rm(reference1) ; rm(reference2) ; rm(ref)

########### population
population <- read_table("/data01/julien/projects/camaflood/OUT/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2005_30_min_v2.asc", col_names = "pop", col_types = cols(pop = col_double()))
population$pop <- ifelse(population$pop == population$pop[1], NA, population$pop)
# 208,800

# from -180 / -60
#population$lon <- rep(seq(-179.75, 179.75, by = 0.5), times = 290)
#population$lat <- rev(rep(seq(-59.75, by = 0.5, length.out = 290), each = 720))
# for 2005 population
population$lon <- rep(seq(-179.75, 179.75, by = 0.5), times = 360)
population$lat <- rev(rep(seq(-89.75, by = 0.5, length.out = 360), each = 720))
#

######## the main job
count  <- 0
count2 <- -3
# population exposed to flood (sum)
complete_serie_pop <- data.frame(years  = seq(YEAR_STA, YEAR_END, 1),
                                 pop_ex1 = 0,
                                 pop_ex2 = 0,
                                 sum_ex1 = 0,
                                 sum_ex2 = 0)

complete_serie_pop_position <- data.frame(years  = rep(seq(YEAR_STA, YEAR_END, 1), each = 4),
                                          position = 0,
                                          pop_ex1 = 0,
                                          pop_ex2 = 0,
                                          sum_ex1 = 0,
                                          sum_ex2 = 0)

for (years in seq(YEAR_STA, YEAR_END, 1)) {
  count <- count + 1
  count2 <- count2 + 4
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

  # quantile
  y1 <- apply(yearly_flw, 2, function(x) max(x, na.rm = TRUE))
  y2 <- apply(yearly_flw, 2, function(x) quantile(x, 0.95, na.rm = TRUE, type = 8))


  # higher than historical?
  potential_flood_1 <- ifelse(y1 > mask$ref1, 1, 0)
  potential_flood_2 <- ifelse(y2 > mask$ref2, 1, 0)

  # also would like to count the nb of excedance
  y3 <- sapply(1:nrow(mask), function(i) sum(yearly_flw[,i] >= mask$ref1[i], na.rm = TRUE))
  y4 <- sapply(1:nrow(mask), function(i) sum(yearly_flw[,i] >= mask$ref2[i], na.rm = TRUE))
  
  #flood_count <- sum(potential_flood_2)
  #correct <- mask$L %in% overflowed_cell
  #potential_flood_3[correct] <- ifelse(y2[correct] > ref[correct], 1, 0)

  # keep flood fraction for these cell, max? 7day ave?
  day7 <- apply(yearly_frc, 2, function(x) quantile(zoo::rollmean(x, 7, align = "right", fill = NA), 0.95, na.rm = TRUE, type = 8))

  # population exposed
  flood_frac <- tibble(lon = mask$lon,
                       lat = mask$lat,
                       position = mask$position,
                       exp1 = day7 * potential_flood_1,
                       exp2 = day7 * potential_flood_2,
                       count1 = y3,
                       count2 = y4)

  flood_frac <- flood_frac %>% left_join(population, by = c("lon", "lat"))

  # summarise
  flood_frc_1 <- flood_frac %>% summarise(pop_ex1 = sum(pop * exp1, na.rm = TRUE),
                                          pop_ex2 = sum(pop * exp2, na.rm = TRUE),
                                          sum_ex1 = sum(count1, na.rm = TRUE) / nrow(mask),
                                          sum_ex2 = sum(count2, na.rm = TRUE) / nrow(mask))

  flood_frc_2 <- flood_frac %>% group_by(position) %>%
    summarise(pop_ex1 = sum(pop * exp1, na.rm = TRUE),
                                          pop_ex2 = sum(pop * exp2, na.rm = TRUE),
                                          sum_ex1 = sum(count1, na.rm = TRUE) / nrow(mask),
                                          sum_ex2 = sum(count2, na.rm = TRUE) / nrow(mask))
  
  # save
  complete_serie_pop[count, -1]          <- flood_frc_1
  complete_serie_pop_position[count2:(count2+3), -1] <- flood_frc_2
}

## Summarise and write
write_csv(complete_serie_pop, paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/population_exposure_ts.csv", sep = ""))
write_csv(complete_serie_pop_position, paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/population_exposure_position_ts.csv", sep = ""))
