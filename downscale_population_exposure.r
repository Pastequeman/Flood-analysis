#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

## Purpose:
# every year > compute ave. nb of people exposed to flood

## libraries:
library("tibble")
library("readr")
suppressMessages(library("dplyr"))
library("ncdf4")
########### start inputs
if (length(args) != 3 ) {
  stop("error in cmd parameters > GCM=H2C/G2C_... | EXP=dam_trim/nodam_trim | country=yes/no")
} else {
  GCMS     <- args[1] #"I2C_"
  EXP      <- args[2] #"nodam_trim"
  COUNTRY  <- args[3] # yes or no
}

# other inputs
step <- 0.005

## clean
if (file.exists(paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/country_exposure.csv"))) {
  file.remove(paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/country_exposure.csv"))
}
if (COUNTRY == "yes") {
  if (file.exists(paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/position_exposure.csv"))) {
    file.remove(paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/position_exposure.csv"))}
}

########### years
if (GCMS == "H0C_" | GCMS == "M0C_" | GCMS == "G0C_" | GCMS == "I0C_") {
  YEAR_STA <- 1900
  YEAR_END <- 2005
} else {
  YEAR_STA <- 2006
  YEAR_END <- 2099
}

regions <- c("af1", "as1", "as2", "as3", "ca1", "eu1", "eu2",
             "eu3", "na1", "na2", "oc1", "sa1", "si1", "si2")

######## Main job
for (region in regions) {

  # Regions specific
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

  if (COUNTRY == "yes") {
    # Open the country file
    c_file <- file(paste0("/data01/julien/projects/camaflood/DAT/country/", region, "_0.005deg.bin"), open = "rb") # eplace OUT > DAT
    country <- readBin(con = c_file, what = "integer", n = xdef*ydef, size = 4, endian = "little")
    close(c_file) ; rm(c_file)
  }
  # check which of the initial 8 regions are concerned
  initial_reg <- tibble(reg = seq(1, 8),
                        lon = rep(c(-180, -90, 0, 90), times = 2),
                        lat = rep(c(90, 0), each = 4),
                        inc = 0)
  if (region != "si2") {
    for (i in 1:8) {
      initial_reg$inc[i] <- ifelse((sum(seq(xstart, by = 0.005, length.out = xdef) %in% seq(initial_reg$lon[i], by = 0.005, length.out = 18000)) > 0) & (sum(seq(ystart, by = -0.005, length.out = ydef) %in% seq(initial_reg$lat[i], by = -0.005, length.out = 18000)) >0 ), 1, 0)    
    }
  } else {
    for (i in 1:8) {
      initial_reg$inc[i] <- ifelse((sum(c(seq(xstart, 180, by = 0.005), seq(-180, -165, by = 0.005)) %in% seq(initial_reg$lon[i], by = 0.005, length.out = 18000)) > 0) & (sum(seq(ystart, by = -0.005, length.out = ydef) %in% seq(initial_reg$lat[i], by = -0.005, length.out = 18000)) >0 ), 1, 0)    
    }    
  }
  #print(initial_reg)
  #} ## DEBUG
  # get the population
  for (i in initial_reg[initial_reg$inc == 1,]$reg) {
    nc_data <- nc_open(paste0("/data01/julien/projects/camaflood/DAT/population/inter_", i,".nc"))
    population <- ncvar_get(nc_data, "var1")
    nc_close(nc_data)
    #population <- scan(paste0("/data01/julien/projects/camaflood/DAT/population/interpolated_reg_", i, ".asc"), quiet = TRUE)
    pop <- data.frame(lon = rep(seq((initial_reg[initial_reg$reg == i,]$lon + 0.0025), by = 0.005, length.out = 18000), times = 18000),
                      lat = rep(seq((initial_reg[initial_reg$reg == i,]$lat - 0.0025), by = -0.005, length.out = 18000), each = 18000),
                      dat = as.vector(population) )# no need to transpose here!
    if (region != "si2") {
      population_v2 <- pop %>% filter(lon > xstart & lon < (xstart+0.005*xdef), lat < ystart & lat > (ystart-0.005*ydef))      
    } else {
      if (i == 1) {
        population_v2 <- pop %>% filter(lon > -180 & lon < -165, lat < ystart & lat > (ystart-0.005*ydef))        
      } else {
        population_v2 <- pop %>% filter(lon > xstart & lon < 180, lat < ystart & lat > (ystart-0.005*ydef))
      }
    }


    if (initial_reg[initial_reg$inc == 1,]$reg[1] == i) {
      population_v3 <- population_v2
      rm(population) ; rm(pop)
    } else {
      population_v3 <- rbind(population_v3, population_v2)
      rm(population) ; rm(pop)
    }
  }
  rm(population_v2)
  #  if (region != "si2") {
  #print(dim(population_v3)) ; print(xdef*ydef) ## DEBUG
  population_v3 <- population_v3 %>% arrange(desc(lat), lon)

  
  # Open the position file
  pos_file <- file(paste0("/data01/julien/projects/camaflood/OUT/", region, "_position_0.005deg.bin"), open = "rb")
  pos <- readBin(pos_file, what = "integer", n = xdef*ydef, endian = "little")
  close(pos_file) ; rm(pos_file)

  for (years in seq(YEAR_STA, YEAR_END)) {
    
    # Downscaled water file
    file_water <- file(paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/fld_", region, "_", years, ".flood"), open = "rb")
    temp <- readBin(con = file_water, what = "numeric", n = xdef*ydef, size = 4, endian = "little")
    close(file_water) ; rm(file_water)
    
    # inference
    ex <- ifelse(temp > 0, population_v3$dat, 0)

    sum <- tibble(position = pos,
                  country = country,
                  exposure = ex)
    
    # Summarise > country
    country_ex        <- sum %>% filter(exposure != -9999) %>%
      group_by(country) %>% summarise(tot_ex = sum(exposure, na.rm = TRUE))
    country_ex$year   <- years
    country_ex$region <- region
    #print(region)
    # Summarise > position 
    position_ex        <- sum %>% filter(exposure != -9999) %>%
      group_by(position) %>% summarise(tot_ex = sum(exposure, na.rm = TRUE))
    position_ex$year   <- years
    position_ex$region <- region
    #print(sum(position_ex$tot_ex)/1e6) ## DEBUG

    if (years == YEAR_STA) {
      exposure_country  <- country_ex
      exposure_position <- position_ex
    } else {
      exposure_country  <- rbind(exposure_country,  country_ex)
      exposure_position <- rbind(exposure_position, position_ex)
    }

  } # year loop
  if (file.exists(paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/country_exposure.csv"))) {
    write_csv(exposure_country,  paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/country_exposure.csv"), append = TRUE)
    write_csv(exposure_position, paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/position_exposure.csv"), append = TRUE)      
  } else {
    write_csv(exposure_country,  paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/country_exposure.csv"))
    write_csv(exposure_position, paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/position_exposure.csv"))  
  }
}
# Write the final file
