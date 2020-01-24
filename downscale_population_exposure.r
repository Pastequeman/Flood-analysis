#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

# Purpose:
# every year > compute ave. nb of people exposed to flood

# libraries
library("tibble")
library("readr")
suppressMessages(library("dplyr"))


########### start inputs
if (length(args) != 4 ) {
  stop("error in cmd parameters > GCM | EXP | model")
} else {
  GCMS     <- args[1] #"I2C_"
  EXP      <- args[2] #"nodam_trim"
  MODEL    <- args[3] #"CAMA" # H08
  COUNTRY  <- args[4] # yes or no
}

# other inputs
step <- 0.005

###### Mask to retreive the country names
mask <- read_csv("/data01/julien/projects/camaflood/OUT/up_middle_downstream_v2.csv",
                 col_types = cols(lon = col_double(),
                                  lat = col_double(),
                                  dam = col_integer(),
                                  nx  = col_integer(),
                                  ny  = col_integer(),
                                  L   = col_integer(),
                                  jx  = col_integer(),
                                  jy  = col_integer(),
                                  position = col_integer(),
                                  country  = col_factor())
                 )



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
    start <- -85.000 ; ystart <- 15.000
  } else if (region == "si1") {    # region 13
    xdef <- 12000    ; ydef <- 7000
    xstart <- 55.000 ; ystart <- 80.000
  } else if (region == "si2") {    # region 14
    xdef <- 19000     ; ydef <- 5000
    xstart <- 100.000 ; ystart <- 75.000
  }
  
  # Open the relevant population file
  pop_file <- file(paste0("/data01/julien/projects/camaflood/OUT/", region, "_pop_0.005deg.bin"), open = "rb")
  pop <- readBin(pop_file, what = "numeric", n = xdef*ydef, endian = "little")
  close(pop_file) ; rm(pop_file)

  # Open the position file
  pos_file <- file(paste0("/data01/julien/projects/camaflood/OUT/", region, "_position_0.005deg.bin"), open = "rb")
  pos <- readBin(pos_file, what = "integer", n = xdef*ydef, endian = "little")
  close(pos_file) ; rm(pos_file)

  
  if (COUNTRY == "yes") {
    # Open the country file
    country_file <- file(paste0("/data01/julien/projects/camaflood/OUT/", region, "_country_code_0.005deg.bin"), open = "rb")
    country      <- readBin(country_file, what = "integer", size = 4, n = xdef*ydef, endian = "little")
    close(country_file) ; rm(country_file)
    country <- levels(mask$country)[country]
  }

  
  for (years in seq(YEAR_STA, YEAR_END)) {
    
    # downscaled water file
    file_water <- file(paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/fld_", region, "_", years, ".flood"), open = "rb")
    temp <- readBin(con = file_water, what = "numeric", n = xdef*ydef, size = 4, endian = "little")
    close(file_water) ; rm(file_water)
    
    # inference
    ex <- ifelse(temp > 0, pop, 0)

    sum <- tibble(position = pos,
                  country = country,
                  exposure = ex)
    
    # Summarise > country
    country_ex        <- sum %>% filter(exposure != -9999) %>%
      group_by(country) %>% summarise(tot_ex = sum(exposure, na.rm = TRUE))
    country_ex$year   <- years
    country_ex$region <- region

    # Summarise > position 
    position_ex        <- sum %>% filter(exposure != -9999) %>%
      group_by(position) %>% summarise(tot_ex = sum(exposure, na.rm = TRUE))
    position_ex$year   <- years
    position_ex$region <- region
    
    if (years == YEAR_STA & region == "af1") {
      exposure_country  <- country_ex
      exposure_position <- position_ex
    } else {
      exposure_country  <- rbind(exposure_country,  country_ex)
      exposure_position <- rbind(exposure_position, position_ex)
    }

  } # year loop
}
# Write the final file
write_csv(exposure_country,  paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/country_exposure.csv"))
write_csv(exposure_position, paste0("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP, "/position_exposure.csv"))  
