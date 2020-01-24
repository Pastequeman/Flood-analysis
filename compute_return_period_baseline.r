#!/usr/bin/Rscript

# Purpose:
# Create the file containing the expected value for 100y occurance

## Libraries
library("tibble")
library("readr")
suppressMessages(library("dplyr"))
library("ppcc")

## start inputs should be the baseline!
GCMS     <- "G0C"
MODELS   <- "CAMA"
VARS     <- "outflw"
EXP      <- "nodam_trim"
YEAR_STA <- 1975
YEAR_END <- 2005
indice   <- "quantile"  # max or quantile

## Mask
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
# remove the sea
mask <- mask %>% filter(is.na(position) != TRUE)


#### read and prepare
count <- 0
complete_serie <- matrix(data = 0, nrow = length(seq(YEAR_STA, YEAR_END, 1)), ncol = nrow(mask))

for (years in seq(YEAR_STA, YEAR_END, 1)) {
  count <- count + 1
  #print(years)
  if (MODELS == "CAMA") {
    File_in <- file(description = paste("/data01/julien/models/camaflood/out/global_", GCMS, "_", EXP, "/", VARS, years , ".bin", sep = ""), open = "rb")
    endianess <- "little"
  }
  
  d <- seq(as.Date(paste(years, "/1/1", sep = "")), as.Date(paste(years, "/12/31", sep= "")), "day")
  yearly_matrix <- matrix(data = 0, nrow = length(d), ncol = nrow(mask))
  
  for (nb_days in 1:length(d)) {
    if (MODELS == "H08") {
      months <- stringr::str_pad(lubridate::month(d[nb_days]), 2, side = "left", pad = "0")
      days <- stringr::str_pad(lubridate::day(d[nb_days]), 2, side = "left", pad = "0")
      File_in <- file(description = paste("/data01/julien/models/camaflood/Diss/", GCMS, years , months, days, ".hlf", sep = ""), open = "rb")
      endianess <- "big"
      t  <- readBin(con = File_in, 
                    what = "numeric", 
                    n = 259200,
                    size = 4,
                    endian = endianess)
      t <- ifelse(t == t[1], NA, t)
      yearly_matrix[nb_days,] <- t[mask$L]/1000 # unit
    } else {
      t  <- readBin(con = File_in, 
                    what = "numeric", 
                    n = 259200,            # 360 * 720
                    size = 4,              # size?
                    endian = endianess)    # here size is super important
      t <- ifelse(t == t[1], NA, t)
      yearly_matrix[nb_days,] <- t[mask$L] # unit
    }
    
    
    if (MODELS == "H08") { close(File_in) }
  } # daily loop
  if (MODELS == "CAMA") { close(File_in) }

  # now keep the max within each column of the matrix
  # max
  #complete_serie[count,] <- apply(yearly_matrix, 2, function(x) max(x, na.rm = TRUE))
  # quantile
  if (indice == "max") {
    complete_serie[count,] <- apply(yearly_matrix, 2, function(x) max(x, na.rm = TRUE))
  } else {
    complete_serie[count,] <- apply(yearly_matrix, 2, function(x) quantile(x, 0.95, na.rm = TRUE, type = 8))
  }

}  # all years processed (31)
rm(yearly_matrix)
########### POST PROCESS ########

## Fit gumbel using Lmoment method
# using function
#fit_function <- function(x) {if (sum(is.na(x)) > 16) {NA} else if (abs(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) < 0.2) {NA} else {lmomco::lmr2par(x, type ="gum")}}
#fit     <- apply(complete_serie, 2, fit_function)

# manually
sorted_serie <- apply(complete_serie, 2, function(x) sort(x, na.last = TRUE))
M1 <- apply(sorted_serie, 2, function(x) mean(x, na.rm = TRUE))
M2 <- apply(sorted_serie, 2, function(x) 1 / 31 * sum((seq(1, 31, 1) - 1) / (31 - 1) * x))
L1 <- M1
L2 <- 2*M2-M1

alpha   <- L2/log(2)
epsilon <- L1-alpha*0.57721

## Fit y values
X <- matrix(data = rep(c(30, 100), nrow(mask)), nrow = nrow(mask), ncol = 2, byrow = TRUE)

# parameters 
#list_function <- function(x) { if (is.list(x)) {if (is.null(x)) {c(0,0)} else {x$para}} else {c(0,0)}}
#all_parameters <- do.call(rbind, lapply(fit, list_function))
all_parameters <- data.frame(epsilon, alpha)

## Compute the expected discharge
Y <- all_parameters[,1] - all_parameters[,2] * log(-log(1 - 1/X))
Y <- ifelse(Y > 0, Y, NA)

## Stats
stat_function <- function(x) { if (sum(is.na(x)) > 16) {NA} else if (abs(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) < 0.2) {NA} else {ppccTest(x, qfn = "qgumbel", mc = 100)}}
stat_test <- apply(complete_serie, 2, stat_function)

list_func <- function(x) { if (is.list(x)) {if (is.null(x)) {0} else {x$statistic}} else {0}}
stat_test <- do.call(rbind, lapply(stat_test, list_func))

## add resampling
max_it <- 100
boot_1 <- matrix(data = 0, nrow = max_it, ncol = nrow(all_parameters))
boot_2 <- matrix(data = 0, nrow = max_it, ncol = nrow(all_parameters))
boot_para1 <- matrix(data = 0, nrow = max_it, ncol = nrow(all_parameters))
boot_para2 <- matrix(data = 0, nrow = max_it, ncol = nrow(all_parameters))
for (j in 1:max_it) {
  #print(j)
  ss   <- sample(seq(1,31,1), 20, replace = FALSE)

  # manual
  sorted_serie <- apply(complete_serie[ss,], 2, function(x) sort(x))
  M1 <- apply(sorted_serie, 2, function(x) mean(x, na.rm = TRUE))
  M2 <- apply(sorted_serie, 2, function(x) 1 / 20 * sum((seq(1, 20, 1) - 1) / (20 - 1) * x))
  L1 <- M1
  L2 <- 2*M2-M1

  alpha   <- L2/log(2)
  epsilon <- L1-alpha*0.57721

  # 30 and 100 values ?
  Y100 <- epsilon - alpha * log(-log(1 - 1/100))
  Y30  <- epsilon - alpha * log(-log(1 - 1/30))
  all_parameters2 <- data.frame(Y30, Y100)
  # package
  #fit2 <- apply(complete_serie[ss,], 2, fit_function)
  #all_parameters2 <- do.call(rbind, lapply(fit2, list_function))
  
  #Y <- all_parameters2[,1] - all_parameters2[,2] * log(-log(1 - 1/X))
  #boot[j,] <- Y[,100]
  boot_1[j,] <- all_parameters2[,1]
  boot_2[j,] <- all_parameters2[,2]

  boot_para1[j, ] <- alpha
  boot_para2[j, ] <- epsilon
}
a <- apply(boot_1, 2, function(x) quantile(x, 0.5, na.rm = TRUE, type = 8))
b <- apply(boot_2, 2, function(x) quantile(x, 0.5, na.rm = TRUE, type = 8))


## Write outputs
out <- data.frame(L = mask$L,
                  return_30  = Y[, 1],
                  return_100 = Y[, 2],
                  stat = unname(stat_test),
                  para1 = all_parameters[,1],
                  para2 = all_parameters[,2],
                  para3 = a,
                  para4 = b)

## Save
if (MODELS == "CAMA") {
  write_csv(out, paste("/data01/julien/projects/camaflood/OUT/global_",GCMS, "_", EXP, "/global_reference_return_", VARS, "_Lmoment_gumbel_", indice, "_v1.csv", sep = ""))

  file_out <- file(paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, "_", EXP, "/global_gev_para1_", VARS, "_Lmoment_", indice, "_v1.bin", sep = ""), open = "wb")
  for (j in 1:max_it) {
    writeBin(boot_para1[j,], con = file_out, endian = "little")
  }
  close(file_out)

  file_out <- file(paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, "_", EXP, "/global_gev_para2_", VARS, "_Lmoment_", indice, "_v1.bin", sep = ""), open = "wb")
  for (j in 1:max_it) {
    writeBin(boot_para2[j,], con = file_out, endian = "little")
  }
  close(file_out)
} else {
  write_csv(out, paste("/data01/julien/projects/camaflood/OUT/global_",GCMS, "_",EXP, "/global_reference_return_", VARS, "_Lmoment_gumbel_qua_", indice, "_h08_v1.csv", sep = ""))
}
