#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)
## Purpose:
# compute the gev for a future period
# then using the expected value for 100 yrs look how often this value could be expected

## Libraries
library("tibble")
library("readr")
suppressMessages(library("dplyr"))
library("ppcc")

## read commanf line inputs
if (length(args) < 4) {
  stop("Required: GCM ; indice ; threshold ; bootstrap")
} else {
  GCMS      <- args[1] #"G2C_"
  indice    <- args[2] #"max"  # max or quantile
  threshold <- as.integer(args[3]) # 100  
  bootstrap <- as.integer(args[4]) #1000
}

########### start inputs

MODELS   <- "CAMA"
VARS     <- "outflw"
EXP      <- c("dam_trim", "nodam_trim")
YEAR_STA <- 2070
YEAR_END <- 2099

#print(c(GCMS, indice, threshold, bootstrap))

#if (threshold == 30) {
  
#} else {
# create dir
if (!dir.exists(file.path(paste("/data01/julien/projects/camaflood/OUT/thres", threshold,"/global_", GCMS, EXP[1], "/", sep = "")))) {
  dir.create(file.path(paste("/data01/julien/projects/camaflood/OUT/thres", threshold, "/global_", GCMS, EXP[1], "/", sep = "")))
}

if (!dir.exists(file.path(paste("/data01/julien/projects/camaflood/OUT/thres", threshold, "/global_", GCMS, EXP[2], "/", sep = "")))) {
  dir.create(file.path(paste("/data01/julien/projects/camaflood/OUT/thres", threshold, "/global_", GCMS, EXP[2], "/", sep = "")))
}
#}



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


########### reference file (carreful name!)
if (GCMS == "M2C_" | GCMS == "M3C_" | GCMS == "M2C_GRdy" | GCMS == "M2C_NRCD" | GCMS == "M3C_GRdy" | GCMS == "M3C_NRCD") {
  GCM_ref <- "M0C_"
} else if (GCMS == "G2C_" | GCMS == "G3C_" | GCMS == "G2C_GRdy" | GCMS == "G2C_NRCD" | GCMS == "G3C_GRdy" | GCMS == "G3C_NRCD") {
  GCM_ref <- "G0C_"  
} else if (GCMS == "H2C_" | GCMS == "H3C_" | GCMS == "H2C_GRdy" | GCMS == "H2C_NRCD" | GCMS == "H3C_GRdy" | GCMS == "H3C_NRCD") {
  GCM_ref <- "H0C_"
} else if (GCMS == "I2C_" | GCMS == "I3C_" | GCMS == "I2C_GRdy" | GCMS == "I2C_NRCD" | GCMS == "I3C_GRdy" | GCMS == "I3C_NRCD") {
  GCM_ref <- "I0C_"
}

# ow use the more robust estimate
if (MODELS == "CAMA") {
  baseline <- read_csv(paste("/data01/julien/projects/camaflood/OUT/global_", GCM_ref,
                             "nodam_trim/global_reference_return_", VARS,
                             "_Lmoment_gumbel_", indice, "_v1.csv", sep = ""),
                       col_types = cols(L = col_integer(),
                                        return_30 = col_double(),
                                        return_100 = col_double(),
                                        stat  = col_double(),
                                        para1 = col_double(),
                                        para2 = col_double(),
                                        para3 = col_double(),
                                        para4 = col_double())
                       )
} else {

  file_path <- paste("/data01/julien/projects/camaflood/OUT/global_", GCM_ref,
                     "nodam/global_reference_return_", VARS,
                     "_Lmoment_gumbel_", indice, "_h08_v1.csv", sep = "")
  
  baseline <- read_csv(file_path,
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

mask$reference <- baseline$para4  # make sure para4 is adequate!

# clean
rm(baseline) ; rm(GCM_ref)
# need to be order!!!!!
#reference <- reference %>% arrange(L)


complete_serie_dam   <- matrix(data = 0, nrow = length(seq(YEAR_STA, YEAR_END, 1)), ncol = nrow(mask))
complete_serie_nodam <- matrix(data = 0, nrow = length(seq(YEAR_STA, YEAR_END, 1)), ncol = nrow(mask))

for (SCE in EXP) {
  count <- 0
  for (years in seq(YEAR_STA, YEAR_END, 1)) {
    count <- count + 1
    #print(years)
    
    if (MODELS == "CAMA") {
      File_in <- file(description = paste("/data01/julien/models/camaflood/out/global_", GCMS, SCE, "/", VARS, years , ".bin", sep = ""), open = "rb")
      endianess <- "little"
    }
    d <- seq(as.Date(paste(years, "/1/1", sep = "")), as.Date(paste(years, "/12/31", sep= "")), "day")
    yearly_matrix <- matrix(data = 0, nrow = length(d), ncol = nrow(mask))
    
    for (nb_days in 1:length(d)) {
      if (MODELS == "H08") {
        months <- stringr::str_pad(lubridate::month(d[nb_days]), 2, side = "left", pad = "0") 
        days <- stringr::str_pad(lubridate::day(d[nb_days]), 2, side = "left", pad = "0")
        File_in <- file(description = paste("/data01/julien/models/camaflood/Diss/H08__", GCMS, years, months, days, "bytswp.hlf", sep = ""), open = "rb")
        endianess <- "little"
        t  <- readBin(con = File_in, what = "numeric", n = 259200, size = 4, endian = endianess)
        t <- ifelse(t == t[1], NA, t)
        yearly_matrix[nb_days,] <- t[mask$L]/1000
      } else {
        t  <- readBin(con = File_in, what = "numeric", n = 259200, size = 4, endian = "little")
        t <- ifelse(t == t[1], NA, t)
        yearly_matrix[nb_days,] <- t[mask$L]
      }
      if (MODELS == "H08") { close(File_in) }
    } # daily loop
    if (MODELS == "CAMA") { close(File_in) }
    
    # now keep the max within each column of the matrix
    #complete_serie[count,] <- apply(yearly_matrix, 2, function(x) {if (sum(is.na(x)) > 360 ) {NA} else {max(x, na.rm = TRUE)}})
    # could also take Q95
    if (indice == "max") {
      if (SCE == EXP[1]) {
        complete_serie_dam[count,] <- apply(yearly_matrix, 2, function(x) {if (sum(is.na(x)) > 360 ) {NA} else {max(x, na.rm = TRUE)}})
      } else {
        complete_serie_nodam[count,] <- apply(yearly_matrix, 2, function(x) {if (sum(is.na(x)) > 360 ) {NA} else {max(x, na.rm = TRUE)}})
      }
    } else {
      if (SCE == EXP[1]) {
        complete_serie_dam[count,] <- apply(yearly_matrix, 2, function(x) {if (sum(is.na(x)) > 360 ) {NA} else {quantile(x, 0.95, na.rm = TRUE, type = 8)}})
      } else {
        complete_serie_nodam[count,] <- apply(yearly_matrix, 2, function(x) {if (sum(is.na(x)) > 360 ) {NA} else {quantile(x, 0.95, na.rm = TRUE, type = 8)}}) 
      }
    }
  } # all years processed (31)
}

complete_serie_dam   <- ifelse(is.finite(complete_serie_dam) != TRUE, NA, complete_serie_dam)
complete_serie_nodam <- ifelse(is.finite(complete_serie_nodam) != TRUE, NA, complete_serie_nodam)

########## POST PROCESS ##########

# sort
sorted_dam   <- apply(complete_serie_dam,   2, function(x) sort(x, na.last = TRUE))
sorted_nodam <- apply(complete_serie_nodam, 2, function(x) sort(x, na.last = TRUE))

# which place should pool data toguether

test_result <- sapply(1:ncol(complete_serie_nodam), function(i) (sorted_dam[,i] - sorted_nodam[,i] ))
test_stat   <- apply(test_result, 2, function(x) {ifelse(abs(sum(x, na.rm = TRUE)) > threshold, FALSE, TRUE) })
# K-S test 
#test_result <- sapply(1:ncol(complete_serie_nodam), function(i) ks.test(sorted_dam[,i], sorted_nodam[,i]))
#diff_1 <- sapply(1:ncol(complete_serie_dam), function(i) (round(complete_serie_dam[,i]) - round(complete_serie_nodam[,i])))
#test_stat <- apply(diff_1, 2, function(x) {ifelse(sum(x == 0) > 10, 0, ifelse(sum(x[1] == x) > 10, 1, 1)) })
#test_stat <- numeric(ncol(complete_serie_nodam))
#for (i in 1:ncol(complete_serie_nodam)) {
#  test_stat[i] <- test_result[,i]$p.value
#}
#similar <- ifelse(test_stat >= 0.05, TRUE, FALSE)
similar <- test_stat #ifelse(test_stat == 0, TRUE, FALSE)
# add criteria for the max value
#similar <- ifelse(near(sorted_dam[31,], sorted_nodam[31,], tol = 5), TRUE, similar)

pooled_data <- matrix(data = 0, nrow = 2 * length(seq(YEAR_STA, YEAR_END, 1)), ncol = sum(similar))
pooled_data <- rbind(complete_serie_dam[, similar], complete_serie_nodam[, similar])

# now the reaining cells for both scenario
remain_dam   <- complete_serie_dam[,!similar]
remain_nodam <- complete_serie_nodam[,!similar]

# sort
sorted_pooled <- apply(pooled_data,  2, function(x) sort(x, na.last = TRUE))
sorted_dam    <- apply(remain_dam,   2, function(x) sort(x, na.last = TRUE))
sorted_nodam  <- apply(remain_nodam, 2, function(x) sort(x, na.last = TRUE))

# fit
M1_pooled <- apply(sorted_pooled, 2, function(x) mean(x, na.rm = TRUE))
M2_pooled <- apply(sorted_pooled, 2, function(x) 1 / 60 * sum((seq(1, 60, 1) - 1) / (60 - 1) * x))
L1_pooled <- M1_pooled
L2_pooled <- 2 * M2_pooled - M1_pooled

M1_dam <- apply(sorted_dam, 2, function(x) mean(x, na.rm = TRUE))
M2_dam <- apply(sorted_dam, 2, function(x) 1 / 30 * sum((seq(1, 30, 1) - 1) / (30 - 1) * x))
L1_dam <- M1_dam
L2_dam <- 2 * M2_dam - M1_dam

M1_nodam <- apply(sorted_nodam, 2, function(x) mean(x, na.rm = TRUE))
M2_nodam <- apply(sorted_nodam, 2, function(x) 1 / 30 * sum((seq(1, 30, 1) - 1) / (30 - 1) * x))
L1_nodam <- M1_nodam
L2_nodam <- 2 * M2_nodam - M1_nodam

# add derive the final parameters
alpha_pooled   <- L2_pooled / log(2)
epsilon_pooled <- L1_pooled - alpha_pooled * 0.57721

alpha_dam   <- L2_dam / log(2)
epsilon_dam <- L1_dam - alpha_dam * 0.57721

alpha_nodam   <- L2_nodam / log(2)
epsilon_nodam <- L1_nodam - alpha_nodam * 0.57721

# fit Y values
X <- matrix(data = rep(c(30, 100), nrow(mask)), nrow = nrow(mask), ncol = 2, byrow = TRUE)

# need to put back the two dataset together
id <- seq(1, 60589)
dam_para <- data.frame(id = c(id[similar], id[!similar]), epsilon = c(epsilon_pooled, epsilon_dam), alpha = c(alpha_pooled, alpha_dam))
nodam_para <- data.frame(id = c(id[similar], id[!similar]), epsilon = c(epsilon_pooled, epsilon_nodam), alpha = c(alpha_pooled, alpha_nodam))

dam_para   <- dam_para[order(dam_para$id),]
nodam_para <- nodam_para[order(nodam_para$id),]

# tests for resampling
# do it x times, save only the return periods for 30 and 100 years

boot_30y_nodam   <- matrix(data = 0, nrow = bootstrap, ncol = nrow(dam_para))
boot_30y_dam     <- matrix(data = 0, nrow = bootstrap, ncol = nrow(dam_para))
boot_100y_nodam  <- matrix(data = 0, nrow = bootstrap, ncol = nrow(dam_para))
boot_100y_dam    <- matrix(data = 0, nrow = bootstrap, ncol = nrow(dam_para))
boot_para1_nodam <- matrix(data = 0, nrow = bootstrap, ncol = nrow(dam_para))
boot_para1_dam   <- matrix(data = 0, nrow = bootstrap, ncol = nrow(dam_para))
boot_para2_nodam <- matrix(data = 0, nrow = bootstrap, ncol = nrow(dam_para))
boot_para2_dam   <- matrix(data = 0, nrow = bootstrap, ncol = nrow(dam_para))

for (j in 1:bootstrap) {
  print(j)
  # sample
  ss   <- sample(seq(1,30,1), 30, replace = TRUE) # what is the best ? before 20 and no replacment

  sorted_serie_dam   <- apply(complete_serie_dam[ss,], 2, function(x) sort(x, na.last = TRUE))
  sorted_serie_nodam <- apply(complete_serie_nodam[ss,], 2, function(x) sort(x, na.last = TRUE))

  #test_result <- sapply(1:ncol(sorted_serie_nodam), function(i) ks.test(sorted_serie_dam[,i], sorted_serie_nodam[ ,i]))
  #test_stat <- numeric(ncol(complete_serie_nodam))
  test_result <- sapply(1:ncol(sorted_serie_dam), function(i) (sorted_serie_dam[,i] - sorted_serie_nodam[,i] ))
  test_stat   <- apply(test_result, 2, function(x) {ifelse(abs(sum(x, na.rm = TRUE)) > threshold, FALSE, TRUE) })
  #diff_1 <- sapply(1:ncol(complete_serie_dam), function(i) (round(complete_serie_dam[,i]) - round(complete_serie_nodam[,i])))
  #test_stat <- apply(diff_1, 2, function(x) {ifelse(sum(x == 0) > 10, 0, ifelse(sum(x[1] == x) > 10, 1, 1)) })
  #for (i in 1:ncol(sorted_serie_dam)) {
  #  test_stat[i] <- test_result[,i]$p.value
  #}
  #similar <- ifelse(test_stat >= 0.05, TRUE, FALSE)
  similar <- test_stat #ifelse(test_stat == 0, TRUE, FALSE)
  # second criteria
#  similar <- ifelse(near(sorted_serie_dam[20,], sorted_serie_nodam[20,], tol = 5), TRUE, similar)
  
  pooled_data <- matrix(data = 0, nrow = 30, ncol = sum(similar))
  pooled_data <- rbind(sorted_serie_dam[, similar], sorted_serie_nodam[, similar])

  # now the reaining cells for both scenario
  remain_dam   <- sorted_serie_dam[,!similar]
  remain_nodam <- sorted_serie_nodam[,!similar]

  # sort
  sorted_pooled <- apply(pooled_data,  2, function(x) sort(x, na.last = TRUE))
  sorted_dam    <- apply(remain_dam,   2, function(x) sort(x, na.last = TRUE))
  sorted_nodam  <- apply(remain_nodam, 2, function(x) sort(x, na.last = TRUE))

  # fit
  M1_pooled <- apply(sorted_pooled, 2, function(x) mean(x, na.rm = TRUE))
  M2_pooled <- apply(sorted_pooled, 2, function(x) 1 / 60 * sum((seq(1, 60, 1) - 1) / (60 - 1) * x))
  L1_pooled <- M1_pooled
  L2_pooled <- 2 * M2_pooled - M1_pooled

  M1_dam <- apply(sorted_dam, 2, function(x) mean(x, na.rm = TRUE))
  M2_dam <- apply(sorted_dam, 2, function(x) 1 / 30 * sum((seq(1, 30, 1) - 1) / (30 - 1) * x))
  L1_dam <- M1_dam
  L2_dam <- 2 * M2_dam - M1_dam

  M1_nodam <- apply(sorted_nodam, 2, function(x) mean(x, na.rm = TRUE))
  M2_nodam <- apply(sorted_nodam, 2, function(x) 1 / 30 * sum((seq(1, 30, 1) - 1) / (30 - 1) * x))
  L1_nodam <- M1_nodam
  L2_nodam <- 2 * M2_nodam - M1_nodam

# add derive the final parameters
  alpha_pooled   <- L2_pooled / log(2)
  epsilon_pooled <- L1_pooled - alpha_pooled * 0.57721

  alpha_dam   <- L2_dam / log(2)
  epsilon_dam <- L1_dam - alpha_dam * 0.57721

  alpha_nodam   <- L2_nodam / log(2)
  epsilon_nodam <- L1_nodam - alpha_nodam * 0.57721

  # need to put back the two dataset together
  id <- seq(1, 60589)
  dam_temp   <- data.frame(id = c(id[similar], id[!similar]), epsilon = c(epsilon_pooled, epsilon_dam), alpha = c(alpha_pooled, alpha_dam))
  nodam_temp <- data.frame(id = c(id[similar], id[!similar]), epsilon = c(epsilon_pooled, epsilon_nodam), alpha = c(alpha_pooled, alpha_nodam))

  dam_temp   <- dam_temp[order(dam_temp$id),]
  nodam_temp <- nodam_temp[order(nodam_temp$id),]

  # 30 and 100 values ?
  Y100_dam   <- dam_temp$epsilon - dam_temp$alpha     * log(-log(1 - 1/100))
  Y100_nodam <- nodam_temp$epsilon - nodam_temp$alpha * log(-log(1 - 1/100))
  Y30_dam    <- dam_temp$epsilon - dam_temp$alpha     * log(-log(1 - 1/30))
  Y30_nodam  <- nodam_temp$epsilon - nodam_temp$alpha * log(-log(1 - 1/30))

  # save all data
  boot_100y_dam[j, ]   <- Y100_dam
  boot_100y_nodam[j, ] <- Y100_nodam
  boot_30y_dam[j, ]    <- Y30_dam
  boot_30y_nodam[j, ]  <- Y30_nodam

  boot_para1_dam[j, ]   <- dam_temp$epsilon
  boot_para1_nodam[j, ] <- nodam_temp$epsilon
  boot_para2_dam[j, ]   <- dam_temp$alpha
  boot_para2_nodam[j, ] <- nodam_temp$alpha
}

## compute the expected discharge > use the bootstrap estimated parameters...
ave_100_dam   <- apply(boot_100y_dam, 2,   function(x) quantile(x, 0.5, na.rm = TRUE))
ave_100_nodam <- apply(boot_100y_nodam, 2, function(x) quantile(x, 0.5, na.rm = TRUE))
ave_30_dam    <- apply(boot_30y_dam, 2,    function(x) quantile(x, 0.5, na.rm = TRUE))
ave_30_nodam  <- apply(boot_30y_nodam, 2,  function(x) quantile(x, 0.5, na.rm = TRUE))


Y_dam   <- dam_para$epsilon - dam_para$alpha * log(-log(1 - 1/X))
Y_nodam <- nodam_para$epsilon - nodam_para$alpha  * log(-log(1 - 1/X))

Y_dam <- ifelse(Y_dam > 0, Y_dam, NA)
Y_nodam <- ifelse(Y_nodam > 0, Y_nodam, NA)

## Add comparison
new_return_period_dam   <- 1 / (-exp(-exp((dam_para$alpha - mask$reference) / dam_para$epsilon)) + 1)
new_return_period_nodam <- 1 / (-exp(-exp((nodam_para$alpha - mask$reference) / nodam_para$epsilon)) + 1)

new_return_period_dam   <- ifelse(is.finite(new_return_period_dam)   != TRUE, NA, new_return_period_dam)
new_return_period_nodam <- ifelse(is.finite(new_return_period_nodam) != TRUE, NA, new_return_period_nodam)


## write outputs
out_dam <- data.frame(L = mask$L,
                  change = new_return_period_dam,
                  return_30  = Y_dam[, 1],
                  return_100 = Y_dam[, 2],
                  stat = NA, #unname(stat_test),
                  para1 = dam_para$epsilon,
                  para2 = dam_para$alpha,
                  para3 = ave_30_dam,
                  para4 = ave_100_dam)

out_nodam <- data.frame(L = mask$L,
                  change = new_return_period_nodam,
                  return_30  = Y_nodam[, 1],
                  return_100 = Y_nodam[, 2],
                  stat = NA, #unname(stat_test),
                  para1 = nodam_para$epsilon,
                  para2 = nodam_para$alpha,
                  para3 = ave_30_nodam,
                  para4 = ave_100_nodam)

## write

# write files
#if (threshold == 30) {
#  if (MODELS == "CAMA") {
#    write_csv(out_dam, paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP[1], "/global_gev_compared_", VARS, "_Lmoment_", indice, "_v2.csv", sep = ""))
#    write_csv(out_nodam, paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP[2], "/global_gev_compared_", VARS, "_Lmoment_",# indice, "_v2.csv", sep = ""))
    
    # dams
#    file_out <- file(paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP[1],"/global_gev_para1_", VARS, "_Lmoment_", indice,# "_v1.bin", sep = ""), open = "wb")
#    for (j in 1:bootstrap) {
 #     writeBin(boot_para1_dam[j,], con = file_out, endian = "little")
#    }
#    close(file_out)
    
#    file_out <- file(paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP[1],"/global_gev_para2_", VARS, "_Lmoment_", indice,# "_v1.bin", sep = ""), open = "wb")
#    for (j in 1:bootstrap) {
#      writeBin(boot_para2_dam[j,], con = file_out, endian = "little")
#    }
#    close(file_out)
    
    # no dams
#    file_out <- file(paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP[2], "/global_gev_para1_", VARS, "_Lmoment_", indice#, "_v1.bin", sep = ""), open = "wb")
#    for (j in 1:bootstrap) {
#      writeBin(boot_para1_nodam[j,], con = file_out, endian = "little")
#    }
#    close(file_out)

#    file_out <- file(paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, EXP[2], "/global_gev_para2_", VARS, "_Lmoment_", indice#, "_v1.bin", sep = ""), open = "wb")
#    for (j in 1:bootstrap) {
#      writeBin(boot_para2_nodam[j,], con = file_out, endian = "little")
#    }
#    close(file_out)
    
  
#  } else {
#    print("not supported yet")
    #write_csv(out, paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, "_", EXP, "/global_gev_compared_", VARS, "_Lmoment_", indice, "_h08_v1.csv", sep = ""))
#  }    
#} else {
  if (MODELS == "CAMA") {
    write_csv(out_dam, paste("/data01/julien/projects/camaflood/OUT/thres", threshold, "/global_", GCMS, EXP[1], "/global_gev_compared_", VARS, "_Lmoment_", indice, "_v2.csv", sep = ""))
    write_csv(out_nodam, paste("/data01/julien/projects/camaflood/OUT/thres", threshold, "/global_", GCMS, EXP[2], "/global_gev_compared_", VARS, "_Lmoment_", indice, "_v2.csv", sep = ""))
    
    # dams
    file_out <- file(paste("/data01/julien/projects/camaflood/OUT/thres", threshold, "/global_", GCMS, EXP[1],"/global_gev_para1_", VARS, "_Lmoment_", indice, "_v1.bin", sep = ""), open = "wb")
    for (j in 1:bootstrap) {
      writeBin(boot_para1_dam[j,], con = file_out, endian = "little")
    }
    close(file_out)
    
    file_out <- file(paste("/data01/julien/projects/camaflood/OUT/thres", threshold, "/global_", GCMS, EXP[1],"/global_gev_para2_", VARS, "_Lmoment_", indice, "_v1.bin", sep = ""), open = "wb")
    for (j in 1:bootstrap) {
      writeBin(boot_para2_dam[j,], con = file_out, endian = "little")
    }
    close(file_out)
    
    # no dams
    file_out <- file(paste("/data01/julien/projects/camaflood/OUT/thres", threshold, "/global_", GCMS, EXP[2], "/global_gev_para1_", VARS, "_Lmoment_", indice, "_v1.bin", sep = ""), open = "wb")
    for (j in 1:bootstrap) {
      writeBin(boot_para1_nodam[j,], con = file_out, endian = "little")
    }
    close(file_out)

    file_out <- file(paste("/data01/julien/projects/camaflood/OUT/thres", threshold, "/global_", GCMS, EXP[2], "/global_gev_para2_", VARS, "_Lmoment_", indice, "_v1.bin", sep = ""), open = "wb")
    for (j in 1:bootstrap) {
      writeBin(boot_para2_nodam[j,], con = file_out, endian = "little")
    }
    close(file_out)
    
  
  } else {
    print("not supported yet")
    #write_csv(out, paste("/data01/julien/projects/camaflood/OUT/global_", GCMS, "_", EXP, "/global_gev_compared_", VARS, "_Lmoment_", indice, "_h08_v1.csv", sep = ""))
  }  
}

