#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

## libraries
library("tibble")
library("readr")
suppressMessages(library("dplyr"))

if (length(args) != 2) {
  stop("Required: GCM ; indice")
} else {
  gcms      <- args[1] #"G2C_"
  exp       <- args[2] # "nodam_trim"
}

if (gcms %in% c("H2C_", "G2C_", "M2C_", "I2C_",
                "H3C_", "G3C_", "M3C_", "I3C_")) {
  year_sta <- 2006
  year_end <- 2036
} else {
  year_sta <- 1861
  year_end <- 1891
}

basins <- c("amazon", "nile", "congo", "mississippi", "amur", "parana",
            "yenisey", "ob-irtysh", "lena", "niger", "zambezi", "yangtze",
            "mackenzie", "chari", "volga", "st lawrence", "indus",
            "syr darya", "oricono", "murray", "ganges", "shatt al arab",
            "orange", "huanghe", "yukon", "sensgal", "colorado",
            "rio grande", "danube", "mekong", "tocantins", "columbia",
            "darling", "brahmuputra", "sao francisco", "amu darya",
            "dnieper")

basins_id <- seq(1, 37)
#              ok   higher   ok      ok     ok
basins_l <- c(132010, 96186, 135752, 80818, 56796,
#              ok      ok    ok     ok    higher
              168723, 45185, 33614, 27975, 117011,
#               ok     ok      ok    ok     ok
              153067, 85543, 35387, 111990, 60210,
#                ok    low    ok     ok     low     ok
              63573, 94097, 66734, 117593, 179919, 95579,
#              ok      ok     ok     ok   higher   ok      ok
              80366, 171045, 79782, 40357, 109057, 82931, 91602,
#             ok       ok      ok     ok    low     low     ok
              65205, 111452, 137062, 63478, 177045, 94860, 143567,
#              ok     ok
              68159, 56582)

for (years in seq(year_sta, year_end, 1)) {
  # Floodplain depth
  file_in2 <- file(paste("/data01/julien/models/camaflood/out/global_",
                         gcms, exp, "/outflw", years, ".bin", sep = ""),
                   open = "rb")

  d <- seq(as.Date(paste(years, "/1/1", sep = "")),
           as.Date(paste(years, "/12/31", sep = "")), "day")
  yearly_fld <- matrix(data = 0, nrow = length(d), ncol = 259200)

  for (nb_days in seq_len(length(d))) {
    t <- readBin(con = file_in2, what = "numeric",
                 n = 259200, size = 4, endian = "little")
    yearly_fld[nb_days, ] <- t
  }
  close(file_in2)

  # save the max
  if (years == year_sta) {
    max_b <- apply(yearly_fld[, basins_l], 2,
                   function(x) {
                     max(x, na.rm = TRUE)
                   }
                   )
  } else {
    temp <- apply(yearly_fld[, basins_l], 2,
                  function(x) {
                    max(x, na.rm = TRUE)
                  }
                  )
    max_b <- ifelse(temp > max_b, temp, max_b)
  }
} # all done

# save
write_csv(tibble(id = basins_id,
                 basin = basins,
                 l = basins_l,
                 max_f = max_b),
          paste0("./../OUT/max_flow_", gcms, exp, ".csv"))
