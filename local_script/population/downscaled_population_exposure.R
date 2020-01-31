## Purpose:
# use the new estimate using the downscaled dataset
# available at the country scale and at the global scale (with position relative to dams)

## Libraries:
library("tidyverse")
library("RColorBrewer")
library("extrafont") ; loadfonts() # device = "win"


## Load datasets
## with position
for (GCMS in c("H2C_", "G2C_", "M2C_", "I2C_","H3C_", "G3C_", "M3C_", "I3C_",  "H0C_", "G0C_", "M0C_", "I0C_")) { #
  for (SCE in c("dam_trim", "nodam_trim")) {
    if (SCE == "dam_trim" & GCMS %in% c("H0C_", "M0C_", "I0C_", "G0C_")) {
      next
    }
    
    temp <- read_csv(paste0("./Inputs/global/global_", GCMS, SCE, "/position_exposure.csv"),
                     col_types = cols(position = col_integer(),
                                      tot_ex   = col_double(),
                                      year     = col_integer(),
                                      region = col_character()))
    
    if (GCMS %in% c("H2C_", "I2C_", "M2C_", "G2C_")) {
      temp$rcp <- "rcp2.6"
    } else if (GCMS %in% c("H3C_", "I3C_", "M3C_", "G3C_")) {
      temp$rcp <- "rcp6.0"
    } else {
      temp$rcp <- "hist"
    }
    
    # add GCM family
    if (GCMS %in% c("H0C_", "H2C_", "H3C_")) {
      temp$family <- "HadGEM2-ES"
    } else if (GCMS %in% c("M0C_", "M2C_", "M3C_")) {
      temp$family <- "MIROC5"
    } else if (GCMS %in% c("I0C_", "I2C_", "I3C_")) {
      temp$family <- "IPSL-CM5A-LR"
    } else if (GCMS %in% c("G0C_", "G2C_", "G3C_")) {
      temp$family <- "GFDL-ESM2M"
    }
    
    temp$GCM <- GCMS
    temp$exp <- SCE
    
    # Combined
    if (GCMS == "H2C_" & SCE == "dam_trim") {
      population_exp <- temp  
    } else {
      population_exp <- rbind(population_exp, temp)
    }
    
  }
}

# clean
rm(temp) ; rm(SCE) ; rm(GCMS)

population_exp


## aggregate regions
population_exp_v2 <- 
population_exp %>% group_by(position, year, rcp, family, GCM, exp) %>%
  summarise(total_exposure = sum(tot_ex, na.rm = TRUE))

population_exp_v2

## Global exposure
population_exp_v3 <- 
population_exp_v2 %>% #filter(is.na(position) | position %in% c(2,3,4)) %>%
  group_by(year, rcp, family, GCM, exp) %>%
  summarise(global_exposure = sum(total_exposure, na.rm = TRUE))
population_exp_v3
# max exposure is about 53 million people

# how to agreate > taking average among GCM does not seam a good idea.
# just report the overall range?

global_exposure_end_century <- 
population_exp_v3 %>% filter(year >= 2070) %>%
  group_by(rcp, exp) %>%
  summarise(m1 = mean(global_exposure)/1e6,
            q1 = quantile(global_exposure, 0.025)/1e6,
            q2 = quantile(global_exposure, 0.975)/1e6)
  



# comparison
population_exp_v4 <- 
population_exp_v3 %>% group_by(exp, year, rcp) %>%
  summarise(ave_exposure = mean(global_exposure))
population_exp_v4

g <- ggplot()
g + geom_line(data = population_exp_v4,
              aes(x = year, y = ave_exposure / 1e6, color = exp, linetype = rcp)) 

  