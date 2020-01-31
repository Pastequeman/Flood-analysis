## Purpose:
# read the 100-y return discharge
# reproduce Hirabayashi-san work
#
# rcp2.6 only

## updated: 22/04/2019


## Library:
library("tidyverse")
library("RColorBrewer")
library("extrafont") ; loadfonts(device = "win") #device = "win"
library("sf")

##### theme and axis maps #####
theme_map <- function(...) {
  theme_minimal() +
    theme(
      legend.margin = margin(t = 0, r = 0, b = -0.1, l = 0, unit = "cm"),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(c(0,0,0,0), "cm"),
      legend.text = element_text(family = "Times New Roman", color = "black", size = 8),
      legend.key = element_blank(),
      legend.spacing.y = unit(0.1, "cm"),
      plot.margin = margin(t = -0.75, l = -0.2, b = 0, r = 0.2, "cm"),
      plot.title = element_text(family = "Times New Roman", color = "black", size = 10, vjust = -0.5),
      text = element_text(family = "Times New Roman", color = "#22211d", size = 10),
      axis.line = element_blank(), #element_line(size = 1, colour = "grey80"),
      axis.text.x = element_text(family = "Times New Roman", color = "#22211d", size = 7), 
      axis.text.y = element_text(family = "Times New Roman", color = "#22211d", size = 7), 
      axis.ticks = element_blank(),
      axis.title.x = element_text(family = "Times New Roman", color = "#22211d", face = "bold", size = 7), #element_blank(),
      axis.title.y = element_text(family = "Times New Roman", color = "#22211d", face = "bold", size = 7), #element_blank(),
      panel.grid.minor = element_line(color = "#ebebe5", size = 0.2, linetype = 2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.390, linetype = 1),
      strip.text = element_text(family = "Times New Roman", face = "bold", size = 7),
      #  panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),   #"#f5f5f2"
      panel.background = element_rect(fill = "white", color = NA), #element_blank(),
      legend.background =  element_rect(fill=scales::alpha('white', 0.4)), #element_blank(),
      panel.border = element_blank(),
      ...
    )
}
theme_map_short <- function(...) {
  theme_minimal() +
    theme(
      legend.margin = margin(t = 0, r = 0, b = -0.1, l = 0, unit = "cm"),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(c(0,0,0,0), "cm"),
      legend.text = element_text(family = "Times New Roman", color = "black", size = 8),
      legend.key = element_blank(),
      legend.spacing.y = unit(-0.1, "cm"),
      plot.margin = margin(t = -0.75, l = -0.2, b = -0.75, r = 0.2, "cm"),
      plot.title = element_text(family = "Times New Roman", color = "black", size = 10, vjust = -0.5),
      text = element_text(family = "Times New Roman", color = "#22211d", size = 10),
      axis.line = element_blank(), #element_line(size = 1, colour = "grey80"),
      axis.text.x = element_text(family = "Times New Roman", color = "#22211d", size = 5), 
      axis.text.y = element_text(family = "Times New Roman", color = "#22211d", size = 5), 
      axis.ticks = element_blank(),
      axis.title.x = element_text(family = "Times New Roman", color = "#22211d", face = "bold", size = 7), #element_blank(),
      axis.title.y = element_text(family = "Times New Roman", color = "#22211d", face = "bold", size = 7), #element_blank(),
      panel.grid.minor = element_line(color = "#ebebe5", size = 0.2, linetype = 2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.390, linetype = 1),
      strip.text = element_text(family = "Times New Roman", face = "bold", size = 7),
      #  panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),   #"#f5f5f2"
      panel.background = element_rect(fill = "white", color = NA), #element_blank(),
      legend.background =  element_rect(fill = "white", color = NA), #element_blank(),
      panel.border = element_blank(),
      ...
    )
}
theme_map2 <- function(...) {
  theme_minimal() +
    theme(
      legend.margin = margin(t = 0, r = 0, b = -0.1, l = 0, unit = "cm"),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(c(0,0,0,0), "cm"),
      legend.text = element_text(family = "Times New Roman", color = "black", size = 8),
      legend.key = element_blank(),
      legend.spacing.y = unit(-0.1, "cm"),
      plot.margin = margin(t = -0.75, l = -0.2, b = -0.75, r = 0.2, "cm"),
      plot.title = element_text(family = "Times New Roman", color = "black", size = 10, vjust = -0.5),
      text = element_text(family = "Times New Roman", color = "#22211d", size = 10),
      axis.line = element_blank(), #element_line(size = 1, colour = "grey80"),
      axis.text.x = element_text(family = "Times New Roman", color = "#22211d", size = 7), 
      axis.text.y = element_text(family = "Times New Roman", color = "#22211d", size = 7), 
      axis.ticks = element_blank(),
      axis.title.x = element_text(family = "Times New Roman", color = "#22211d", face = "bold", size = 7), #element_blank(),
      axis.title.y = element_text(family = "Times New Roman", color = "#22211d", face = "bold", size = 7), #element_blank(),
      panel.grid.minor = element_line(color = "#ebebe5", size = 0.2, linetype = 2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.390, linetype = 1),
      strip.text = element_text(family = "Times New Roman", face = "bold", size = 7),
      #  panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),   #"#f5f5f2"
      panel.background = element_rect(fill = "white", color = NA), #element_blank(),
      legend.background =  element_rect(fill = "white", color = NA), #element_blank(),
      panel.border = element_blank(),
      ...
    )
}
theme_map_or <- function(...) {
  theme_minimal() +
    theme(
      legend.margin = margin(t = 0, r = 0, b = -0.1, l = 0, unit = "cm"),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(c(0,0,0,0), "cm"),
      legend.text = element_text(family = "Times New Roman", color = "black", size = 8),
      legend.key = element_blank(),
      legend.spacing.y = unit(-0.1, "cm"),
      plot.margin = margin(t = -0.75, l = -0.2, b = 0, r = 0.2, "cm"),
      plot.title = element_text(family = "Times New Roman", color = "black", size = 10, vjust = -0.5),
      text = element_text(family = "Times New Roman", color = "#22211d", size = 10),
      axis.line = element_blank(), #element_line(size = 1, colour = "grey80"),
      axis.text.x = element_text(family = "Times New Roman", color = "#22211d", size = 7), 
      axis.text.y = element_text(family = "Times New Roman", color = "#22211d", size = 7), 
      axis.ticks = element_blank(),
      axis.title.x = element_text(family = "Times New Roman", color = "#22211d", face = "bold", size = 7), #element_blank(),
      axis.title.y = element_text(family = "Times New Roman", color = "#22211d", face = "bold", size = 7), #element_blank(),
      panel.grid.minor = element_line(color = "#ebebe5", size = 0.2, linetype = 2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.390, linetype = 1),
      strip.text = element_text(family = "Times New Roman", face = "bold", size = 7),
      #  panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),   #"#f5f5f2"
      panel.background = element_rect(fill = "white", color = NA), #element_blank(),
      legend.background =  element_rect(fill = "white", color = NA), #element_blank(),
      panel.border = element_blank(),
      ...
    )
}
# axis
world2 <- map_data("world", wrap = c(-180, 180)) %>% filter(region != "Antarctica")

world.sf <- sf::st_as_sf(world2, coords = c("long", "lat"), crs = 4326) %>% 
  group_by(group) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>% 
  ungroup()

ewbrks <-  c(-160, -80, 0 , 80, 160) # seq(-180, 180, 50)
nsbrks <-  c(-50, 0, 50)             # seq(-90, 90, 50)
ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste(abs(x), "°W"), ifelse(x > 0, paste(x, "°E"), x)) ))
nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste(abs(x), "°S"), ifelse(x > 0, paste(x, "°N"), x)) ))


##### read the files #####

# Koppen Geiger
koppen <- read_table2("Inputs/Koeppen-Geiger-ASCII.txt")
# fix artifact in the dataset
koppen$Cls[koppen$Lat == 20.75 & koppen$Lon == 17.25] <- "BWh"
# remove "BWh" and "EF"
# add L
temp <- tibble(lon = rep(seq(-179.75, 179.75, by = 0.5), times = 360),
               lat = rep(seq(89.75, -89.75, by = -0.5), each = 720),
               L   = seq(1, 259200, 1))
koppen <- koppen %>% left_join(temp, by = c("Lon" = "lon", "Lat"  = "lat"))


##### flood frequency baseline #####

for (GCMS in c("H0C_", "G0C_", "M0C_", "I0C_")) {
  temp <- read_csv(paste("Inputs/global/global_", GCMS, "nodam_trim/global_reference_return_outflw_Lmoment_gumbel_max_v1.csv", sep = ""),
                   col_types = cols(L          = col_integer(),
                                    #  change     = col_double(),
                                    return_30  = col_double(),
                                    return_100 = col_double(),
                                    stat       = col_double(),
                                    para1      = col_double(),
                                    para2      = col_double(),
                                    para3      = col_double(),
                                    para4      = col_double())
  )
  
  # add gcm info
  temp$GCM <- GCMS
  
  if (GCMS == "H0C_") {
    global_hist <- temp
  } else {
    global_hist <- rbind(global_hist, temp)
  }
}
global_hist

## the quantile hist
for (GCMS in c("H0C_", "G0C_", "M0C_", "I0C_")) {
  temp <- read_csv(paste("Inputs/global/global_", GCMS, "nodam_trim/global_reference_return_outflw_Lmoment_gumbel_quantile_v1.csv", sep = ""),
                   col_types = cols(L          = col_integer(),
                                    #  change     = col_double(),
                                    return_30  = col_double(),
                                    return_100 = col_double(),
                                    stat       = col_double(),
                                    para1      = col_double(),
                                    para2      = col_double(),
                                    para3      = col_double(),
                                    para4      = col_double())
  )
  
  # add gcm info
  temp$GCM <- GCMS
  
  if (GCMS == "H0C_") {
    global_hist_qua <- temp
  } else {
    global_hist_qua <- rbind(global_hist_qua, temp)
  }
}
global_hist_qua

## dam and no dams scenarios
for (GCMS in c("H3C_", "G3C_", "M3C_", "I3C_")) {
  temp <- read_csv(paste("Inputs/global/thres150/global_", GCMS, "dam_trim/global_gev_compared_outflw_Lmoment_max_v2.csv", sep = ""),
                   col_types = cols(L = col_integer(),
                                    change = col_double(),
                                    return_30 = col_double(),
                                    return_100 = col_double(),
                                    stat = col_double(),
                                    para1 = col_double(),
                                    para2 = col_double(),
                                    para3 = col_double(),
                                    para4 = col_double())
  )
  
  # add gcm info
  temp$GCM <- GCMS
  
  if (GCMS == "H3C_") {
    global_new_return_rcp_60_dam <- temp
  } else {
    global_new_return_rcp_60_dam <- rbind(global_new_return_rcp_60_dam, temp)
  }
}
## recompile the change
# 100 y 
#global_new_return_rcp_26_dam$return_100 <- global_new_return_rcp_26_dam$para3 - global_new_return_rcp_26_dam$para4 * log(-log(1 - 1/100))
# change
#global_new_return_rcp_26_dam$change <- 1 / (-exp(-exp((global_hist$para3 - global_new_return_rcp_26_dam$return_100) / global_hist$para4))+ 1)
global_new_return_rcp_60_dam$change <- 1 / (-exp(-exp((global_new_return_rcp_60_dam$para1 - 
                                                         global_hist$para4) / global_new_return_rcp_60_dam$para2))+ 1)

global_new_return_rcp_60_dam
# no dam scenarios
for (GCMS in c("H3C_", "G3C_", "M3C_", "I3C_")) {
  temp <- read_csv(paste("Inputs/global/thres150/global_", GCMS, "nodam_trim/global_gev_compared_outflw_Lmoment_max_v2.csv", sep = ""),
                   col_types = cols(L          = col_integer(),
                                    change     = col_double(),
                                    return_30  = col_double(),
                                    return_100 = col_double(),
                                    stat       = col_double(),
                                    para1      = col_double(),
                                    para2      = col_double(),
                                    para3      = col_double(),
                                    para4      = col_double())
  )
  
  # add gcm info
  temp$GCM <- GCMS
  
  if (GCMS == "H3C_") {
    global_new_return_rcp_60_nodam <- temp
  } else {
    global_new_return_rcp_60_nodam <- rbind(global_new_return_rcp_60_nodam, temp)
  }
}
# change: how is the expectation for the historical 100-y flood
global_new_return_rcp_60_nodam$change <- 1 / (-exp(-exp((global_new_return_rcp_60_nodam$para1 - 
                                                           global_hist$para4) / global_new_return_rcp_60_nodam$para2))+ 1)

global_new_return_rcp_60_nodam

## quantile data
# dam
for (GCMS in c("H3C_", "G3C_", "M3C_", "I3C_")) {
  temp <- read_csv(paste("Inputs/global/thres150/global_", GCMS, "dam_trim/global_gev_compared_outflw_Lmoment_quantile_v2.csv", sep = ""),
                   col_types = cols(L = col_integer(),
                                    change = col_double(),
                                    return_30 = col_double(),
                                    return_100 = col_double(),
                                    stat = col_double(),
                                    para1 = col_double(),
                                    para2 = col_double(),
                                    para3 = col_double(),
                                    para4 = col_double())
  )
  
  # add gcm info
  temp$GCM <- GCMS
  
  if (GCMS == "H3C_") {
    global_new_return_rcp_60_dam_qua <- temp
  } else {
    global_new_return_rcp_60_dam_qua <- rbind(global_new_return_rcp_60_dam_qua, temp)
  }
}
## recompile the change
global_new_return_rcp_60_dam_qua$change <- 1 / (-exp(-exp((global_new_return_rcp_60_dam_qua$para1 - 
                                                         global_hist_qua$para4) / global_new_return_rcp_60_dam_qua$para2))+ 1)

global_new_return_rcp_60_dam_qua
# no dam scenarios
for (GCMS in c("H3C_", "G3C_", "M3C_", "I3C_")) {
  temp <- read_csv(paste("Inputs/global/thres150/global_", GCMS, "nodam_trim/global_gev_compared_outflw_Lmoment_quantile_v2.csv", sep = ""),
                   col_types = cols(L          = col_integer(),
                                    change     = col_double(),
                                    return_30  = col_double(),
                                    return_100 = col_double(),
                                    stat       = col_double(),
                                    para1      = col_double(),
                                    para2      = col_double(),
                                    para3      = col_double(),
                                    para4      = col_double())
  )
  
  # add gcm info
  temp$GCM <- GCMS
  
  if (GCMS == "H3C_") {
    global_new_return_rcp_60_nodam_qua <- temp
  } else {
    global_new_return_rcp_60_nodam_qua <- rbind(global_new_return_rcp_60_nodam_qua, temp)
  }
}
# change: how is the expectation for the historical 100-y flood
global_new_return_rcp_60_nodam_qua$change <- 1 / (-exp(-exp((global_new_return_rcp_60_nodam_qua$para1 - 
                                                           global_hist_qua$para4) / global_new_return_rcp_60_nodam_qua$para2))+ 1)

global_new_return_rcp_60_nodam

## add ranking
global_new_return_rcp_60_nodam$rank <- ifelse(global_new_return_rcp_60_nodam$return_30 >= 2.5, "A", "B")
global_new_return_rcp_60_dam$rank <- ifelse(global_new_return_rcp_60_dam$return_30 >= 2.5, "A", "B")
#### RCP6.0: dam simulation #####

# better to convert the change to range after takin gGCM average
## try filtering
global_new_return_rcp_60_dam_v2 <-
  global_new_return_rcp_60_dam %>% group_by(L) %>% summarise(n = n(), 
                                                             ave_30_y = mean(return_30, na.rm = TRUE),
                                                             change_2 = median(change, na.rm = TRUE),
                                                             dev_change = sd(change, na.rm = TRUE))



global_new_return_rcp_60_dam_v2$new_range <- ifelse(global_new_return_rcp_60_dam_v2$change_2 <= 5, "  2-5   ", NA)
global_new_return_rcp_60_dam_v2$new_range <- ifelse(global_new_return_rcp_60_dam_v2$change_2 > 5 & global_new_return_rcp_60_dam_v2$change_2 <= 25, "  5-25  ", global_new_return_rcp_60_dam_v2$new_range)
global_new_return_rcp_60_dam_v2$new_range <- ifelse(global_new_return_rcp_60_dam_v2$change_2 > 25 & global_new_return_rcp_60_dam_v2$change_2 <= 50, " 25-50  ", global_new_return_rcp_60_dam_v2$new_range)
global_new_return_rcp_60_dam_v2$new_range <- ifelse(global_new_return_rcp_60_dam_v2$change_2 > 50 & global_new_return_rcp_60_dam_v2$change_2 <= 75, " 50-75  ", global_new_return_rcp_60_dam_v2$new_range)
global_new_return_rcp_60_dam_v2$new_range <- ifelse(global_new_return_rcp_60_dam_v2$change_2 > 75 & global_new_return_rcp_60_dam_v2$change_2 <= 95, " 75-95  ", global_new_return_rcp_60_dam_v2$new_range)
global_new_return_rcp_60_dam_v2$new_range <- ifelse(global_new_return_rcp_60_dam_v2$change_2 > 95 & global_new_return_rcp_60_dam_v2$change_2 <= 105, " 95-105 ", global_new_return_rcp_60_dam_v2$new_range)
global_new_return_rcp_60_dam_v2$new_range <- ifelse(global_new_return_rcp_60_dam_v2$change_2 > 105 & global_new_return_rcp_60_dam_v2$change_2 <= 125, "105-125 ", global_new_return_rcp_60_dam_v2$new_range)
global_new_return_rcp_60_dam_v2$new_range <- ifelse(global_new_return_rcp_60_dam_v2$change_2 > 125 & global_new_return_rcp_60_dam_v2$change_2 <= 250, "125-250 ", global_new_return_rcp_60_dam_v2$new_range)
global_new_return_rcp_60_dam_v2$new_range <- ifelse(global_new_return_rcp_60_dam_v2$change_2 > 250 & global_new_return_rcp_60_dam_v2$change_2 <= 500, "250-500 ", global_new_return_rcp_60_dam_v2$new_range)
global_new_return_rcp_60_dam_v2$new_range <- ifelse(global_new_return_rcp_60_dam_v2$change_2 > 500 & global_new_return_rcp_60_dam_v2$change_2 <= 1000, "500-1000", global_new_return_rcp_60_dam_v2$new_range)
global_new_return_rcp_60_dam_v2$new_range <- ifelse(global_new_return_rcp_60_dam_v2$change_2 >= 1000, "   >1000", global_new_return_rcp_60_dam_v2$new_range)

global_new_return_rcp_60_dam_v2$new_range <- factor(global_new_return_rcp_60_dam_v2$new_range, ordered = TRUE, 
                                                    levels = c("  2-5   ", 
                                                               "  5-25  ", 
                                                               " 25-50  ", 
                                                               " 50-75  ", 
                                                               " 75-95  ",
                                                               " 95-105 ", 
                                                               "105-125 ", 
                                                               "125-250 ", 
                                                               "250-500 ", 
                                                               "500-1000", 
                                                               "   >1000"))

temp <- tibble(L   = seq(1, 259200, 1),
               lon = rep(seq(-179.75, 179.75, by = 0.5), times = 360),
               lat = rep(seq(89.75, -89.75, by = -0.5), each = 720))

global_new_return_rcp_60_dam_v2 <- global_new_return_rcp_60_dam_v2 %>% left_join(temp, by = "L")

global_new_return_rcp_60_dam_v2 <- global_new_return_rcp_60_dam_v2 %>% 
  left_join(koppen, by = c("lon" = "Lon", "lat" = "Lat", "L" = "L"))
# filter 
# keep if discharge is high enough
global_new_return_rcp_60_dam_v2$rank <- ifelse(global_new_return_rcp_60_dam_v2$ave_30_y >= 5, "A", "B")
global_new_return_rcp_60_dam_v2 <- global_new_return_rcp_60_dam_v2 %>% 
  filter(!(Cls %in% c("BWh", "EF", "BWk") & rank != "A"))


#### dam plot ####
# alternative legend
agg_range <- global_new_return_rcp_60_dam_v2 %>% filter(new_range != "NA") %>% 
  group_by(new_range) %>%
  summarize(n_range = n())
agg_range

alt_legend <- ggplot(data = agg_range, 
                     aes(y = n_range, x = new_range, fill = new_range)) + 
  geom_bar(stat = 'identity', width = 0.5) + 
  scale_fill_manual(values = c("#1A184A", "#3563AD", "#3593D3", "#4CC6E1", "#A1DBE2", "grey60",
                               "#F4F68B", "#FFCA05", "#F0691F", "#F07599","#E72521")) + 
  scale_y_continuous("", expand = c(0, 0), breaks = seq(0, 10000, 5000), 
                     limits = c(0, 12000), labels = c("0", "5k", "10k")) + 
  scale_x_discrete(limits = rev(levels(agg_range$new_range))) + 
  coord_flip() + 
  theme(plot.margin = margin(t = 0, l = 0, b = 0, r = 0, "cm"),
        plot.title = element_text(family = "Times New Roman", size = 5, 
                                  margin = margin(b = -0.1, unit = "cm")), 
        plot.subtitle = element_text(family = "Times New Roman", size = 5), 
        axis.text.y = element_text(family = "Times New Roman", color = "#22211d", size = 4, face = "italic"), 
        axis.text.x = element_text(family = "Times New Roman", color = "#22211d", size = 4, face = "italic",
                                   margin = margin(t = -0.10, unit = "cm")), 
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = seq(0, 10000, 5000), color = "white") #+ 
  #labs(title = expression(bold(" Return period")~"[years]"))

alt_legend


# main plot

map_dam <- 
  ggplot() + 
  geom_sf(data = world.sf, color = "black", fill = "#805c4f", size = 0.05) +
  geom_tile(data = global_new_return_rcp_60_dam_v2 %>% filter(is.na(new_range) != TRUE),
            aes(x = lon, y = lat, fill = new_range), alpha = 0.9) + 
  scale_x_continuous(breaks = ewbrks, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, expand = c(0, 0)) +
  theme_map_short() +
  coord_sf(ylim = c(-64, 90), xlim = c(-180, 180)) +  # , datum = NA
  #coord_equal(ratio = 1, xlim = c(-180, 180)) +
  labs(x = "", y = "", tag = "a") + 
  geom_text(aes(x = -130, y = -59, label = "Return period [years]"), 
            family = "Times New Roman", color = "black", size = 1.5) +
  theme(legend.justification = c(0, 0), legend.position = "none",
        legend.background = element_rect(fill = "transparent", color = NA, size = NA),
        plot.tag.position = c(.15, .95),
        plot.tag = element_text(family = "Times New Roman", color = "#22211d", size = 10, face = "bold"),
        plot.margin = margin(t = -0.5, b = -1.1, l = -0.85, unit = "cm")) +
  scale_fill_manual(values = c("#1A184A", "#3563AD", "#3593D3", "#4CC6E1", "#A1DBE2","grey60",
                               "#F4F68B", "#FFCA05", "#F0691F", "#F07599","#E72521"),
                    name = expression(bold("Return period")~"[years]"),
                    #labels = c("Upstream of dam(s)", "Downstream of dam(s)"),
                    guide = guide_legend(nrow = 2,
                                         direction = "horizontal",
                                         byrow = TRUE,
                                         override.aes = list(alpha = 1, size =  6),
                                         title.position = "left",
                                         title.hjust = 0,
                                         label.hjust = 0)
  )
map_dam

legend_grob = ggplotGrob(alt_legend)

final_plot <- map_dam + annotation_custom(grob = legend_grob, 
                                          xmin = min(global_new_return_rcp_60_dam_v2$lon) + 5, 
                                          xmax = min(global_new_return_rcp_60_dam_v2$lon) + 95, 
                                          ymin = -80, 
                                          ymax = 20)

final_plot


ggsave(paste("Figures/global/rcp60/rcp60_dam_return_period_rev_max_v2.tiff", sep = ""),
       dpi = 600, width = 7, height = 3, units = "cm")

#### RCP6.0: no dam simulation #####

global_new_return_rcp_60_nodam_v2 <-
  global_new_return_rcp_60_nodam %>%  group_by(L) %>% summarise(n = n(),
                                                                ave_30_y = mean(return_30, na.rm = TRUE),
                                                                change_2 = median(change, na.rm = TRUE),
                                                                dev_change = sd(change, na.rm = TRUE))



global_new_return_rcp_60_nodam_v2$new_range <- ifelse(global_new_return_rcp_60_nodam_v2$change_2 <= 5, "  2-5   ", NA)
global_new_return_rcp_60_nodam_v2$new_range <- ifelse(global_new_return_rcp_60_nodam_v2$change_2 > 5 & global_new_return_rcp_60_nodam_v2$change_2 <= 25, "  5-25  ", global_new_return_rcp_60_nodam_v2$new_range)
global_new_return_rcp_60_nodam_v2$new_range <- ifelse(global_new_return_rcp_60_nodam_v2$change_2 > 25 & global_new_return_rcp_60_nodam_v2$change_2 <= 50, " 25-50  ", global_new_return_rcp_60_nodam_v2$new_range)
global_new_return_rcp_60_nodam_v2$new_range <- ifelse(global_new_return_rcp_60_nodam_v2$change_2 > 50 & global_new_return_rcp_60_nodam_v2$change_2 <= 75, " 50-75  ", global_new_return_rcp_60_nodam_v2$new_range)
global_new_return_rcp_60_nodam_v2$new_range <- ifelse(global_new_return_rcp_60_nodam_v2$change_2 > 75 & global_new_return_rcp_60_nodam_v2$change_2 <= 95, " 75-95  ", global_new_return_rcp_60_nodam_v2$new_range)
global_new_return_rcp_60_nodam_v2$new_range <- ifelse(global_new_return_rcp_60_nodam_v2$change_2 > 95 & global_new_return_rcp_60_nodam_v2$change_2 <= 105, " 95-105 ", global_new_return_rcp_60_nodam_v2$new_range)
global_new_return_rcp_60_nodam_v2$new_range <- ifelse(global_new_return_rcp_60_nodam_v2$change_2 > 105 & global_new_return_rcp_60_nodam_v2$change_2 <= 125, "105-125 ", global_new_return_rcp_60_nodam_v2$new_range)
global_new_return_rcp_60_nodam_v2$new_range <- ifelse(global_new_return_rcp_60_nodam_v2$change_2 > 125 & global_new_return_rcp_60_nodam_v2$change_2 <= 250, "125-250 ", global_new_return_rcp_60_nodam_v2$new_range)
global_new_return_rcp_60_nodam_v2$new_range <- ifelse(global_new_return_rcp_60_nodam_v2$change_2 > 250 & global_new_return_rcp_60_nodam_v2$change_2 <= 500, "250-500 ", global_new_return_rcp_60_nodam_v2$new_range)
global_new_return_rcp_60_nodam_v2$new_range <- ifelse(global_new_return_rcp_60_nodam_v2$change_2 > 500 & global_new_return_rcp_60_nodam_v2$change_2 <= 1000, "500-1000", global_new_return_rcp_60_nodam_v2$new_range)
global_new_return_rcp_60_nodam_v2$new_range <- ifelse(global_new_return_rcp_60_nodam_v2$change_2 >= 1000, "   >1000", global_new_return_rcp_60_nodam_v2$new_range)

global_new_return_rcp_60_nodam_v2$new_range <- factor(global_new_return_rcp_60_nodam_v2$new_range, ordered = TRUE, 
                                                      levels = c("  2-5   ", 
                                                                 "  5-25  ", 
                                                                 " 25-50  ", 
                                                                 " 50-75  ", 
                                                                 " 75-95  ",
                                                                 " 95-105 ", 
                                                                 "105-125 ", 
                                                                 "125-250 ", 
                                                                 "250-500 ", 
                                                                 "500-1000", 
                                                                 "   >1000"))

temp <- tibble(L   = seq(1, 259200, 1),
               lon = rep(seq(-179.75, 179.75, by = 0.5), times = 360),
               lat = rep(seq(89.75, -89.75, by = -0.5), each = 720))

global_new_return_rcp_60_nodam_v2 <- global_new_return_rcp_60_nodam_v2 %>% left_join(temp, by = "L")
global_new_return_rcp_60_nodam_v2 <- global_new_return_rcp_60_nodam_v2 %>% left_join(koppen, by = c("lon" = "Lon", "lat" = "Lat", "L" = "L"))
# filter 
global_new_return_rcp_60_nodam_v2$rank <- ifelse(global_new_return_rcp_60_nodam_v2$ave_30_y >= 5, "A", "B")
global_new_return_rcp_60_nodam_v2 <- global_new_return_rcp_60_nodam_v2 %>% 
  filter(!(Cls %in% c("BWh", "EF", "BWk") & rank != "A"))

###### no dam plot #####
uncertain_v2 <- uncertain %>% select(L, lon, lat, class1)
global_new_return_rcp_60_nodam_v2 <- global_new_return_rcp_60_nodam_v2 %>% left_join(uncertain_v2)


agg_range <- global_new_return_rcp_60_nodam_v2 %>% filter(is.na(new_range) != TRUE, class1 == TRUE) %>% 
  group_by(new_range) %>%
  summarize(n_range = n())
agg_range

alt_legend <- ggplot(data = agg_range, 
                     aes(y = n_range, x = new_range, fill = new_range)) + 
  geom_bar(stat = 'identity', width = 0.5) + 
  scale_fill_manual(values = c("#1A184A", "#3563AD", "#3593D3", "#4CC6E1", "#A1DBE2", "grey60",
                               "#F4F68B", "#FFCA05", "#F0691F", "#F07599","#E72521")) + 
  scale_y_continuous("", expand = c(0, 0), breaks = seq(0, 10000, 5000), 
                     limits = c(0, 12000), labels = c("0", "5k", "10k")) + 
  scale_x_discrete(limits = rev(levels(agg_range$new_range))) + 
  coord_flip() + 
  theme(plot.margin = margin(t = 0, l = 0, b = 0, r = 0, "cm"),
        plot.title = element_text(family = "Times New Roman", size = 4, 
                                  margin = margin(b = -0.1, unit = "cm")), 
        plot.subtitle = element_text(family = "Times New Roman", size = 4), 
        axis.text.y = element_text(family = "Times New Roman", color = "#22211d", size = 4, face = "italic"), 
        axis.text.x = element_text(family = "Times New Roman", color = "#22211d", size = 4, face = "italic",
                                   margin = margin(t = -0.10, unit = "cm")), 
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = seq(0, 10000, 5000), color = "white") #+ 
  #labs(title = expression(bold(" Return period")~"[years]"))

alt_legend



## the plot
nodam <- 
  ggplot() + 
  geom_sf(data = world.sf, color = "black", fill = "#805c4f", size = 0.05) +
  geom_tile(data = global_new_return_rcp_60_nodam_v2 %>% filter(is.na(new_range) != TRUE),
            aes(x = lon, y = lat, fill = new_range), alpha = 0.9) + 
  scale_x_continuous(breaks = ewbrks, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, expand = c(0, 0)) +
  theme_map_short() +
  coord_sf(ylim = c(-64, 90), xlim = c(-180, 180)) +  # , datum = NA
  #coord_equal(ratio = 1, xlim = c(-180, 180)) +
  labs(x = "", y = "", tag = "a") + 
  geom_text(aes(x = -130, y = -59, label = "Return period [years]"), 
            family = "Times New Roman", color = "black", size = 1.5) +
  theme(legend.justification = c(0, 0), legend.position = "none",
        legend.background = element_rect(fill = "transparent", color = NA, size = NA),
        plot.tag.position = c(.15, .95),
        plot.tag = element_text(family = "Times New Roman", color = "#22211d", size = 10, face = "bold"),
        plot.margin = margin(t = -0.5, b = -1.1, l = -0.85, unit = "cm")) +
  scale_fill_manual(values = c("#1A184A", "#3563AD", "#3593D3", "#4CC6E1", "#A1DBE2","grey60",
                               "#F4F68B", "#FFCA05", "#F0691F", "#F07599","#E72521"),
                    name = expression(bold("Return period")~"[years]"),
                    #labels = c("Upstream of dam(s)", "Downstream of dam(s)"),
                    guide = guide_legend(nrow = 2,
                                         direction = "horizontal",
                                         byrow = TRUE,
                                         override.aes = list(alpha = 1, size =  6),
                                         title.position = "left",
                                         title.hjust = 0,
                                         label.hjust = 0)
  )

nodam
legend_grob = ggplotGrob(alt_legend)

nodam <- nodam + annotation_custom(grob = legend_grob, 
                                        xmin = min(global_new_return_rcp_60_nodam_v2$lon) + 5, 
                                        xmax = min(global_new_return_rcp_60_nodam_v2$lon) + 95, 
                                        ymin = -80, 
                                        ymax = 20)

nodam

ggsave(paste("Figures/global/rcp60/rcp60_nodam_return_period_rev_max_v2.tiff", sep = ""),
       dpi = 600, width = 7, height = 3, units = "cm")


#### uncertainty #####
# here load both max and quantile
# then look at each grid cell how many of the indicator say increase / decrease (8 in total)
global_new_return_rcp_60_nodam
global_new_return_rcp_60_nodam_qua
global_new_return_rcp_60_nodam_qua$rank <- ifelse(global_new_return_rcp_60_nodam_qua$return_30 >= 5, "A", "B")

uncertain <- rbind(global_new_return_rcp_60_nodam, global_new_return_rcp_60_nodam_qua)
uncertain$dir <- ifelse(uncertain$change > 100, 1, -1)
uncertain <- uncertain %>% group_by(L) %>% summarise(n = n(), 
                                                     un = sum(dir, na.rm = TRUE),
                                                     ave_flw = mean(return_30, na.rm = TRUE))

temp <- tibble(L   = seq(1, 259200, 1),
               lon = rep(seq(-179.75, 179.75, by = 0.5), times = 360),
               lat = rep(seq(89.75, -89.75, by = -0.5), each = 720))

uncertain <- uncertain %>% left_join(temp, by = "L")
uncertain <- uncertain %>% left_join(koppen, by = c("lon" = "Lon", "lat" = "Lat", "L" = "L"))

# filter
uncertain$rank <- ifelse(uncertain$ave_flw >= 5, "A", "B")
uncertain <- uncertain %>%   filter(!(Cls %in% c("BWh", "EF", "BWk") & rank != "A"))

uncertain$class1 <- ifelse(abs(uncertain$un) >= 4, TRUE, FALSE)
summary(uncertain$class1)

# alternative legend
agg_range <- uncertain %>% 
  group_by(un) %>%
  summarize(n_range = n())
agg_range

alt_legend <- ggplot(data = agg_range, 
                     aes(y = n_range, x = as_factor(un), fill = as_factor(un))) + 
  geom_bar(stat = 'identity', width = 0.5) + 
  scale_fill_manual(values = c("#1A184A", "#3563AD", "#4CC6E1", "#A1DBE2", "grey60",
                               "#F4F68B", "#FFCA05", "#F07599","#E72521")) + 
  scale_y_continuous("", expand = c(0, 0), breaks = seq(0, 10000, 5000), 
                     limits = c(0, 12000), labels = c("0", "5k", "10k")) + 
  scale_x_discrete(limits = rev(levels(as_factor(agg_range$un))), 
                   labels = c("8/8", "7/9", "6/8", "5/8", "4/8", "5/8", "6/8", "7/8", "8/8")) + 
  coord_flip() + 
  theme(plot.margin = margin(t = 0, l = 0, b = 0, r = 0, "cm"),
        plot.title = element_text(family = "Times New Roman", size = 4, 
                                  margin = margin(b = -0.1, unit = "cm")), 
        plot.subtitle = element_text(family = "Times New Roman", size = 4), 
        axis.text.y = element_text(family = "Times New Roman", color = "#22211d", size = 4, face = "italic"), 
        axis.text.x = element_text(family = "Times New Roman", color = "#22211d", size = 4, face = "italic",
                                   margin = margin(t = -0.10, unit = "cm")), 
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = seq(0, 10000, 5000), color = "white") #+ 
  #labs(title = expression(bold("Consistency among\nGCMs and indices")))

alt_legend


# main plot
uncertainty_dam <- 
  ggplot() + 
  geom_sf(data = world.sf, color = "black", fill = "#805c4f", size = 0.05) +
  geom_tile(data = uncertain,
            aes(x = lon, y = lat, fill = as_factor(un)), alpha = 0.9) + 
  scale_x_continuous(breaks = ewbrks, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, expand = c(0, 0)) +
  theme_map_short() +
  coord_sf(ylim = c(-64, 90), xlim = c(-180, 180)) +  # , datum = NA
  #coord_equal(ratio = 1, xlim = c(-180, 180)) +
  labs(x = "", y = "", tag = "b") + 
  geom_text(aes(x = -130, y = -59, label = "Consistency among GCMs and indices"), 
            family = "Times New Roman", color = "black", size = 1) +
  theme(legend.justification = c(0, 0), legend.position = "none",
        legend.background = element_rect(fill = "transparent", color = NA, size = NA),
        plot.tag.position = c(.15, .95),
        plot.tag = element_text(family = "Times New Roman", color = "#22211d", size = 10, face = "bold"),
        plot.margin = margin(t = -0.5, b = -1.1, l = -0.85, unit = "cm")) +
  scale_fill_manual(values = c("#1A184A", "#3563AD", "#4CC6E1", "#A1DBE2","grey60",
                               "#F4F68B", "#FFCA05", "#F07599","#E72521"),
                    name = expression(bold("Consistency among/nGCMs and indices")),
                    #labels = c("Upstream of dam(s)", "Downstream of dam(s)"),
                    guide = guide_legend(nrow = 2,
                                         direction = "horizontal",
                                         byrow = TRUE,
                                         override.aes = list(alpha = 1, size =  6),
                                         title.position = "left",
                                         title.hjust = 0,
                                         label.hjust = 0)
  )
uncertainty_dam

legend_grob = ggplotGrob(alt_legend)

final_plot <- uncertainty_dam + annotation_custom(grob = legend_grob, 
                                          xmin = min(global_new_return_rcp_60_nodam_v2$lon) + 5, 
                                          xmax = min(global_new_return_rcp_60_nodam_v2$lon) + 95, 
                                          ymin = -80, 
                                          ymax = 20)

final_plot


ggsave(paste("Figures/global/rcp60/rcp60_nodam_uncertainty_v2.tiff", sep = ""),
       dpi = 600, width = 7, height = 3, units = "cm")


#### RCP6.0: Differences #####
# the files
global_new_return_rcp_60_nodam
global_new_return_rcp_60_dam
# the most recent mask
mask <- 
  read_csv("Inputs/up_middle_downstream.csv", col_types = cols(lon = col_double(),
                                                               lat = col_double(),
                                                               dam = col_integer(),
                                                               nx  = col_integer(),
                                                               ny  = col_integer(),
                                                               L   = col_integer(),
                                                               jx  = col_integer(),
                                                               jy  = col_integer(),
                                                               position = col_integer())
  )
# all data
global_new_return_rcp60 <- global_new_return_rcp_60_nodam %>% left_join(global_new_return_rcp_60_dam, 
                                                                        by = c("L", "GCM"))

## add stream info (down, middle, up, or other)
#global_new_return_rcp60 <- 
#  global_new_return_rcp60 %>% left_join(mask, by = c( "L"))

## try filtering
#global_new_return_rcp60$change_2.x <- ifelse(global_new_return_rcp60$change_2.x > 1500, NA, global_new_return_rcp60$change_2.x)
#global_new_return_rcp60$change_2.y <- ifelse(global_new_return_rcp60$change_2.y > 1500, NA, global_new_return_rcp60$change_2.y)

## differences
global_new_return_rcp60$diff <- global_new_return_rcp60$change.y - global_new_return_rcp60$change.x



#extract sd
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$diff < -100, "]-Inf, -100[", NA)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$diff >= -100 & global_new_return_rcp60$diff <= -75, "[-100, -75[", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$diff >= -75 & global_new_return_rcp60$diff <= -50, "[-75, -50[", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$diff >= -50 & global_new_return_rcp60$diff <= -25, "[-50, -25[", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$diff >= -25 & global_new_return_rcp60$diff < -5, "[-25, -5[", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$diff >= -5 & global_new_return_rcp60$diff <= 5, "[-5, 5]", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$diff > 5 & global_new_return_rcp60$diff <= 25, "]5, 25]", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$diff > 25 & global_new_return_rcp60$diff <= 50, "]25, 50]", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$diff > 50 & global_new_return_rcp60$diff <= 75, "]50, 75]", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$diff > 75 & global_new_return_rcp60$diff < 100, "]75, 100]", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$diff >= 100, "]100, +Inf[", global_new_return_rcp60$diff_range)

global_new_return_rcp60$diff_range <- factor(global_new_return_rcp60$diff_range, ordered = TRUE, 
                                             levels = c("]-Inf, -100[", "[-100, -75[", "[-75, -50[", "[-50, -25[", "[-25, -5[", "[-5, 5]",
                                                        "]5, 25]", "]25, 50]", "]50, 75]", "]75, 100]", "]100, +Inf["))

global_new_return_rcp60 <- global_new_return_rcp60 %>% left_join(uncertain_v2)
global_new_return_rcp60 <- global_new_return_rcp60 %>% left_join(mask)
global_new_return_rcp60 <- global_new_return_rcp60 %>% 
                                  left_join(koppen, by = c("lon" = "Lon", "lat" = "Lat", "L" = "L"))


agg_range_60 <- global_new_return_rcp60 %>% 
  filter(is.na(diff_range) != TRUE, class1 == TRUE, position %in% c(2), 
         !(Cls %in% c("BWh","EF", "BWk") & rank.x != "A")) %>% 
  group_by(diff_range, GCM) %>%
  summarize(n_range = n())
agg_range_60

a11 <- agg_range_60 %>% filter(GCM == "H3C_", diff_range %in% c("]5, 25]", "]25, 50]", "]50, 75]", "]75, 100]", "]100, +Inf["))
a12 <- agg_range_60 %>% filter(GCM == "H3C_",! diff_range %in% c("]5, 25]", "]25, 50]", "]50, 75]", "]75, 100]", "]100, +Inf["))

a21 <- agg_range_60 %>% filter(GCM == "G3C_", diff_range %in% c("]5, 25]", "]25, 50]", "]50, 75]", "]75, 100]", "]100, +Inf["))
a22 <- agg_range_60 %>% filter(GCM == "G3C_", !diff_range %in% c("]5, 25]", "]25, 50]", "]50, 75]", "]75, 100]", "]100, +Inf["))

a31 <- agg_range_60 %>% filter(GCM == "I3C_", diff_range %in% c("]5, 25]", "]25, 50]", "]50, 75]", "]75, 100]", "]100, +Inf["))
a32 <- agg_range_60 %>% filter(GCM == "I3C_", !diff_range %in% c("]5, 25]", "]25, 50]", "]50, 75]", "]75, 100]", "]100, +Inf["))

a41 <- agg_range_60 %>% filter(GCM == "M3C_", diff_range %in% c("]5, 25]", "]25, 50]", "]50, 75]", "]75, 100]", "]100, +Inf["))
a42 <- agg_range_60 %>% filter(GCM == "M3C_", !diff_range %in% c("]5, 25]", "]25, 50]", "]50, 75]", "]75, 100]", "]100, +Inf["))
# the proportion of increase flood
inc1 <- sum(a11$n_range) / (sum(a12$n_range) + sum(a11$n_range))
inc2 <- sum(a21$n_range) / (sum(a22$n_range) + sum(a21$n_range))
inc3 <- sum(a31$n_range) / (sum(a32$n_range) + sum(a31$n_range))
inc4 <- sum(a41$n_range) / (sum(a42$n_range) + sum(a41$n_range))

mean(c(inc1, inc2, inc3, inc4))
sd(c(inc1, inc2, inc3, inc4))

## ensemble...
global_new_return_rcp60 <- global_new_return_rcp60 %>% group_by(L) %>% 
  summarise(ave_diff  = median(diff, na.rm = TRUE),
            ave_nodam = mean(change.x, na.rm = TRUE),
            ave_dam   = mean(change.y, na.rm = TRUE))

global_new_return_rcp60 <- global_new_return_rcp60 %>% left_join(mask, by = "L")
global_new_return_rcp60$rank <- ifelse(global_new_return_rcp60$ave_dam >= 5, "A", "B")

# remove koppen geiger
global_new_return_rcp60 <- global_new_return_rcp60 %>% 
  left_join(koppen, by = c("lon" = "Lon", "lat" = "Lat", "L" = "L"))
# filter 
global_new_return_rcp60 <- global_new_return_rcp60 %>% 
  filter(!(Cls %in% c("BWh","EF", "BWk") & rank != "A"))

## convert to factors
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$ave_diff < -100, "]-Inf, -100[", NA)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$ave_diff >= -100 & global_new_return_rcp60$ave_diff <= -75, "[-100, -75[", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$ave_diff >= -75 & global_new_return_rcp60$ave_diff <= -50, "[-75, -50[", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$ave_diff >= -50 & global_new_return_rcp60$ave_diff <= -25, "[-50, -25[", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$ave_diff >= -25 & global_new_return_rcp60$ave_diff < -5, "[-25, -5[", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$ave_diff >= -5 & global_new_return_rcp60$ave_diff <= 5, "[-5, 5]", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$ave_diff > 5 & global_new_return_rcp60$ave_diff <= 25, "]5, 25]", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$ave_diff > 25 & global_new_return_rcp60$ave_diff <= 50, "]25, 50]", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$ave_diff > 50 & global_new_return_rcp60$ave_diff <= 75, "]50, 75]", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$ave_diff > 75 & global_new_return_rcp60$ave_diff < 100, "]75, 100]", global_new_return_rcp60$diff_range)
global_new_return_rcp60$diff_range <- ifelse(global_new_return_rcp60$ave_diff >= 100, "]100, +Inf[", global_new_return_rcp60$diff_range)

global_new_return_rcp60$diff_range <- factor(global_new_return_rcp60$diff_range, ordered = TRUE, 
                                             levels = c("]-Inf, -100[", "[-100, -75[", "[-75, -50[", "[-50, -25[", "[-25, -5[", "[-5, 5]",
                                                        "]5, 25]", "]25, 50]", "]50, 75]", "]75, 100]", "]100, +Inf["))


global_new_return_rcp60 <- global_new_return_rcp60 %>% left_join(uncertain_v2)

## make map
agg_range <- global_new_return_rcp60 %>% 
  filter(is.na(diff_range) != TRUE, !position %in% c(1, 3), class1 == TRUE) %>% 
  group_by(diff_range) %>%
  summarize(n_range = n())
agg_range

sum(agg_range$n_range[7:11]) / sum(agg_range$n_range)
sum(agg_range$n_range[1:5]) / sum(agg_range$n_range)

# add angles
agg_range$tot_n <- sum(agg_range$n_range)
pie_chart <- 
  agg_range %>% mutate(end_angle = 2 * pi * cumsum(n_range) / tot_n,  # ending angle for each pie slice
                       start_angle = lag(end_angle, default = 0),     # starting angle for each pie slice
                       mid_angle = 0.5 * (start_angle + end_angle))   # middle of each pie slice, for the text label

pie_chart <- mutate(pie_chart,
                    hjust = ifelse(mid_angle>pi, 1, 0),
                    vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1))

rpie = 1 # pie radius
rlabel = 1.2 * rpie # radius of the labels; a number slightly larger than 0.5 seems to work better,
# but 0.5 would place it exactly in the middle as the question asks for.

pie_chart <- pie_chart %>% 
  mutate(focus = ifelse(diff_range %in% c("]100, +Inf["), 0.2, 0)) 

ar <- tibble(x = c(0.9 * sin(pie_chart$mid_angle), 1.2 * sin(pie_chart$mid_angle)),
             y = c(0.9 * cos(pie_chart$mid_angle), 1.2 * cos(pie_chart$mid_angle)),
             gr = rep(1:11, 2))

#ar[1:4,1:2] <- 0 ;  ar[11:14,1:2] <- 0
p <- ggplot(pie_chart) + 
  expand_limits(x = c(-1.4, 1.4), y = c(0, 1.2)) +
  ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.65, r = 0.95, 
                   start = start_angle, end = end_angle, fill = diff_range), 
               show.legend = FALSE, color = "transparent") + 
  scale_fill_manual(values = c("#1A184A", "#3563AD", "#3593D3", "#4CC6E1", "#A1DBE2","grey60",
                               "#F4F68B", "#FFCA05", "#F0691F", "#F07599","#E72521")) +
  # add lines
  geom_path(data = ar %>% filter(!gr %in% c(1, 2, 3, 4)), aes(x = x, y = y, group = gr), colour = "white", size = 1) +
  geom_path(data = ar %>% filter(!gr %in% c(1, 2, 3, 4)), aes(x = x, y = y, group = gr), colour = "black", size = 0.5) +
  # add lables
  geom_label(data = pie_chart %>% filter(!diff_range %in% c("]-Inf, -100[", "[-100, -75[",
                                                            "[-75, -50[", "[-50, -25[")),
              aes(x = rlabel*sin(mid_angle), 
                 y = rlabel*cos(mid_angle), 
                 label = diff_range),
             #hjust = hjust, 
             #vjust = vjust),
             size = 1.5 , family = "Times New Roman",
             color = "black", 
             label.padding = unit(0.10, "lines"),
             position = "identity") + 
  #
  geom_text(aes(x = 0, y = 0), 
            label = "Historical 100-y flood\nfrequency decreases\nfor 60.8% of grid cells",
            size = 1.5 , family = "Times New Roman") +
  
  coord_fixed() +
  theme_void() 

p


## the plot
diff_rcp60_map <- 
ggplot() + 
  geom_sf(data = world.sf, color = "black", fill = "#805c4f", size = 0.05) +
  geom_tile(data = global_new_return_rcp60 %>% filter(is.na(diff_range) != TRUE, 
                                                      !position %in% c(1, 3)),
            aes(x = lon, y = lat, fill = diff_range), alpha = 0.9) + 
  geom_tile(data = global_new_return_rcp60 %>% filter(is.na(diff_range) != TRUE, position %in% c(3)),
            aes(x = lon, y = lat),
            fill = "black", alpha = 0.5) +
  scale_x_continuous(breaks = ewbrks, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, expand = c(0, 0)) +
  theme_map2() +
  coord_sf(ylim = c(-60, 90), xlim = c(-180, 180)) +  # , datum = NA
  #coord_equal(ratio = 1, xlim = c(-180, 180)) +
  labs(x = "", y = "") + # , tag = "c" 
  theme(legend.justification = c(0, 0), legend.position = "none",  #legend.position = c(0, -0.20),
        legend.background = element_rect(fill = "transparent", color = NA, size = NA),
        plot.tag.position = c(.05, .95),
        plot.tag = element_text(family = "Times New Roman", color = "#22211d", size = 10, face = "bold")) +
  scale_fill_manual(values = c("#1A184A", "#3563AD", "#3593D3", "#4CC6E1", "#A1DBE2","grey60",
                               "#F4F68B", "#FFCA05", "#F0691F", "#F07599","#E72521"),
                    name = expression(bold("Return period")~"[years]"),
                    #labels = c("Upstream of dam(s)", "Downstream of dam(s)"),
                    guide = guide_legend(nrow = 2,
                                         direction = "horizontal",
                                         byrow = TRUE,
                                         override.aes = list(alpha = 1, size =  6),
                                         title.position = "left",
                                         title.hjust = 0,
                                         label.hjust = 0)
  )

legend_grob = ggplotGrob(p)
final_plot <- diff_rcp60_map + annotation_custom(grob = legend_grob,
                                                 xmin = min(global_new_return_rcp60$lon) + 0, 
                                                 xmax = min(global_new_return_rcp60$lon) + 105, 
                                                 ymin = -70, 
                                                 ymax = 30)

final_plot

ggsave(paste("Figures/global/rcp60/rcp60_diff_return_period_rev_max_v3.tiff", sep = ""),
       dpi = 600, width = 14, height = 6, units = "cm")




## test
a1<- ggplotGrob(nodam)
a2<- ggplotGrob(nodam)
a3<- ggplotGrob(final_plot)
a <- gridExtra::arrangeGrob(a1, a2, a3, layout_matrix = rbind(c(1,2),c(3,3)))
a
ggsave(paste("Figures/global/rcp60/combined_rcp60.tiff", sep = ""), a,
       dpi = 600, width = 16, height = 16, units = "cm")


#### histogram #####
global_new_return_rcp_60_nodam_v2
global_new_return_rcp_60_dam_v2
global_file <- global_new_return_rcp_60_nodam_v2
global_file$exp <- "nodam"
global_new_return_rcp_60_dam_v2$exp <- "dam"
global_file <- rbind(global_file, global_new_return_rcp_60_dam_v2)

agg_range <- global_file %>% filter(new_range != "NA") %>% 
  group_by(new_range, exp) %>%
  summarize(n_range = n())

agg_range

theme_graph <- function(...) {
  theme_bw() +
    theme(
      plot.title = element_text(family = "Times New Roman", hjust = 0.5),
      legend.position = c(0.8, 0.70),
      legend.justification = c(0, 0),
      plot.margin = margin(t = 0.1, r = 0.1, b = -0.1, l = 0.1, unit = "cm"),
      legend.box.spacing = unit(c(0,0,0,0), "cm"),
      legend.box = "vertical",
      legend.margin = margin(t = 0, r = 0, b = -0.1, l = 0, unit = "cm"),
      legend.key = element_blank(),
      legend.box.margin = margin(t = 0, r = 0, b = -0.1, l = 0, unit = "cm"),
      legend.spacing.y = unit(-0.025, "cm"),
      legend.background = element_rect(fill = "transparent", color = NA, size = NA),
      legend.title = element_text(family = "Times New Roman", size = 10, face = "bold"),
      legend.text = element_text(family = "Times New Roman", size = 8),
      axis.title = element_text(family = "Times New Roman", color = "black", size = 10),
      axis.text = element_text(family = "Times New Roman", color = "black", size = 10),
      strip.text.x = element_text(family = "Times New Roman", 
                                  size = 10, colour = "black", face = "bold"),
      strip.background = element_rect(fill = NA, color = NA),
      panel.border = element_rect(), #element_blank(),
      axis.line.x = element_blank(), #element_line(color = "black", size = 1),
      axis.line.y = element_blank(), #element_line(color = "black", size = 1),
      ...
    )
}



hist_comp <- ggplot(data = agg_range, 
                    aes(y = n_range, x = new_range, group = exp, fill = new_range, color = exp)) + 
  geom_bar(stat = 'identity', width = 0.5, position = "dodge", show.legend = FALSE) + 
  theme_graph() + 
  scale_fill_manual(expression(bold("Change")~"[years]"),
                    values = c("#1A184A", "#3563AD", "#3593D3", "#4CC6E1", "#A1DBE2", "grey60",
                               "#F4F68B", "#FFCA05", "#F0691F", "#F07599","#E72521"), 
                    guide = guide_legend(ncol = 2)) + 
  scale_y_continuous(expression(bold("Count")~"[-]"),
                     expand = c(0, 0), breaks = seq(0, 10000, 5000), 
                     limits = c(0, 12000), labels = c("0", "5k","10k")) +
  scale_x_discrete("") +
  # colors
  scale_color_manual(expression(bold("Simulation")), values = c("black", "red"),
                     guide = guide_legend(ncol = 2))

hist_comp


ggsave("Figures/global/rcp26/histogram_rev_max_v1.tiff",
       dpi = 600, width = 17, height = 8, units = "cm")



##### shift due to choice of indicators ######

indicators_rcp26_map <- 
  ggplot(data = c %>% filter(is.na(change) != TRUE),
         aes(x = lon, y = lat, fill = change)) + 
  geom_polygon(data = world2, 
               aes(x = long, y = lat, group = group),
               color = "black", fill = "#7f7f7f", size = 0.05) +
  geom_tile(alpha = 0.9) + 
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  theme_map() +
  coord_equal(ratio = 1, xlim = c(-180, 180)) +
  labs(x = "", y = "") + 
  theme(legend.justification = c(0, 0), legend.position = c(0.01, 0.01),  #legend.position = c(0, -0.20),
        legend.background = element_rect(fill=scales::alpha('white', 0.4),  colour = NA)) +
  scale_fill_distiller(expression(bold("Shift in flood mitigation")~"[years]"), palette = "RdYlBu",
                       breaks = c(-200,  -150, -100, -50, 0, 50, 100, 150, 200),
                       limits = c(-200, 200),
                       oob = scales::squish,
                       guide = guide_colorbar(direction = "horizontal",
                                              ticks.colour = "black",
                                              frame.colour=c("black"),
                                              discrete = TRUE,
                                              barheight = unit(1.5, units = "mm"),
                                              barwidth = unit(100, units = "mm"),
                                              draw.ulim = TRUE,
                                              title.position = "top",
                                              # some shifting around
                                              title.hjust = 0,
                                              label.hjust = 0)
  )

indicators_rcp26_map

ggsave("Figures/quantile_max_rcp26.tiff", dpi = 600,
       width = 16, height = 8, units = "cm")
