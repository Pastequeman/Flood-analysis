# population exposure to reiver flood below das
# here using ssp scenario with changing populaton level


## Libraries:
library("tidyverse")
library("RColorBrewer")
library("extrafont") ; loadfonts() # device = "win"



## with position
for (GCMS in c("H2C_", "G2C_", "M2C_", "I2C_","H3C_", "G3C_", "M3C_", "I3C_", "H0C_", "M0C_", "I0C_", "G0C_")) {
  for (SCE in c("dam_trim", "nodam_trim")) {
    if (SCE == "dam_trim" & GCMS %in% c("H0C_", "M0C_", "I0C_", "G0C_")) {
      next
    } else if (GCMS %in% c("H0C_", "M0C_", "I0C_", "G0C_")) {
      temp <- read_csv(paste("./Inputs/global/global_", GCMS, SCE, "/population_exposure_position_ts.csv", 
                             sep = ""), col_types = cols(years    = col_integer(),
                                                         ite      = col_integer(),
                                                         position = col_integer(),
                                                         sum_max  = col_double(),
                                                         sum_qua  = col_double(),
                                                         sum_cmax = col_double(),
                                                         sum_cqua = col_double()
                             )
      )
    } else {
      temp <- read_csv(paste("./Inputs/population/ssp_population/global_", GCMS, SCE, "/population_exposure_position_ts.csv", 
                             sep = ""), col_types = cols(years    = col_integer(),
                                                         ite      = col_integer(),
                                                         position = col_integer(),
                                                         sum_max  = col_double(),
                                                         sum_qua  = col_double(),
                                                         sum_cmax = col_double(),
                                                         sum_cqua = col_double()
                             )
      )
    }
    
    if (GCMS == "H2C_" | GCMS == "G2C_" | GCMS == "M2C_" | GCMS == "I2C_") {
      temp$rcp <- "rcp2.6"
    } else if (GCMS == "H3C_" | GCMS == "G3C_" | GCMS == "M3C_" | GCMS == "I3C_") {
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
rm(temp) ; rm(SCE) ; rm(GCMS)


###### basic stat exposure #####
## global estimate 
temp <- population_exp %>% filter(!years %in% seq(1974, 2089))
temp <- temp %>% group_by(years, position, GCM, exp, rcp) %>% summarise(yrs_ave = mean(sum_max))
temp_v2 <- temp %>% group_by(exp, rcp, years, GCM) %>% summarise(s1 = sum(yrs_ave))
temp_v2 <- temp_v2 %>% group_by(exp, rcp, GCM) %>% summarise(s1 = mean(s1))
temp_v2 %>% group_by(exp, rcp) %>% summarise(s2 = median(s1),
                                             s3 = sd(s1),
                                             s4 = quantile(s1, 0.75),
                                             s5 = quantile(s1, 0.25))


## hist for only last 30 years?
temp <- population_exp %>% filter(position %in% c(2, 4), years %in% seq(2069, 2099) )
temp <- temp %>% group_by(years, position, GCM, exp, rcp) %>% summarise(yrs_ave = mean(sum_max))
temp_v2 <- temp %>% group_by(exp, rcp, years, GCM) %>% summarise(s1 = sum(yrs_ave))
temp_v2 <- temp_v2 %>% group_by(exp, rcp, GCM) %>% summarise(s1 = mean(s1))
temp_v2 %>% group_by(exp, rcp) %>% summarise(s2 = median(s1),
                                             s3 = sd(s1),
                                             s4 = quantile(s1, 0.75),
                                             s5 = quantile(s1, 0.25))





###### plot ####### 
## Theme
theme_graph <- function(...) {
  theme_bw() +
    theme(
      plot.title = element_text(family = "Times New Roman", hjust = 0.5),
      legend.position = c(0.025, 0.70),
      legend.justification = c(0, 0),
      plot.margin = margin(t = 0.1, r = 0.1, b = 0, l = 0.1, unit = "cm"),
      legend.box.spacing = unit(c(0,0,0,0), "cm"),
      legend.box = "vertical",
      legend.margin = margin(t = 0, r = 0, b = -0.1, l = 0, unit = "cm"),
      legend.key = element_blank(),
      legend.box.margin = margin(t = 0, r = 0, b = -0.1, l = 0, unit = "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.spacing.x = unit(0.1, "cm"),
      legend.key.size = unit(0.3, "cm"),
      legend.background = element_rect(fill = "transparent", color = NA, size = NA),
      legend.title = element_text(family = "Times New Roman", size = 6, face = "bold"),
      legend.text = element_text(family = "Times New Roman", size = 6),
      axis.title = element_text(family = "Times New Roman", color = "black", size = 8),
      axis.text = element_text(family = "Times New Roman", color = "black", size = 8),
      strip.text.x = element_text(family = "Times New Roman", 
                                  size = 10, colour = "black", face = "bold"),
      strip.background = element_rect(fill = NA, color = NA),
      panel.border = element_rect(), #element_blank(),
      axis.line.x = element_blank(), #element_line(color = "black", size = 1),
      axis.line.y = element_blank(), #element_line(color = "black", size = 1),
      ...
    )
}



ave_pop <- population_exp %>% filter(position %in% c(2, 4), !years %in% seq(2005, 2069)) %>% 
  group_by(rcp, family, GCM, exp, position) %>%
  summarise(ave_pop = mean(sum_max, na.rm = TRUE))

# sum (add) the two position next
ave_pop <- ave_pop %>% group_by(rcp, family, GCM, exp) %>% summarise(sum_pop = sum(ave_pop, na.rm = TRUE))


main_boxes <- population_exp %>% filter(position %in% c(2, 4), !years %in% seq(2005, 2069))
# (add)
main_boxes_v2 <- main_boxes %>% group_by(years, ite, exp, GCM, rcp) %>% summarise(cum_sum_max = sum(sum_max),
                                                                                  cum_sum_qua = sum(sum_qua))

h <- ggplot(main_boxes_v2)

h + theme_graph() +
  # main
  geom_boxplot(aes(x = as.factor(exp), 
                   y = cum_sum_max / 10^6, fill = as.factor(rcp)),
               outlier.shape = NA) + #  coef = 1e30
  # symbols
  geom_point(data = ave_pop, 
             aes(x = as.factor(exp), y = sum_pop / 10^6, shape = as_factor(family), group = rcp),
             position = position_jitterdodge(0.5)) + 
  # Scale
  scale_x_discrete("", labels = c("Dams", "No dams")) +
  scale_y_continuous("") +
  coord_cartesian(ylim = c(0, 50)) +
  labs(tag = "b") +
  # legend
  scale_fill_manual(expression(bold("Scenarios:")), values = c("grey60", "#6baed6", "#fb6a4a"), 
                    labels = c("historical", "RCP2.6", "RCP6.0"), 
                    guide = guide_legend(ncol = 3, title.position = "top")) +
  scale_shape_manual(expression(bold("GCMs:")), values = c(24, 21, 22, 23),
                     guide = guide_legend(ncol = 2, title.position = "top")) +
  theme(legend.justification = c(0, 0), legend.position = c(0.025, 0.65),
        legend.background = element_rect(fill = "transparent", color = NA, size = NA),
        plot.tag.position = c(.25, .95),
        plot.tag = element_text(family = "Times New Roman", color = "#22211d", size = 8, face = "bold"),
        plot.margin = margin(t = 0.1, r = 0.1, b = -0.4, l = -0.8, unit = "cm"))

ggsave("Figures/population/exposure_downstream_middlesteam_ssp_v1.tiff",
       dpi = 600, width = 5, height = 6, unit = "cm")


#### the time series ####
out_mat <- tibble(    yrs = rep(rep(seq(1861, 2099), each = 100), times = 2 * 2),
                      ite = rep(seq(1, 100), times = length(seq(1861, 2099)) *2 * 2),
                      exp = rep(c("nodam", "dam"), each = length(seq(1861, 2099)) * 100 * 2),
                      rcp = rep(rep(c("rcp2.6", "rcp6.0"), each = length(seq(1861, 2099)) * 100), times = 2),
                      pop = 0)

population_exp_v2 <- population_exp %>% filter(position %in% c(2, 4)) %>% 
  group_by(years, ite, rcp, GCM, exp) %>%
  summarise(cum_exp = sum(sum_max))
# first 100 for 1861 to 2005 will be hist  (what about replacing cum_exp by ite?)
a <- population_exp_v2 %>% filter(GCM == "H0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
b <- population_exp_v2 %>% filter(GCM == "M0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
c <- population_exp_v2 %>% filter(GCM == "I0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
d <- population_exp_v2 %>% filter(GCM == "G0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
out_mat$pop[1:(100 * length(seq(1861, 2005)))] <- (a$cum_exp + b$cum_exp + c$cum_exp + d$cum_exp) / 4

a <- population_exp_v2 %>% filter(GCM == "H2C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
b <- population_exp_v2 %>% filter(GCM == "M2C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
c <- population_exp_v2 %>% filter(GCM == "I2C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
d <- population_exp_v2 %>% filter(GCM == "G2C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
out_mat$pop[(100 * length(seq(1861, 2005))+1):(100 * length(seq(1861, 2005))+100*94)] <- (a$cum_exp + b$cum_exp + c$cum_exp + d$cum_exp) / 4

# no dam ; rcp 6.0
a <- population_exp_v2 %>% filter(GCM == "H0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
b <- population_exp_v2 %>% filter(GCM == "M0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
c <- population_exp_v2 %>% filter(GCM == "I0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
d <- population_exp_v2 %>% filter(GCM == "G0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
out_mat$pop[(100 * length(seq(1861, 2005))+100*94 +1):(100 * length(seq(1861, 2005))+100*94 + 100 * length(seq(1861, 2005)))] <- (a$cum_exp + b$cum_exp + c$cum_exp + d$cum_exp) / 4

a <- population_exp_v2 %>% filter(GCM == "H3C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
b <- population_exp_v2 %>% filter(GCM == "M3C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
c <- population_exp_v2 %>% filter(GCM == "I3C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
d <- population_exp_v2 %>% filter(GCM == "G3C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
out_mat$pop[(100 * length(seq(1861, 2005))+100*94 + 100 * length(seq(1861, 2005))+1):(100 * length(seq(1861, 2005))+2 *100*94 + 100 * length(seq(1861, 2005)))] <- (a$cum_exp + b$cum_exp + c$cum_exp + d$cum_exp) / 4

# dam ; rcp2.6
a <- population_exp_v2 %>% filter(GCM == "H0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
b <- population_exp_v2 %>% filter(GCM == "M0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
c <- population_exp_v2 %>% filter(GCM == "I0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
d <- population_exp_v2 %>% filter(GCM == "G0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
out_mat$pop[(2 * 100 * length(seq(1861, 2005))+2 *100*94 + 1):(3 * 100 * length(seq(1861, 2005))+2 *100*94)] <- (a$cum_exp + b$cum_exp + c$cum_exp + d$cum_exp) / 4


a <- population_exp_v2 %>% filter(GCM == "H2C_", exp == "dam_trim") %>% arrange(years, cum_exp)
b <- population_exp_v2 %>% filter(GCM == "M2C_", exp == "dam_trim") %>% arrange(years, cum_exp)
c <- population_exp_v2 %>% filter(GCM == "I2C_", exp == "dam_trim") %>% arrange(years, cum_exp)
d <- population_exp_v2 %>% filter(GCM == "G2C_", exp == "dam_trim") %>% arrange(years, cum_exp)
out_mat$pop[(3 * 100 * length(seq(1861, 2005))+2 *100*94 + 1):(3 * 100 * length(seq(1861, 2005))+3 *100*94)] <- (a$cum_exp + b$cum_exp + c$cum_exp + d$cum_exp) / 4

# dam ; rcp6.0
a <- population_exp_v2 %>% filter(GCM == "H0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
b <- population_exp_v2 %>% filter(GCM == "M0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
c <- population_exp_v2 %>% filter(GCM == "I0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
d <- population_exp_v2 %>% filter(GCM == "G0C_", exp == "nodam_trim") %>% arrange(years, cum_exp)
out_mat$pop[(3 * 100 * length(seq(1861, 2005))+3 *100*94 + 1):(4 * 100 * length(seq(1861, 2005))+3 *100*94)] <- (a$cum_exp + b$cum_exp + c$cum_exp + d$cum_exp) / 4


a <- population_exp_v2 %>% filter(GCM == "H3C_", exp == "dam_trim") %>% arrange(years, cum_exp)
b <- population_exp_v2 %>% filter(GCM == "M3C_", exp == "dam_trim") %>% arrange(years, cum_exp)
c <- population_exp_v2 %>% filter(GCM == "I3C_", exp == "dam_trim") %>% arrange(years, cum_exp)
d <- population_exp_v2 %>% filter(GCM == "G3C_", exp == "dam_trim") %>% arrange(years, cum_exp)
out_mat$pop[(4 * 100 * length(seq(1861, 2005))+3 *100*94 + 1):(4 * 100 * length(seq(1861, 2005))+4 *100*94)] <- (a$cum_exp + b$cum_exp + c$cum_exp + d$cum_exp) / 4

out_mat
tail(out_mat)

# rolling mean
roling_mean_pop <-
  tibble(years = rep(seq(1861, 2099), times = 100 * 2 * 2),
         exp = rep(rep(c("dam", "nodam"), each = length(seq(1861, 2099)) * 100), times = 2),
         rcp = rep(c("rcp6.0", "rcp2.6"), each = length(seq(1861, 2099)) * 2 * 100),
         ite = rep(rep(seq(1:100), each = length(seq(1861, 2099))), times = 2 * 2),
         pop = 0
  )
for (rcps in c("rcp6.0", "rcp2.6")) {
  for (experiments in c("dam", "nodam")) {
    for (it in 1:100) {
      temp <- out_mat %>% filter(rcp == rcps, exp == experiments, ite == it)
      roling_mean_pop[roling_mean_pop$rcp == rcps & roling_mean_pop$exp == experiments & 
                        roling_mean_pop$ite == it,]$pop <- zoo::rollmean(x = temp$pop, 5, align = "right", fill = NA)
      
    }
    
  }
  
}


# mean values
m_roll <- roling_mean_pop %>% group_by(rcp, exp, years) %>% summarise(m_pop = mean(pop, na.rm = TRUE))



j <- ggplot()

j + theme_graph() +
  # nodam hist
  # no dam rcp2.6
  geom_line(data =  roling_mean_pop %>% filter(rcp %in% c("rcp2.6"),
                                               exp == "nodam", years >= 2006),
            aes(x = years, y = pop / 10^6, color = "a", group = ite), alpha = 0.20
  ) +
  # no dam rcp6.0
  geom_line(data =  roling_mean_pop %>% filter(rcp %in% c("rcp6.0"),
                                               exp == "nodam", years >= 2006),
            aes(x = years, y = pop / 10^6, color = "b", group = ite), alpha = 0.20
  ) +
  # dam rcp2.6
  geom_line(data =  roling_mean_pop %>% filter(rcp %in% c("rcp2.6"),
                                               exp == "dam", years >= 2006),
            aes(x = years, y = pop / 10^6, color = "c", group = ite), alpha = 0.20
  ) +
  # dam rcp6.0
  geom_line(data =  roling_mean_pop %>% filter(rcp %in% c("rcp6.0"),
                                               exp == "dam", years >= 2006),
            aes(x = years, y = pop / 10^6, color = "d", group = ite), alpha = 0.20
  ) +
  ## historical
  geom_line(data =  roling_mean_pop %>% filter(rcp %in% c("rcp6.0"),
                                               exp == "dam", years <= 2006),
            aes(x = years, y = pop / 10^6, color = "e", group = ite), alpha = 0.20
  ) + 
  # mean values
  geom_line(data = m_roll %>% filter(rcp == "rcp2.6", exp == "nodam", years >= 2005), 
            aes(x = years, y = m_pop / 10^6), color = "#08519c", size = 1, alpha = 0.8) +
  geom_line(data = m_roll %>% filter(rcp == "rcp6.0", exp == "nodam", years >= 2005), 
            aes(x = years, y = m_pop / 10^6), color = "#a50f15", size = 1, alpha = 0.8) +
  geom_line(data = m_roll %>% filter(rcp == "rcp2.6", exp == "dam", years >= 2005), 
            aes(x = years, y = m_pop / 10^6), color = "#6baed6", size = 1, alpha = 0.8) +
  geom_line(data = m_roll %>% filter(rcp == "rcp6.0", exp == "dam", years >= 2005), 
            aes(x = years, y = m_pop / 10^6), color = "#fb6a4a", size = 1, alpha = 0.8) +
  
  ## legend
  scale_color_manual("Scenario and GCMs:",
                     values = c("#08519c", "#a50f15", "#6baed6", "#fb6a4a", "grey60"),
                     labels = c("RCP2.6 + No dams", "RCP6.0 + No dams",
                                "RCP2.6 + Dams",   "RCP6.0 + Dams", "Historical"),
                     guide = guide_legend(ncol = 4)) +
  # vertical
  geom_vline(aes(xintercept = 2005), color = "black", size = 1.5) +
  # arrow
  geom_segment(aes(x = 2080, xend = 2099, y = 0, yend = 0), arrow = arrow(length = unit(0.1,"cm"))) + 
  geom_segment(aes(x = 2080, xend = 2069, y = 0, yend = 0), arrow = arrow(length = unit(0.1,"cm"))) + 
  # anotations
  geom_text(aes(x = 1990, y = 30, label = "Historical"), family = "Times New Roman", color = "black", size = 2, fontface  = "italic") + 
  geom_text(aes(x = 2050, y = 30, label = "Future"), family = "Times New Roman", color = "black", size = 2, fontface  = "italic") + 
  geom_text(aes(x = 2085, y = 1, label = "Panel b"), family = "Times New Roman", color = "black", size = 2, fontface  = "italic") + 
  # axis 
  scale_y_continuous(expression(bold("Population exposed to river flood")~"[million]")) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(1950, 2100, by = 25)) +
  coord_cartesian(ylim = c(0, 50), xlim = c(1970, 2100)) + 
  labs(tag = "a") + 
  # theme tweak
  theme(legend.justification = c(0, 0), legend.position = c(0.32, 0.85),
        legend.background = element_rect(fill = "transparent", color = NA, size = NA),
        plot.tag.position = c(.15, .96),
        plot.tag = element_text(family = "Times New Roman", color = "#22211d", size = 8, face = "bold"),
        plot.margin = margin(t = 0.1, r = 0.34, b = -0.4, l = 0.1, unit = "cm"))


ggsave("Figures/population/ts_exposure_downstream_middlesteam_ssp_v1.tiff",
       dpi = 600, width = 10, height = 6, unit = "cm")



##### create stats for the exposed population #######
main_boxes_v2
ave_pop


## for end of the century
ave_pop %>% group_by(rcp, exp) %>% summarise(m1 = mean(sum_pop),
                                             m2 = median(sum_pop),
                                             s1 = sd(sum_pop),
                                             l1 = quantile(sum_pop, 0,25),
                                             h1 = quantile(sum_pop, 0.75))

# over the entire future period
## for end of the century
ave_pop <- population_exp %>% filter(position %in% c(2, 4)) %>% 
  group_by(rcp, family, GCM, exp, position) %>%
  summarise(ave_pop = mean(sum_max, na.rm = TRUE))

ave_pop <- ave_pop %>% group_by(rcp, family, GCM, exp) %>% summarise(sum_pop = sum(ave_pop, na.rm = TRUE))


ave_pop %>% group_by(rcp, exp) %>% summarise(m1 = mean(sum_pop),
                                             m2 = median(sum_pop),
                                             s1 = sd(sum_pop),
                                             l1 = quantile(sum_pop, 0,25),
                                             h1 = quantile(sum_pop, 0.75))

