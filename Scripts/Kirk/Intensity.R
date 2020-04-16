###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Load libraries ####
library(onpoint)
library(raster)
library(patchwork)
library(spatstat)
library(tidyverse)

#### Year 1994 ####

# read data 1994 #
data_94 <- readr::read_delim("Data/Kirk/Mounds_94.csv", 
                             delim  = ",", col_names = TRUE) %>% 
  dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
  dplyr::arrange(Type, Date)

# check different data types
table(data_94$Type, useNA = "ifany")

# filter data
mounds_new_94 <- dplyr::filter(data_94, Type == "New")
mounds_old_94 <- dplyr::filter(data_94, Type == "Old")

# create owin # 
# n_grid <- spatstat::ripras(x = stakes$X, y = stakes$Y, shape = "rectangle")
study_plot <- spatstat:::owin(xrange = c(0, 80), yrange = c(0, 80))

# # create ppp old mounds
# mounds_old_94_ppp <- spatstat::ppp(x = mounds_old_94$X, y = mounds_old_94$Y, 
#                                    window = study_plot)
# 
# # add data as mark
# spatstat::marks(mounds_old_94_ppp) <- mounds_old_94$Date[spatstat::inside.owin(x = mounds_old_94$X, 
#                                                                                y = mounds_old_94$Y, 
#                                                                                w = study_plot)]

# create ppp new mounds
mounds_new_94_ppp <- spatstat::ppp(x = mounds_new_94$X, y = mounds_new_94$Y, 
                                   window = study_plot)

# add data as mark
spatstat::marks(mounds_new_94_ppp) <- mounds_new_94$Date[spatstat::inside.owin(x = mounds_new_94$X, 
                                                                            y = mounds_new_94$Y, 
                                                                            w = study_plot)]

# # create ppp total mounds
# mounds_total_94_ppp <- spatstat::superimpose(mounds_old_94_ppp, mounds_new_94_ppp)

# plot mounds
mounds_new_94_gg <- ggplot(data = tibble::as_tibble(mounds_new_94_ppp)) + 
  geom_point(aes(x = x, y = y)) + 
  geom_polygon(data = as_tibble(study_plot), aes(x = x, y = y), size = 1,
               fill = NA, col = "black") +
  coord_equal() + 
  labs(x = "", y = "", title = "N Grid 1994") +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey"), 
        panel.grid.minor = element_line(colour = "grey"))

#### 1995 ####

# read data 1995 #
data_95 <- readr::read_delim("Data/Kirk/Mounds_95.csv", 
                             delim  = ",", col_names = TRUE) %>% 
  dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
  dplyr::arrange(Type, Date)

# check different data types
table(data_95$Type, useNA = "ifany")

# filter data
mounds_new_95 <- dplyr::filter(data_95, Type == "New")

# create ppp new mounds
mounds_new_95_ppp <- spatstat::ppp(x = mounds_new_95$X, y = mounds_new_95$Y, 
                                   window = study_plot)

# add data as mark
spatstat::marks(mounds_new_95_ppp) <- mounds_new_95$Date[spatstat::inside.owin(x = mounds_new_95$X, 
                                                                               y = mounds_new_95$Y, 
                                                                               w = study_plot)]

# plot mounds
mounds_new_95_gg <- ggplot(data = tibble::as_tibble(mounds_new_95_ppp)) + 
  geom_point(aes(x = x, y = y)) + 
  geom_polygon(data = as_tibble(study_plot), aes(x = x, y = y), size = 1,
               fill = NA, col = "black") +
  coord_equal() + 
  labs(x = "", y = "", title = "N Grid 1995") +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey"), 
        panel.grid.minor = element_line(colour = "grey"))

mounds_total_gg <- mounds_new_94_gg + mounds_new_95_gg

#### Intensity over time ####
mounds_new_94_95_ppp <- spatstat::superimpose(mounds_new_94_ppp, mounds_new_95_ppp)

grid_plot <- matrix(data = 0, nrow = 80, ncol = 80) %>% 
  raster::raster(xmn = 0, xmx = 80, 
                 ymn = 0, ymx = 80, 
                 crs = NA)

# convert to dataframe
grid_plot_df <- raster::as.data.frame(grid_plot, xy = TRUE)

# count mounds in 20qm area
production_rate <- purrr::map_dbl(1:nrow(grid_plot_df), function(i) {
  
  # create owin with 20qm around cell center
  owin_temp <-  spatstat::owin(xrange = c(grid_plot_df[i, 1] - sqrt(5), 
                                          grid_plot_df[i, 1] + sqrt(5)), 
                               yrange = c(grid_plot_df[i, 2] - sqrt(5), 
                                          grid_plot_df[i, 2] + sqrt(5))) 
  
  # T/F for points inside
  points_inside_temp <- spatstat::inside.owin(x = mounds_new_94_95_ppp$x, 
                                              y = mounds_new_94_95_ppp$y, 
                                              w = owin_temp)
  
  # count number of trues divided by sample area and time
  rate_temp <- sum(points_inside_temp) / 20 / 2
})

# add value to df
grid_plot_df$layer <- production_rate

# add value to raster
raster::values(grid_plot) <- production_rate

# plot result
production_rate_gg <- ggplot(data = grid_plot_df) + 
  geom_raster(aes(x = x, y = y, fill = layer)) + 
  scale_fill_viridis_c(option = "C", 
                       name = expression(paste("Mounds ", m^{-2}, yr^{-1}))) + 
  coord_equal() + 
  labs(x = "", y = "", title = "N Grid 1994 - 1995") +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey"), 
        panel.grid.minor = element_line(colour = "grey"))
