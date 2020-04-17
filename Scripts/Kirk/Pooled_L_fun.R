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

#### Year 1995 ####

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


#### Pooled L-function ####

# new observation window w/o area with no mounds
study_plot_clipped <- tibble::tibble(x = c(0, 78, 76, 57, 34, 10, 0),
                                     y = c(0, 0 , 17, 67, 77, 80, 80))

# replace window
mounds_new_94_95_ppp$window <-  spatstat::owin(poly = study_plot_clipped)

# plot pattern
plot(mounds_new_94_95_ppp, use.marks = FALSE)

plot(study_plot, add = TRUE, lty = 2)

# l_center <- function(x, ...) {
#   
#   # calculate L function
#   x <- spatstat::Lest(x, ...)
#   
#   # get r value
#   r <- x$r
#   
#   # center L-function by r value
#   l_centered <- spatstat::eval.fv(x - r)
#   
#   return(l_centered)
# }

envelopes_l_fun <- spatstat::envelope(Y = mounds_new_94_95_ppp, 
                                      fun = onpoint::center_l_function, 
                                      funargs = list(r = seq(from  = 0, to = 35, length.out = 525), 
                                                     correction = "Ripley"), 
                                      nsim = 199, nrank = 5)

# plot(envelopes_l_fun, xlim = c(0, 35), ylim = c(-3, 7), main = "")

envelopes_l_fun_gg <- ggplot(data = tibble::as_tibble(envelopes_l_fun)) + 
  geom_ribbon(aes(x = r, ymin = lo, ymax = hi), fill = "grey") +
  geom_line(aes(x = r, y = obs, linetype = "Observed")) +
  geom_line(aes(x = r, y = theo, linetype = "Theoretical")) +
  scale_x_continuous(breaks = seq(from = 0, to = 35, by = 5), 
                     limits = c(0, 35)) +
  scale_y_continuous(breaks = seq(from = -3, to = 7, by = 1), 
                     limits = c(-3, 7)) + 
  scale_linetype_manual(name = "", values = c(1, 2)) +
  labs(x = "Lag (m)", y = expression(paste(L[11], "(h)"))) +
  theme_classic() +
  theme(legend.position = "bottom", 
        legend.key.size = unit(2, "cm"))
