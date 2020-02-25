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

# create ppp old mounds
mounds_old_94_ppp <- spatstat::ppp(x = mounds_old_94$X, y = mounds_old_94$Y, 
                                   window = study_plot)

# add data as mark
spatstat::marks(mounds_old_94_ppp) <- mounds_old_94$Date[spatstat::inside.owin(x = mounds_old_94$X, 
                                                                               y = mounds_old_94$Y, 
                                                                               w = study_plot)]

# create ppp new mounds
mounds_new_94_ppp <- spatstat::ppp(x = mounds_new_94$X, y = mounds_new_94$Y, 
                                   window = study_plot)

# add data as mark
spatstat::marks(mounds_new_94_ppp) <- mounds_new_94$Date[spatstat::inside.owin(x = mounds_new_94$X, 
                                                                               y = mounds_new_94$Y, 
                                                                               w = study_plot)]

# create ppp total mounds
mounds_total_94_ppp <- spatstat::superimpose(mounds_old_94_ppp, mounds_new_94_ppp)

#### 1995 ####

# read data 1995 #
data_95 <- readr::read_delim("Data/Kirk/Mounds_95.csv", 
                             delim  = ",", col_names = TRUE) %>% 
  dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
  dplyr::arrange(Type, Date)

# check different data types
table(data_95$Type, useNA = "ifany")

mounds_new_95 <- dplyr::filter(data_95, Type == "New")

# create ppp new mounds
mounds_new_95_ppp <- spatstat::ppp(x = mounds_new_95$X, y = mounds_new_95$Y, 
                                   window = study_plot)

# add data as mark
spatstat::marks(mounds_new_95_ppp) <- mounds_new_95$Date[spatstat::inside.owin(x = mounds_new_95$X, 
                                                                               y = mounds_new_95$Y, 
                                                                               w = study_plot)]

#### Pooled L-function ####

# pool data
mounds_total_94_95_ppp <- spatstat::superimpose(mounds_new_94_ppp, 
                                                mounds_new_95_ppp)

# new observation window w/o area with no mounds
study_plot_clipped <- tibble::tibble(x = c(0, 78, 76, 57, 34, 10, 0),
                                     y = c(0, 0 , 17, 67, 77, 80, 80))

# replace window
mounds_total_94_95_ppp$window <-  spatstat::owin(poly = study_plot_clipped)

envelopes_l_fun <- spatstat::envelope(Y = mounds_total_94_95_ppp, 
                                      fun = onpoint::center_l_function, 
                                      funargs = list(r = seq(from  = 0, to = 35, length.out = 525), 
                                                     correction = "Ripley"), 
                                      nsim = 199, nrank = 5)


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
