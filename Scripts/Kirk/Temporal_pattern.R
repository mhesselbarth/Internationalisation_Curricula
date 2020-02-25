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

#### Pool data ####
# pool data
mounds_total_94_95_ppp <- spatstat::superimpose(mounds_new_94_ppp, 
                                                mounds_new_95_ppp)

#### Disturbance during year ####

# convert total dataset to df
mounds_total_94_95_df <- tibble::as_tibble(mounds_total_94_95_ppp)


census_dates <- as.Date(c("1994-05-23", "1994-06-03", "1994-06-10", "1994-06-17",
                          "1994-06-21", "1994-07-01", "1994-07-08", "1994-07-17",
                          "1994-07-21", "1994-07-29", "1994-08-05", "1994-09-17",
                          "1995-03-26", "1995-04-19", "1995-04-26", "1995-05-11", 
                          "1995-05-18", "1995-05-25", "1995-06-01", "1995-06-08",
                          "1995-06-15", "1995-06-22", "1995-06-29", "1995-07-06", 
                          "1995-07-13", "1995-07-20", "1995-07-27", "1995-08-03"))

time_in_census <- as.integer(census_dates - dplyr::lag(census_dates, n = 1))

new_mounds <- as.integer(table(cut(mounds_total_94_95_df$marks, 
                                   breaks = c(census_dates, as.Date("1995-08-10")))))

mounds_weekly_94_95_df <- tibble::tibble(census_dates = census_dates, 
                                         time_in_census = time_in_census) %>% 
  dplyr::mutate(new_mounds = new_mounds, 
                production_rate = new_mounds / time_in_census) %>% 
  tidyr::replace_na(replace = list(time_in_census = 0, 
                                   production_rate = 0)) %>% 
  dplyr::filter(census_dates != "1994-05-23")

ggplot(data = mounds_weekly_94_95_df) + 
  geom_line(aes(x = census_dates, y = production_rate)) + 
  geom_point(aes(x = census_dates, y = production_rate), pch = 1, size = 2) + 
  scale_x_date(breaks = seq(from = as.Date("1994-05-01"),
                            to = as.Date("1995-08-01"), by = "3 months"),
               limits = c(as.Date("1994-05-01"), as.Date("1995-08-03"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  labs(x = "Census date", y = "Average Mound Production \n(mounds/plot/day") +
  theme_classic()

####  Average L-function each week ####
