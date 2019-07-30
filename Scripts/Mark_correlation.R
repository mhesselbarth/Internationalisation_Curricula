#### Load libraries ####
library(onpoint)
library(patchwork)
library(spatstat)
library(tidyverse)

#### Import data ####
feeces_df <- read_delim(file = "Data_Fedriani_Wiegand_2014/feeces_dataset_quantitative.txt",
                        delim = "\t")


obs_window <- ripras(x = feeces_df$x, y = feeces_df$y, shape = "rectangle")

feeces_ppp <- ppp(x = feeces_df$x, y = feeces_df$y,
                  window = obs_window)

plot(feeces_ppp)
