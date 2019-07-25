library(NLMR)
library(raster)
library(shar)
library(tidyverse)


#### Continious data ####

set.seed(42)

n_col <- 25
n_row <- 25

raster_cont <- nlm_fbm(ncol = n_col, nrow = n_row)

plot_raster_cont <- ggplot(data = as.data.frame(raster_cont, xy = TRUE)) + 
  geom_raster(aes(x = x, y = y, fill = layer)) + 
  scale_fill_viridis_c("Soil\ncharacteristics") + 
  coord_equal() + 
  theme_void()

ggsave(filename = "plot_raster_cont.png", 
       plot = plot_raster_cont, 
       dpi = 300, 
       height = 10, width = 10, units = "cm")

#### Discrete data ####

set.seed(42)

n_col <- 25
n_row <- 25

raster_disc <- nlm_fbm(ncol = n_col, nrow = n_row, fract_dim = 1.15) %>%
  classify_habitats(classes = 3) %>%
  as.data.frame( xy = TRUE) %>%
  dplyr::mutate(layer = as.factor(layer))

plot_raster_disc <- ggplot(data = raster_disc) + 
  geom_raster(aes(x = x, y = y, fill = layer)) + 
  scale_fill_viridis_d("Land use data") + 
  coord_equal() + 
  theme_void()

ggsave(filename = "plot_raster_disc.png", 
       plot = plot_raster_disc, 
       dpi = 300, 
       height = 10, width = 10, units = "cm")
