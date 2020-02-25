# load libraries
library(helpeR)
library(spatstat)
library(tidyverse)

# set seed
set.seed(42)

n_parents <- 20
n_cluster <- 5
cluster_scale <- 5

x_min <-  0
x_max <- 100

y_min <- 0
y_max <- 100

# create random pattern
random_pat <- tibble(x = runif(n = n_parents, min = x_min, max = x_max), 
                     y = runif(n = n_parents, min = y_min, max = y_max))

# plot random pattern
cluster_process_a <- ggplot(data = random_pat) + 
  geom_point(aes(x = x, y = y)) + 
  geom_rect(aes(xmin = x_min, xmax = x_max, 
                ymin = y_min, ymax = y_max), 
            fill = "transparent", col = "black") + 
  coord_equal() + 
  theme_void()

# save parents
save_ggplot(plot = cluster_process_a, 
            file = "Figures/cluster_process_a.png",
            dpi = 300,
            height = 10, width = 10, units = "cm")

# plot random pattern with circles
cluster_process_b <- ggplot(data = random_pat) + 
  geom_point(aes(x = x, y = y), size = 20, pch = 1) + 
  geom_rect(aes(xmin = x_min, xmax = x_max, 
                ymin = y_min, ymax = y_max), 
            fill = "transparent", col = "black") + 
  coord_equal() + 
  guides(size = FALSE) +
  theme_void()

# save cluster circles
save_ggplot(plot = cluster_process_b, 
            file = "Figures/cluster_process_b.png",
            dpi = 300,
            height = 10, width = 10, units = "cm")

# initialse tibble for clusters
random_clus <- tibble::tibble(x = rep(NA, times = n_parents * n_cluster),
                              y = rep(NA, times = n_parents * n_cluster))

# counter for loop
counter <- 1

# loop through all parents and n_cluster times
for (i in 1:n_parents) {
  
  for (j in 1:n_cluster) {
    
    # random x coord
    random_clus[counter, 1] <- random_pat[i, 1] + runif(n = 1, 
                                                        min = -cluster_scale,
                                                        max = cluster_scale)
    
    # random y coord
    random_clus[counter, 2] <- random_pat[i, 2] + runif(n = 1, 
                                                        min = -cluster_scale,
                                                        max = cluster_scale)
    
    # increase counter
    counter <- counter + 1
    
  }
}

# remove all points outside window
random_clus <-  filter(random_clus, 
                       x > x_min & x < x_max, 
                       y > y_min & y < y_max)

# plot clusters
cluster_process_c <- ggplot(data = random_clus) + 
  geom_point(aes(x = x, y = y), pch = 19) + 
  geom_rect(aes(xmin = x_min, xmax = x_max, 
                ymin = y_min, ymax = y_max), 
            fill = "transparent", col = "black") + 
  coord_equal() + 
  guides(size = FALSE) +
  theme_void()

# save clusters
save_ggplot(plot = cluster_process_c, 
            file = "Figures/cluster_process_c.png",
            dpi = 300,
            height = 10, width = 10, units = "cm")


