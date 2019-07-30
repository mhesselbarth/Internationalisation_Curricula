library(helpeR)
library(onpoint)
library(patchwork)
library(spatstat)
library(tidyverse)

#### Header ####

set.seed(42)

# set parameters
n <- 100

x_min <- 0 
x_max <- 20

y_min <- 0
y_max <- 5

marks <- 1:5

points_coords <- tibble(x = runif(n = n, min = x_min, max = x_max ), 
                        y = runif(n = n, min = y_min, max = y_max), 
                        mark = as.factor(sample(x = marks, size = n, replace = TRUE)))

point_pattern_example <- ggplot(data = points_coords) + 
  geom_point(aes(x = x, y = y, col = mark), size = 5) + 
  scale_color_viridis_d() + 
  guides(col = FALSE) + 
  theme_void()

save_ggplot(filename = "Figures/point_pattern_example.png",
            plot = point_pattern_example,
            dpi = 300,
            height = 2.5, width = 10, units = "cm")


#### Example point pattern ####

set.seed(42)

# set parameters
n <- 100

x_min <- 0 
x_max <- 100

y_min <- 0
y_max <- 100

marks <- 1:5

points_coords <- tibble(x = runif(n = n, min = x_min, max = x_max), 
                        y = runif(n = n, min = y_min, max = y_max), 
                        mark = runif(n = n, min = 5, max = 85))

point_pattern_size <- ggplot(data = points_coords) + 
  geom_point(aes(x = x, y = y, size = mark), pch = 1) + 
  geom_rect(aes(xmin = x_min, xmax = x_max, 
                ymin = y_min, ymax = y_max), 
            fill = "transparent", col = "black") + 
  scale_size("DBH [cm]") + 
  labs(x = "x coordinate [m]", y = "y coordinate [m]") + 
  coord_equal() + 
  theme_void()

save_ggplot(filename = "Figures/point_pattern_size.png",
            plot = point_pattern_size,
            dpi = 300,
            height = 10, width = 10, units = "cm")

#### Random point pattern ####
set.seed(42)

# set parameters
n <- 100

x_min <- 0 
x_max <- 100

y_min <- 0
y_max <- 100

points_coords <- tibble(x = runif(n = n, min = x_min, max = x_max), 
                        y = runif(n = n, min = y_min, max = y_max))

ppp_random <- ppp(x = points_coords$x, y = points_coords$y, 
                  window = ripras(x = points_coords$x, y = points_coords$y, 
                                  shape = "rectangle"))

ppp_random_ggplot <- ggplot(data = points_coords) + 
  geom_point(aes(x = x, y = y), pch = 19, size = 2.5) + 
  geom_rect(aes(xmin = x_min, xmax = x_max, 
                ymin = y_min, ymax = y_max), 
            fill = "transparent", col = "black") + 
  labs(x = "x coordinate [m]", y = "y coordinate [m]") + 
  coord_equal() + 
  theme_void()

save_ggplot(filename = "Figures/point_pattern_random.png",
            plot = ppp_random_ggplot,
            dpi = 300,
            height = 10, width = 10, units = "cm")

#### Clustered point pattern ####
set.seed(42)

# set parameters
x_min <- 0 
x_max <- 100

y_min <- 0
y_max <- 100

ppp_cluster <- rThomas(kappa = 0.001, scale = 5, mu = 20,
                       win = owin(xrange = c(x_min, x_max), 
                                  yrange = c(y_min, y_max)))

ppp_cluster_ggplot <- ggplot(data = as.data.frame(ppp_cluster)) + 
  geom_point(aes(x = x, y = y), pch = 19, size = 2.5) + 
  geom_rect(aes(xmin = x_min, xmax = x_max, 
                ymin = y_min, ymax = y_max), 
            fill = "transparent", col = "black") +
  labs(x = "x coordinate [m]", y = "y coordinate [m]") + 
  coord_equal() + 
  theme_void()

save_ggplot(filename = "Figures/point_pattern_clustered.png",
            plot = ppp_cluster_ggplot,
            dpi = 300,
            height = 10, width = 10, units = "cm")

#### Regular point pattern ####
set.seed(42)

# set parameters
x_min <- 0 
x_max <- 100

y_min <- 0
y_max <- 100

ppp_regular <- rStrauss(beta = 0.02, gamma = 0.1, R = 7.5,
                        W = owin(xrange = c(x_min, x_max), 
                                 yrange = c(y_min, y_max)))

ppp_regular_ggplot <- ggplot(data = as.data.frame(ppp_regular)) + 
  geom_point(aes(x = x, y = y), pch = 19, size = 2.5) + 
  geom_rect(aes(xmin = x_min, xmax = x_max, 
                ymin = y_min, ymax = y_max), 
            fill = "transparent", col = "black") +
  labs(x = "x coordinate [m]", y = "y coordinate [m]") + 
  coord_equal() + 
  theme_void()

save_ggplot(filename = "Figures/point_pattern_regular.png",
            plot = ppp_regular_ggplot,
            dpi = 300,
            height = 10, width = 10, units = "cm")
     

#### Summary functions #### 

set.seed(42)

# random
pcf_csr <- envelope(Y = ppp_random, fun = pcf, 
                    divisor = "d", nsim = 199, 
                    correction = "Ripley")

pcf_csr_quantums <- plot_quantums(input = pcf_csr, 
                                  legend_position = "none", 
                                  ylab = "g(r)", 
                                  base_size = 5, 
                                  line_size = 0.1)

save_ggplot(filename = "Figures/pcf_random.png",
            plot = pcf_csr_quantums,
            dpi = 300,
            height = 10, width = 10, units = "cm")

# clustered
pcf_cluster <- envelope(Y = ppp_cluster, fun = pcf, 
                        divisor = "d", nsim = 199, 
                        correction = "Ripley")

pcf_cluster_quantums <- plot_quantums(input = pcf_cluster, 
                                      legend_position = "none", 
                                      ylab = "g(r)", 
                                      base_size = 5, 
                                      line_size = 0.1)

save_ggplot(filename = "Figures/pcf_cluster.png",
            plot = pcf_cluster_quantums,
            dpi = 300,
            height = 10, width = 10, units = "cm")

# regular
pcf_regular <- envelope(Y = ppp_regular, fun = pcf, 
                        divisor = "d", nsim = 199, 
                        correction = "Ripley")

pcf_regular_quantums <- plot_quantums(input = pcf_regular, 
                                      legend_position = "none", 
                                      ylab = "g(r)", 
                                      base_size = 5, 
                                      line_size = 0.1)

save_ggplot(filename = "Figures/pcf_regular.png",
            plot = pcf_regular_quantums,
            dpi = 300,
            height = 10, width = 10, units = "cm")

# combine to one plot
pcf_overall_quantums <- pcf_cluster_quantums + 
  pcf_regular_quantums + 
  pcf_csr_quantums

save_ggplot(filename = "Figures/pcf_overall.png",
            plot = pcf_overall_quantums,
            dpi = 300,
            height = 3, width = 8, units = "cm", overwrite = TRUE)

