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

# ggsave(filename = "point_pattern_example.png", 
#        plot = point_pattern_example, 
#        dpi = 300, 
#        height = 2.5, width = 10, units = "cm")


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

# ggsave(filename = "point_pattern_size.png", 
#        plot = point_pattern_size, 
#        dpi = 300, 
#        height = 10, width = 10, units = "cm")

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

point_pattern_random <- ggplot(data = points_coords) + 
  geom_point(aes(x = x, y = y), pch = 19, size = 2.5) + 
  geom_rect(aes(xmin = x_min, xmax = x_max, 
                ymin = y_min, ymax = y_max), 
            fill = "transparent", col = "black") + 
  labs(x = "x coordinate [m]", y = "y coordinate [m]") + 
  coord_equal() + 
  theme_void()

# ggsave(filename = "point_pattern_random.png",
#        plot = point_pattern_random,
#        dpi = 300,
#        height = 10, width = 10, units = "cm")

#### Clustered point pattern ####
set.seed(42)

# set parameters
x_min <- 0 
x_max <- 100

y_min <- 0
y_max <- 100

points_cluster <- rThomas(kappa = 0.001, scale = 5, mu = 20, 
                          win = owin(xrange = c(x_min, x_max), 
                                     yrange = c(y_min, y_max)))

point_pattern_clustered <- ggplot(data = as.data.frame(points_cluster)) + 
  geom_point(aes(x = x, y = y), pch = 19, size = 2.5) + 
  geom_rect(aes(xmin = x_min, xmax = x_max, 
                ymin = y_min, ymax = y_max), 
            fill = "transparent", col = "black") +
  labs(x = "x coordinate [m]", y = "y coordinate [m]") + 
  coord_equal() + 
  theme_void()

# ggsave(filename = "point_pattern_clustered.png",
#        plot = point_pattern_clustered,
#        dpi = 300,
#        height = 10, width = 10, units = "cm")

#### Regular point pattern ####
set.seed(42)

# set parameters
x_min <- 0 
x_max <- 100

y_min <- 0
y_max <- 100

points_regular <- rStrauss(beta = 0.02, gamma = 0.1, R = 7.5,
                            W = owin(xrange = c(x_min, x_max), 
                                     yrange = c(y_min, y_max)))

point_pattern_regular <- ggplot(data = as.data.frame(points_regular)) + 
  geom_point(aes(x = x, y = y), pch = 19, size = 2.5) + 
  geom_rect(aes(xmin = x_min, xmax = x_max, 
                ymin = y_min, ymax = y_max), 
            fill = "transparent", col = "black") +
  labs(x = "x coordinate [m]", y = "y coordinate [m]") + 
  coord_equal() + 
  theme_void()

# ggsave(filename = "point_pattern_regular.png",
#        plot = point_pattern_regular,
#        dpi = 300,
#        height = 10, width = 10, units = "cm")
