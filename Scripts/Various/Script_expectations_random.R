library(tidyverse)


lambda <- 2.3

area <- seq(from = 0, to = 10)

expec_objects <- area * lambda

plot_expect <- ggplot() + 
  geom_line(aes(x = area, y = expec_objects), size = 0.75) + 
  scale_x_continuous(name = expression(paste("Area [", m^2, "]")), breaks = seq(from = 0, to = 10, by = 2)) + 
  scale_y_continuous(name = "Expected number of objects", breaks = seq(from = 0, to = 25, by = 5)) + 
  theme_classic(base_size = 10)

r <- seq(from = 0, to = 10, length.out = 525)

g_r <- tibble::tibble(r = r, value = 1, method = "g(r)")
o_r <- tibble::tibble(r = r, value = 1 * lambda, method = "O(r)")
k_r <- tibble::tibble(r = r, value =  pi * r ^ 2, method = "K(r)")
l1_r <- tibble::tibble(r = r, value =  r, method = "L1(r)")
l2_r <- tibble::tibble(r = r, value = r - r, method = "L2(r)")

summary_funs <- rbind(g_r, o_r, k_r, l1_r, l2_r) %>% 
  dplyr::mutate(method = forcats::as_factor(method))

summary_funs_label <- dplyr::filter(summary_funs, r == max(r)) %>% 
  dplyr::mutate(r = max(r))

summary_funs_label$r[3] <- 1.75
summary_funs_label$value[3] <- 10


plot_fun <- ggplot(data = summary_funs) + 
  geom_line(aes(x = r, y = value, group = method), size = 0.75) + 
  geom_text(data = summary_funs_label,
            aes(label = method, x = r + 0.75, y = value), size = 3.5) + 
  coord_cartesian(ylim = c(0, 10)) +
  scale_x_continuous(name = "r [m]", breaks = seq(from = 0, to = 10, by = 2)) + 
  scale_y_continuous(name = "f(r)", breaks = seq(from = 0, to = 10, by = 1)) + 
  theme_classic(base_size = 10)

ggsave(plot = plot_fun, 
       filename = "C:/Users/Maximilian/ownCloud/03_Lehre/Internationalisierung_der_Lehre/Kirk/Material/plot_fun.png", 
       dpi = 300, width = 10, height = 7.5, units = "cm")

ggsave(plot = plot_expect, 
       filename = "C:/Users/Maximilian/ownCloud/03_Lehre/Internationalisierung_der_Lehre/Kirk/Material/plot_expect.png", 
       dpi = 300, width = 10, height = 7.5, units = "cm")
