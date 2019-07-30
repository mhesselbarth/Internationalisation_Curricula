#### Load libraries ####
library(onpoint)
library(patchwork)
library(spatstat)
library(tidyverse)

#### Import data ####
feeces_df <- read_delim(file = "data_Fedriani_Wiegand_2014/feeces_dataset_bivariate.txt",
                        delim = "\t")

feeces_df <- mutate(feeces_df, 
                    mark = as.factor(case_when(mark_1 == 1 & mark_2 == 0 ~ 1, 
                                               mark_1 == 0 & mark_2 == 1 ~ 2)))

plot_coords <- read_delim(file = "data_Fedriani_Wiegand_2014/plot_coords_b.txt", 
                          delim = "\t")

obs_window <- owin(poly = list(x = plot_coords$x, y = plot_coords$y))

feeces_ppp <- ppp(x = feeces_df$x, y = feeces_df$y, marks = feeces_df$mark,
                  window = obs_window)

plot(feeces_ppp)

#### Random labeling
random_labeling_ppp <- rlabel(feeces_ppp, nsim = 199)

#### Construct envelopes mark connection ####
envelope_random_labeling_11 <- envelope(Y = feeces_ppp, fun = markconnect, 
                                        nsim = 199, nrank = 5,
                                        funargs = list(i = 1, j = 1, correction = "Ripley"), 
                                        simulate = random_labeling_ppp)

envelope_random_labeling_12 <- envelope(Y = feeces_ppp, fun = markconnect, 
                                        nsim = 199, nrank = 5,
                                        funargs = list(i = 1, j = 2, correction = "Ripley"), 
                                        simulate = random_labeling_ppp)

####  Construct envelopes difference densities ####
envelope_pcf_1_dot <- envelope(Y = feeces_ppp, fun = pcfdot, 
                               nsim = 199, nrank = 5, 
                               funargs = list(i = 1, 
                                              correction = "Ripley", divisor = "d"), 
                               simulate = random_labeling_ppp)

envelope_pcf_2_dot <- envelope(Y = feeces_ppp, fun = pcfdot, 
                               nsim = 199, nrank = 5, 
                               funargs = list(i = 2, 
                                              correction = "Ripley", divisor = "d"), 
                               simulate = random_labeling_ppp)

envelope_pcf_diff <- eval.fv(envelope_pcf_1_dot - envelope_pcf_2_dot)

#### Plot result ####
result_full <- rbind(cbind(as.data.frame(envelope_random_labeling_11), data = "p11"), 
                     cbind(as.data.frame(envelope_random_labeling_12), data = "p12"), 
                     cbind(as.data.frame(envelope_pcf_diff), data = "g1,1+2 - g2,1+2"))

ggplot(data = result_full) +
  geom_ribbon(aes(x = r, ymin = lo, ymax = hi), fill = "grey") +
  geom_line(aes(x = r, y = mmean, col = "Theoretical"), linetype = 2) +
  geom_line(aes(x = r, y = obs, col = "Observed")) + 
  facet_wrap(~ data, scales = "free") + 
  scale_color_manual(name = "", 
                     values = c(Theoretical = "black", Observed = "red")) + 
  labs(x = "r [m]", y = "Function value f(r)")
