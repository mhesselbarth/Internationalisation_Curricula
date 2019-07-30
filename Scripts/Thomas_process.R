#### Load libraries ####

library(onpoint)
library(patchwork)
library(spatstat)
library(tidyverse)

#### Import data ####
feeces_df <- read_delim(file = "data_Fedriani_Wiegand_2014/feeces_dataset.txt",
                        delim = "\t")

feeces_df_wb <- filter(feeces_df, 
                       mark != "badger")

plot_coords_b <- read_delim(file = "data_Fedriani_Wiegand_2014/plot_coords_a.txt", 
                            delim = "\t")

obs_window <- owin(poly = list(x = plot_coords_b$x, y = plot_coords_b$y))

feeces_ppp <- ppp(x = feeces_df$x, y = feeces_df$y, marks = feeces_df$mark,
                 window = obs_window)

feeces_ppp_wb <- ppp(x = feeces_df_wb$x,
                     y = feeces_df_wb$y,
                     marks = feeces_df_wb$mark,
                     window = obs_window)

plot(feeces_ppp)
plot(feeces_ppp_wb)

#### With badgers ####

#### Fit Thomas process ####
fitted_thomas <- kppm(X = unmark(feeces_ppp), 
                      statistic = "pcf", statsargs = list(divisor = "d", 
                                                          correction = "Ripley"),
                      cluster = "Thomas", method = "mincon")

simulated_thomas <- simulate.kppm(object = fitted_thomas, nsim = 199, 
                                  window = obs_window)

#### Construct envelopes ####
envelope_pcf_thomas <- envelope(Y = unmark(feeces_ppp), 
                                fun = pcf, 
                                nrank = 5, 
                                simulate = simulated_thomas,
                                funargs = list(divisor = "d", correction = "Ripley"),
                                nsim = 199)

envelope_nnd_thomas <- envelope(Y = unmark(feeces_ppp), 
                                fun = Gest, 
                                nrank = 5, 
                                simulate = simulated_thomas,
                                funargs = list(correction = "km"),
                                nsim = 199)

envelope_epf_thomas <- envelope(Y = unmark(feeces_ppp), 
                                fun = Fest, 
                                nrank = 5, 
                                simulate = simulated_thomas,
                                funargs = list(correction = "km"),
                                nsim = 199)

result_df_thomas <- rbind(cbind(as.data.frame(envelope_pcf_thomas), 
                                fun = "Pair-correlation function", data = "with badgers"),
                          cbind(as.data.frame(envelope_nnd_thomas), 
                                fun = "Nearest-neighbor distribution function", data = "with badgers"),
                          cbind(as.data.frame(envelope_epf_thomas), 
                                fun = "Empty space function", data = "with badgers"))

#### Without badgers ####

#### Fit Thomas process ####
fitted_thomas_wb <- kppm(X = unmark(feeces_ppp_wb), 
                         statistic = "pcf", statsargs = list(divisor = "d", 
                                                             correction = "Ripley"),
                         cluster = "Thomas", method = "mincon")

simulated_thomas_wb <- simulate.kppm(object = fitted_thomas_wb, nsim = 199, 
                                                  window = obs_window)

#### Construct envelopes ####
envelope_pcf_thomas_wb <- envelope(Y = unmark(feeces_ppp_wb), 
                                   fun = pcf, 
                                   nrank = 5, 
                                   simulate = simulated_thomas,
                                   funargs = list(divisor = "d",
                                                  correction = "Ripley"),
                                   nsim = 199)

envelope_nnd_thomas_wb <- envelope(Y = unmark(feeces_ppp_wb), 
                                   fun = Gest,
                                   nrank = 5, 
                                   simulate = simulated_thomas,
                                   funargs = list(correction = "km"),
                                   nsim = 199)

envelope_epf_thomas_wb <- envelope(Y = unmark(feeces_ppp_wb), 
                                   fun = Fest, 
                                   nrank = 5, 
                                   simulate = simulated_thomas,
                                   funargs = list(correction = "km"),
                                   nsim = 199)

result_df_thomas_wb <- rbind(cbind(as.data.frame(envelope_pcf_thomas_wb), 
                                   fun = "Pair-correlation function", data = "w/o badgers"),
                             cbind(as.data.frame(envelope_nnd_thomas), 
                                   fun = "Nearest-neighbor distribution function", data = "w/o badgers"),
                             cbind(as.data.frame(envelope_epf_thomas), 
                                   fun = "Empty space function", data = "w/o badgers"))

#### Plot results ####
result_df_thomas_full <- rbind(result_df_thomas, 
                               result_df_thomas_wb)

# names(result_df_thomas_full)[c(2, 3)] <- c("Observed", "Theoretical")

ggplot(data = result_df_thomas_full) + 
  geom_ribbon(aes(x = r, ymin = lo, ymax = hi), fill = "grey") +
  geom_line(aes(x = r, y = mmean, col = "Theoretical"), linetype = 2) +
  geom_line(aes(x = r, y = obs, col = "Observed")) + 
  facet_wrap(~ fun + data, scales = "free", ncol = 2, nrow = 3) + 
  scale_color_manual(name = "", 
                     values = c(Theoretical = "black", Observed = "red")) + 
  labs(x = "r [m]", y = "Function value f(r)")
