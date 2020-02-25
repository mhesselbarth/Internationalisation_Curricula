###################################################
##    Author: Maximilian H.K. Hesselbarth        ##
##    Department of Ecosystem Modelling          ##
##    University of Goettingen                   ##
##    maximilian.hesselbarth@uni-goettingen.de   ##
##    www.github.com/mhesselbarth                ##
###################################################

#### Load libraries ####
library(onpoint)
library(spatstat)
library(tidyverse)

#### Import data ####
feeces_df <- read_delim(file = "Data/Jose/feces_dataset_quantitative.txt",
                        delim = "\t")

obs_window <- ripras(x = feeces_df$x, y = feeces_df$y, shape = "rectangle")

feeces_ppp <- ppp(x = feeces_df$x, y = feeces_df$y, 
                  marks = data.frame(mark_1 = feeces_df$mark_1,
                                     mark_2 = feeces_df$mark_2),
                  window = obs_window)

plot(feeces_ppp)

#### Observed data ####
mccf_12 <- markcrosscorr(X = feeces_ppp, correction = "Ripley", 
                         r = 0:500)[[1]][[2]]

# plot(mccf_12)

#### Random labeling ####
random_labeling_ppp <- rlabel(feeces_ppp, nsim = 199)

#### Construct envelopes ####
envelope_mccf_12 <- purrr::map_dfr(random_labeling_ppp, function(x) {
  
  result <- markcrosscorr(X = x, correction = "Ripley", 
                          r = 0:500)
  
  as.data.frame(result$fns[[2]])
}, .id = "nsim") %>%
  group_by(r) %>%
  summarise(lo = quantile(iso, 0.025), 
            hi = quantile(iso, 0.9725)) %>%
  mutate(theo = 1)

#### Plot results #### 
ggplot(data = envelope_mccf_12) + 
  geom_ribbon(aes(x = r, ymin = lo, ymax = hi), fill = "grey") + 
  geom_line(aes(x = r, y = theo), col = "red", linetype = 2) + 
  geom_line(data = mccf_12, aes(x = r, y = iso)) + 
  scale_x_continuous(limits = c(0, 500))
