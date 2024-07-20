library(tidyverse)
library(glue)
library(arrow)
library(patchwork)

## Load and process data
steps <- read_parquet("data/collective_behavior_attitudes_norms_habits_experiment.parquet") |> select(
  filename, id = `[run number]`, step = `[step]`, 
  fraction_behavior = `fraction-behavior turtles`,
  attitude_mean = `attitude-mean`, attitude_sd = `attitude-sd`, 
  beta_pastbehavior = `beta-pastbehavior`, beta_conformity = `beta-conformity`, 
  frac_random_contacts = `frac-random-contacts`, 
  timeline, memory_capacity = `memory-capacity`, 
  avg_contacts = `avg-contacts`, N, unobserved_utility = `unobserved-utility`) |> 
  mutate(expected_fraction_behavior = pnorm(attitude_mean, mean = 0, sd = sqrt(1 + attitude_sd^2)))
runs <- steps |> filter(step == 100) |> mutate(run_id = row_number()) |> 
 select(filename, id, run_id, everything())
steps <- steps |> left_join(runs |> select(filename, id, run_id), by = join_by("filename","id")) |> 
 select(filename, id, run_id, everything())
settings <- runs |> 
 group_by(attitude_mean, attitude_sd, beta_pastbehavior, beta_conformity,
                             frac_random_contacts, timeline, memory_capacity, avg_contacts, 
                             N, unobserved_utility) |> 
 summarize(
  n = n(),
  prevalence_mean = mean(fraction_behavior),
  prevalence_sd = sd(fraction_behavior),
  prevalence_extremity_mean = mean(abs(fraction_behavior-0.5)),
  prevalence_lower25 = sum(fraction_behavior < 0.25)/n(),
  prevalence_higher75 = sum(fraction_behavior > 0.75)/n(),
  prevalence_lower30 = sum(fraction_behavior < 0.3)/n(),
  prevalence_higher70 = sum(fraction_behavior > 0.7)/n(),
  prevalence_lower10 = sum(fraction_behavior < 0.1)/n(),
  prevalence_higher90 = sum(fraction_behavior > 0.9)/n(),
  prevalence_mid2575 = 1 - prevalence_lower25 - prevalence_higher75, 
  prevalence_mid3070 = 1 - prevalence_lower30 - prevalence_higher70, 
  prevalence_mid1090 = 1 - prevalence_lower10 - prevalence_higher90, 
  .groups = "drop") |> 
 select(n, everything())
# Now we have three data frames which different types of observations (rows):
# steps (17.463.421 rows): Each observation is a time step of one of several runs
#        Main variable of interest is fraction_behavior which is the prevalance at that time step
#        Most other variables are the parameters of the run
# runs (172.894 rows): Each observation is a run
#        The variables are the same as in steps but only for the last timesteps t = 100, timesteps start with t = 0
# settings (9021 rows): Each observation is a unique combination of parameters
#        Main variables of interest are prevalence_lower25 and prevalence_higher75 which is the frequency of runs which have a fraction_behavior 
#        lower or higher as 25%/75%. Other measures are computed from the distribution of fraction_behavior in runs. 
#        The variable n shows how many runs very made for this parameter setting. It ranges from 8 to 150.
# Note: Several simulation experiments with various parameter settings were run. The data is merged in the steps data frame. 
#       The file is a results of a mix of exploratory and systematic runs. 
#       Also several experiments were made to fill up the most relevant parameter setting presented in the paper to reach 150 runs per setting. 



## Figures

# Fig. 2: 
# Graphics are produced by NetLogo with the function `make-pub-example-runs`
# Histogram and color code of normally distributed attitudes
ex <- read_csv("example1_run-0.csv", skip=4) |> 
 mutate(breaks = round(u_attitude*4)/4) 
h <- ex$breaks |> hist( seq(-4.5, 4.5, by = 0.25), plot = FALSE)
tibble(mids=h$mids, counts=h$counts) |> 
 ggplot(aes(x = mids, y = counts, fill = mids)) + geom_col(color = "black", width = 0.25, linewidth = 0.25) + 
 scale_fill_gradient2(low = "#D73229", mid = "white", high = "#2CD13B", limits = c(-4,4)) +
 scale_x_continuous(breaks = seq(-4,4,2)) +
 scale_y_continuous(breaks = c()) +
 labs(x = "", y = "") + guides(fill = "none") +
 theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## Fig. 3
# Trajectories
trajectories_plot <-  function(steps,h_x = 1) {
 g1 <- steps |> ggplot(aes(step, fraction_behavior, color = factor(run_id), group = factor(run_id))) + 
  geom_line() + geom_line(aes(y=expected_fraction_behavior), color = "blue") + 
  ylim(c(0,1)) + guides(color = "none") + scale_color_grey() + theme_minimal() + labs( x = "", y = "Prevalence") 
 g2 <- steps |> filter(step == max(step)) |> ggplot(aes(y = fraction_behavior)) + geom_vline(xintercept = 0, color = "gray") +
  geom_histogram(aes(x = after_stat(count / sum(count))), breaks = seq(0,1,by=0.04)) + 
  theme_minimal() + labs(x = "Frequency", y = "") +  ylim(c(0,1)) +
  scale_x_continuous(breaks = c(h_x,0.5,1), labels = scales::percent_format(), limits = c(0,h_x)) + 
  theme(axis.text.y = element_blank())
 g1 + g2 + plot_layout(ncol = 2, widths = c(3,1), guides = "collect")
}
# Baseline configuration and 3 more trajectory plots of prevalence of 150 runs 
steps |> filter(attitude_mean == 0.1, attitude_sd == 1, beta_pastbehavior == 3, beta_conformity == 6, 
                frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15) |> 
 trajectories_plot(h_x = 1)
steps |> filter(attitude_mean == 0.1, attitude_sd == 1, beta_pastbehavior == 6, beta_conformity == 6, 
                frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15) |> 
 trajectories_plot(h_x = 0.2)
steps |> filter(attitude_mean == 0.1, attitude_sd == 1, beta_pastbehavior == 3, beta_conformity == 3, 
                frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15) |> 
 trajectories_plot(h_x = 0.2)
steps |> filter(attitude_mean == 0.1, attitude_sd == 1, beta_pastbehavior == 1.5, beta_conformity == 6, 
                frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15) |> 
 trajectories_plot(h_x = 1)
# Central panel draft
settings |> filter(attitude_mean == 0.1, attitude_sd == 1, 
                   beta_pastbehavior %in% seq(0,8,by=0.5), beta_conformity %in% seq(0,8,by=0.5), 
                   frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15) |> 
 ggplot(aes(beta_conformity, beta_pastbehavior)) + 
 geom_tile(aes(fill = prevalence_higher75)) + 
 geom_point(aes(color = prevalence_lower25)) + 
 coord_fixed() +
 scale_color_gradient(low = "white", high = "#D73229") + 
 scale_fill_gradient(low = "white", high = "#2CD13B") + 
 labs(fill = "Frequency\nprevalance above 75%\nat t=100", 
      color = "Frequency\nprevalance below 25%\nat t=100", 
      x = "𝛽 Conformity", y = "𝛽 Past behavior") + 
 theme_minimal() 


# NOT IN THE PAPER: Explorations
settings |> filter(attitude_mean == 0.1, attitude_sd == 1, frac_random_contacts == 0.2, 
                   timeline == "day-night", memory_capacity == 5, avg_contacts == 15,
                   beta_pastbehavior %in% seq(0,8,by=0.5), beta_conformity %in% seq(0,8,by=0.5)) |>
 ggplot(aes(beta_conformity, beta_pastbehavior, 
            fill = prevalence_lower10)) + geom_tile() + coord_fixed()

settings |> filter(attitude_mean == 0.1, attitude_sd == 1, frac_random_contacts %in% c(0,0.2),
                   timeline == "day-night", memory_capacity == 5, avg_contacts == 15,
                   beta_pastbehavior %in% seq(0,8,by=0.5), beta_conformity %in% seq(0,8,by=0.5)) |>
 ggplot(aes(beta_conformity, beta_pastbehavior, 
            fill = prevalence_higher70)) + geom_tile() + coord_fixed() + facet_grid(~frac_random_contacts)

settings |> filter(attitude_mean == 0.1, attitude_sd == 1, frac_random_contacts %in% c(0),
                   timeline == "day-night", memory_capacity == 5, avg_contacts %in% c(15,50),
                   beta_pastbehavior %in% seq(0,8,by=0.5), beta_conformity %in% seq(0,8,by=0.5)) |>
 ggplot(aes(beta_conformity, beta_pastbehavior, 
            fill = prevalence_higher90)) + geom_tile() + coord_fixed() + facet_grid(~avg_contacts)

settings |> filter(attitude_mean == 0.1, attitude_sd == 1, frac_random_contacts %in% c(0.2),
                   timeline == "updates-in-place", memory_capacity == 5, avg_contacts == 15,
                   beta_pastbehavior %in% seq(0,8,by=0.5), beta_conformity %in% seq(0,8,by=0.5)) |>
 ggplot(aes(beta_conformity, beta_pastbehavior, 
            fill = prevalence_higher90)) + geom_tile() + coord_fixed() + facet_grid(~frac_random_contacts)

settings |> filter(attitude_mean == 0.1, attitude_sd == 1, frac_random_contacts %in% c(0.2),
                   #timeline == "day-night", 
                   memory_capacity == 5, avg_contacts == 15,
                   beta_pastbehavior %in% seq(0,8,by=0.5), beta_conformity %in% seq(0,8,by=0.5)) |>
 ggplot(aes(beta_conformity, beta_pastbehavior, 
            fill = prevalence_mean)) + geom_tile() + coord_fixed() + facet_grid(~timeline)