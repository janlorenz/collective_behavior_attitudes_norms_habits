library(tidyverse)

# Load raw data
data <- read_csv("data/compliance experiment-overview-table_01.csv", skip = 6) |> select(-`hide-links`) |> 
 bind_rows(read_csv("data/compliance experiment-overview-table_02.csv", skip = 6) |> 
            mutate(`[run number]` = `[run number]` + 35280))
# rename variables and compute expected fraction masked when past behavior and conformity
# do not matter
d <- data |> select(
  id = `[run number]`, step = `[step]`, 
  fraction_masked = `fraction-masked`,
  attitude_bias = `attitude-bias`, beta_attitude = `beta-attitude`, 
  beta_pastbehavior = `beta-pastbehavior`, beta_conformity = `beta-conformity`, 
  frac_random_contacts = `frac-random-contacts`, 
  timeline, 
  memory_capacity = `memory-capacity`, 
  avg_contacts = `avg-contacts`, N) |> 
  mutate(expected_fraction_masked = pnorm(attitude_bias, mean = 0, sd = sqrt(1 + beta_attitude^2)))

# Check full factorial design
factorial_vars <- 
  c("attitude_bias", "beta_attitude", "beta_pastbehavior", "beta_conformity",
    "frac_random_contacts", "timeline", "memory_capacity")
d |> filter(step == 100) |> 
  count(across(all_of(factorial_vars))) # |> pull(n) |> unique()
# 8 frac-random-contact (0, 0.2), memory (5, 20) and timeline (day-night, updates-in-place)
# 9 attitudes, bias (0, 0.05, 0.1) and beta (like sd, 0, 0.5, 1) 
# 49 pastbehavior, conformity (seq(0,3,by=0.5))
# --> 3528 configurations

# Summary data frame
ds <- d |> arrange(id,step) |> 
  group_by(across(all_of(factorial_vars)), id, expected_fraction_masked) |> 
  summarize(mean25_fraction_masked = mean(tail(fraction_masked,25)),
            sd25_fraction_masked = sd(tail(fraction_masked,25)),
            mean10_fraction_masked = mean(tail(fraction_masked,10)),
            sd10_fraction_masked = sd(tail(fraction_masked,10)),
            fraction_masked = tail(fraction_masked,1),
            boost_fraction_masked = fraction_masked - tail(expected_fraction_masked,1),
            boost_mean10_fraction_masked = mean10_fraction_masked - tail(expected_fraction_masked,1),
            stab10 = fraction_masked - mean10_fraction_masked,
            stab25 = fraction_masked - mean25_fraction_masked)
ds |> ggplot() + geom_density(aes(stab10)) + geom_density(aes(stab25), color = "blue")
# The variation and inparticular the difference between the blue and black line 
# shows that some trajectories haven't stabilized
# need to communicate this somehow but probably differently



# Show trajectories facetted
trajectories_facets <- function(d, 
                         attitude_bias_ = 0.1, 
                         beta_attitude_ = 0, 
                         beta_pastbehavior_ = c(0.5,1,1.5,2), 
                         beta_conformity_ = c(0.5,1,1.5,2), 
                         frac_random_contacts_ = 0.2, 
                         timeline_ = "updates-in-place", 
                         memory_capacity_ = 5) d |> 
 filter(attitude_bias == attitude_bias_, 
        beta_attitude == beta_attitude_, 
        beta_pastbehavior %in% beta_pastbehavior_, 
        beta_conformity %in% beta_conformity_, 
        frac_random_contacts == frac_random_contacts_, 
        timeline == timeline_, 
        memory_capacity == memory_capacity_) |> arrange(id, step) |> 
 ggplot(aes(step,fraction_masked,  color = factor(id) )) + 
 geom_line() + 
 geom_line(aes(y=expected_fraction_masked), color = "blue") + 
 ylim(c(0,1)) + guides(color = "none") + 
 facet_grid(beta_conformity ~ beta_pastbehavior, 
            labeller = label_both) +
 scale_color_grey() + theme_minimal() +
 labs(caption = paste0("attitude_bias = ", attitude_bias_, 
                       ", beta_attitude = ", beta_attitude_, 
#                       ", beta_pastbehavior = ", beta_pastbehavior_,
#                       ", beta_conformity = ", beta_conformity_, 
                       "\n frac_random_contacts = ", frac_random_contacts_, 
                       ", timeline = ", timeline_, 
                       ", memory_capacity = ", memory_capacity_)) 
# Make figures of trajectories
trajectories_facets(d,                   
                    attitude_bias_ = 0, 
                    beta_attitude_ = 0, 
                    frac_random_contacts_ = 0.2, 
                    timeline_ = "updates-in-place", 
                    memory_capacity_ = 5)
trajectories_facets(d,                   
                    attitude_bias_ = 0.05, 
                    beta_attitude_ = 0, 
                    frac_random_contacts_ = 0.2, 
                    timeline_ = "updates-in-place", 
                    memory_capacity_ = 5)
trajectories_facets(d,                   
                    attitude_bias_ = 0.05, 
                    beta_attitude_ = 0.5, 
                    frac_random_contacts_ = 0.2, 
                    timeline_ = "updates-in-place", 
                    memory_capacity_ = 5)
trajectories_facets(d,                   
                    attitude_bias_ = 0.1, 
                    beta_attitude_ = 1, 
                    frac_random_contacts_ = 0.2, 
                    timeline_ = "updates-in-place", 
                    memory_capacity_ = 5)
trajectories_facets(d,                   
                    attitude_bias_ = 0, 
                    beta_attitude_ = 1, 
                    frac_random_contacts_ = 0.2, 
                    timeline_ = "updates-in-place", 
                    memory_capacity_ = 5)

# Figures about boost effects for bias

# small bias 0.05 (expected masks 0.52), 
# when boosting goes in the other (wrong) direction this goes in negatively in the mean!
plot_for_bias <- function(bias = 0.05) ds |> 
  rename(random_contacts = frac_random_contacts, time = timeline, memory = memory_capacity) |> 
  filter(attitude_bias == bias, beta_attitude <= 1) |> 
  group_by(beta_pastbehavior, beta_conformity, beta_attitude,
           random_contacts, time, memory) |> 
  summarize(boost = mean(boost_mean10_fraction_masked),
            sd_boost = sd(boost_mean10_fraction_masked)) |> 
  ggplot(aes(beta_conformity, beta_pastbehavior, fill = boost)) +
  geom_tile() + geom_point(aes(alpha = sd_boost, size = sd_boost), color = "white") + 
  facet_grid(time + random_contacts ~ memory + beta_attitude, 
             labeller = label_both) + 
  scale_fill_viridis_c() + 
  scale_size_continuous(range = c(0,3)) + 
  scale_alpha(range = c(0,1)) + coord_fixed() +
  labs(title = paste0("attitude_bias = ",bias,", N = 2000, avg_contacts = 15"),
       caption = paste0("The expected fraction-masked (without conformity and consistency) for beta_attitude = 0, 0.5, 1 is ",
         paste(round(pnorm(bias, sd=sqrt(1+c(0,0.5,1)^2)), digits = 3), collapse = ", "), " 
         boost is fraction-masked (after 100 timestep) minus the expected (average of timesteps 91-100, and 10 simulation runs)
         sd_boost is the standard deviation over the 20 runs, when sd_boost larger than boost, boost is likely to become negative")
       )
plot_for_bias(0.05)
plot_for_bias(0.1)

# Draft of a typology
ds |> filter(attitude_bias == 0.05, beta_attitude <= 1) |> 
 group_by(beta_pastbehavior, beta_conformity, beta_attitude,
          frac_random_contacts, timeline, memory_capacity) |> 
 summarize(
  boost = mean(boost_mean10_fraction_masked),
  wrong_boost = mean(boost_mean10_fraction_masked < 0),
  type = case_when(boost > 0.1 & boost <= 0.2 & wrong_boost <= 0.1 ~ "small boost",
                   boost > 0.2 ~ "big boost",
                   boost > 0.1 & wrong_boost > 0.1 ~ "risk of wrong direction",
                   TRUE ~ "no"
  )) |> 
 ggplot(aes(beta_conformity, beta_pastbehavior, fill = type)) +
 geom_tile() + 
 facet_grid(timeline + memory_capacity ~ frac_random_contacts + beta_attitude,
            labeller = label_both) 
