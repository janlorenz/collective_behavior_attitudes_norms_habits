library(tidyverse)

# Load raw data
data <- read_csv("data/compliance experiment-overview-table_01.csv", skip = 6) |> select(-`hide-links`) |> 
 bind_rows(read_csv("data/compliance experiment-overview-table_02.csv", skip = 6) |> 
            mutate(`[run number]` = `[run number]` + 35280), 
           read_csv("data/compliance experiment-larger-table.csv", skip = 6) |> 
            mutate(`[run number]` = `[run number]` + 70560) |> 
            rename(`attitude-bias` = `attitude-mean`, `beta-attitude` = `attitude-sd`, 
                   `fraction-masked` = `fraction-masked turtles`),
           read_csv("data/compliance experiment-larger-fixed-contacts-table.csv", skip = 6) |> 
            mutate(`[run number]` = `[run number]` + 96570) |> 
            rename(`attitude-bias` = `attitude-mean`, `beta-attitude` = `attitude-sd`, 
                   `fraction-masked` = `fraction-masked turtles`),
           read_csv("data/compliance experiment-larger-detail-table.csv", skip = 6) |> 
            mutate(`[run number]` = `[run number]` + 122580) |> 
            rename(`attitude-bias` = `attitude-mean`, `beta-attitude` = `attitude-sd`, 
                   `fraction-masked` = `fraction-masked turtles`),
           read_csv("data/compliance experiment-baseline-table.csv", skip = 6) |> 
            mutate(`[run number]` = `[run number]` + 127700) |> 
            rename(`attitude-bias` = `attitude-mean`, `beta-attitude` = `attitude-sd`, 
                   `fraction-masked` = `fraction-behavior turtles`),
           read_csv("data/compliance experiment-baseline-surrounding-table.csv", skip = 6) |> 
            mutate(`[run number]` = `[run number]` + 128900) |> 
            rename(`attitude-bias` = `attitude-mean`, `beta-attitude` = `attitude-sd`, 
                   `fraction-masked` = `fraction-behavior turtles`),
           read_csv("data/compliance experiment-baseline-surrounding_fill1-table.csv", skip = 6) |> 
            mutate(`[run number]` = `[run number]` + 134680) |> 
            rename(`attitude-bias` = `attitude-mean`, `beta-attitude` = `attitude-sd`, 
                   `fraction-masked` = `fraction-behavior turtles`),
           read_csv("data/compliance experiment-baseline-surrounding_fill4-table.csv", skip = 6) |> 
            mutate(`[run number]` = `[run number]` + 135940) |> 
            rename(`attitude-bias` = `attitude-mean`, `beta-attitude` = `attitude-sd`, 
                   `fraction-masked` = `fraction-behavior turtles`),
           read_csv("data/compliance experiment-baseline-surrounding_fill2-table.csv", skip = 6) |> 
            mutate(`[run number]` = `[run number]` + 136060) |> 
            rename(`attitude-bias` = `attitude-mean`, `beta-attitude` = `attitude-sd`, 
                   `fraction-masked` = `fraction-behavior turtles`),
           read_csv("data/compliance experiment-baseline-surrounding_fill3-table.csv", skip = 6) |> 
            mutate(`[run number]` = `[run number]` + 136780) |> 
            rename(`attitude-bias` = `attitude-mean`, `beta-attitude` = `attitude-sd`, 
                   `fraction-masked` = `fraction-behavior turtles`),
           read_csv("data/compliance experiment-baseline-surrounding_fill5-table.csv", skip = 6) |> 
            mutate(`[run number]` = `[run number]` + 137380) |> 
            rename(`attitude-bias` = `attitude-mean`, `beta-attitude` = `attitude-sd`, 
                   `fraction-masked` = `fraction-behavior turtles`),
           read_csv("data/compliance experiment-baseline-surrounding_fill6-table.csv", skip = 6) |> 
            mutate(`[run number]` = `[run number]` + 140980) |> 
            rename(`attitude-bias` = `attitude-mean`, `beta-attitude` = `attitude-sd`, 
                   `fraction-masked` = `fraction-behavior turtles`),
           read_csv("data/compliance experiment-baseline-surrounding_fill7-table.csv", skip = 6) |> 
            mutate(`[run number]` = `[run number]` + 143620) |> 
            rename(`attitude-bias` = `attitude-mean`, `beta-attitude` = `attitude-sd`, 
                   `fraction-masked` = `fraction-behavior turtles`),
           read_csv("data/compliance experiment-baseline-surrounding_fill8-table.csv", skip = 6) |>
            mutate(`[run number]` = `[run number]` + 145300) |>
            rename(`attitude-bias` = `attitude-mean`, `beta-attitude` = `attitude-sd`,
                   `fraction-masked` = `fraction-behavior turtles`),
           read_csv("data/compliance experiment-baseline-surrounding_fill9-table.csv", skip = 6) |>
            mutate(`[run number]` = `[run number]` + 145540) |>
            rename(`attitude-bias` = `attitude-mean`, `beta-attitude` = `attitude-sd`,
                   `fraction-masked` = `fraction-behavior turtles`),
           read_csv("data/compliance experiment-baseline-surrounding_fill10-table.csv", skip = 6) |>
            mutate(`[run number]` = `[run number]` + 145570) |>
            rename(`attitude-bias` = `attitude-mean`, `beta-attitude` = `attitude-sd`,
                   `fraction-masked` = `fraction-behavior turtles`)
 )
nrow(data)/101

# rename variables and compute expected fraction masked when past behavior and conformity
# do not matter
d <- data |> select(
  id = `[run number]`, step = `[step]`, 
  fraction_behavior = `fraction-masked`,
  attitude_mean = `attitude-bias`, attitude_sd = `beta-attitude`, 
  beta_pastbehavior = `beta-pastbehavior`, beta_conformity = `beta-conformity`, 
  frac_random_contacts = `frac-random-contacts`, 
  timeline, 
  memory_capacity = `memory-capacity`, 
  avg_contacts = `avg-contacts`, N) |> 
  mutate(expected_fraction_behavior = pnorm(attitude_mean, mean = 0, sd = sqrt(1 + attitude_sd^2)))
rm(data)

# Check full factorial design
factorial_vars <- 
 c("attitude_mean", "attitude_sd", "beta_pastbehavior", "beta_conformity",
   "frac_random_contacts", "timeline", "memory_capacity", "avg_contacts")
d |> filter(step == 100) |> 
 count(across(all_of(factorial_vars))) |> pull(n) |> table()
d |> filter(step == 100) |> 
 count(across(all_of(factorial_vars))) |> filter(n == 90)
dd <- d |> filter(step == 100, attitude_mean == 0.1, attitude_sd == 1, 
                  frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15) |> 
 count(across(all_of(factorial_vars)))
dd <- d |> filter(step == 100, attitude_mean == 0.1, attitude_sd == 1, 
            beta_pastbehavior %in% seq(0,9,by=1.5), 
            beta_conformity %in% seq(0,9,by=1.5), 
            frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15) |> 
 count(across(all_of(factorial_vars)))
# 8 frac-random-contact (0, 0.2), memory (5, 20) and timeline (day-night, updates-in-place)
# 9 attitudes, bias (0, 0.05, 0.1) and beta (like sd, 0, 0.5, 1) 
# 49 pastbehavior, conformity (seq(0,3,by=0.5))
# --> 3528 configurations



# Fig 2: Histogram and color code of normally distributed attitudes
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


# Fig 3:  Several runs baseline

d |> filter(attitude_mean == 0.1, attitude_sd == 1, beta_pastbehavior == 1, beta_conformity == 6, 
            frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15) |> 
 ggplot(aes(step, fraction_behavior, color = factor(id))) + 
 geom_line() + 
 geom_line(aes(y=expected_fraction_behavior), color = "blue") + 
 ylim(c(0,1)) + guides(color = "none") + 
 scale_color_grey() + theme_minimal() +
 labs( x = "Time", y = "Fraction Behavior") 
d |> filter(attitude_mean == 0.1, attitude_sd == 1, beta_pastbehavior == 3, beta_conformity == 6, 
            frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15,
            step == 100) |> 
 ggplot(aes(fraction_behavior)) + geom_histogram(aes(y = after_stat(count / sum(count))),binwidth = 0.05)
 


d |> filter(attitude_mean == 0.1, attitude_sd == 1, 
            beta_pastbehavior %in% c(0,3,6), 
            beta_conformity %in% c(0,3,6), 
            frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15) |> 
 ggplot(aes(step, fraction_behavior, color = factor(id))) + 
 geom_line() + 
 geom_line(aes(y=expected_fraction_behavior), color = "blue") + 
 facet_grid(beta_pastbehavior~beta_conformity, labeller = label_both) +
 ylim(c(0,1)) + guides(color = "none") + 
 scale_color_grey() + theme_minimal() +
 labs( x = "Time", y = "Fraction Behavior") 

d |> filter(attitude_mean == 0.1, attitude_sd == 1, 
            beta_pastbehavior %in% seq(3,7,by=1), 
            beta_conformity %in% seq(3,7,by=1), 
            frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15) |> 
 ggplot(aes(step, fraction_behavior, color = factor(id))) + 
 geom_line() + 
 geom_line(aes(y=expected_fraction_behavior), color = "blue") + 
 facet_grid(beta_pastbehavior~beta_conformity, labeller = label_both) +
 ylim(c(0,1)) + guides(color = "none") + 
 scale_color_grey() + theme_minimal() +
 labs( x = "Time", y = "Fraction Behavior") 

d |> filter(attitude_mean == 0.1, attitude_sd == 1, 
            beta_pastbehavior %in% seq(0,6,by=1.5), 
            beta_conformity %in% seq(0,7.5,by=1.5), 
            frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15) |> 
 ggplot(aes(step, fraction_behavior, color = factor(id))) + 
 geom_line() + 
 geom_line(aes(y=expected_fraction_behavior), color = "blue") + 
 facet_grid(beta_pastbehavior~beta_conformity, labeller = label_both) +
 ylim(c(0,1)) + guides(color = "none") + 
 scale_color_grey() + theme_minimal() +
 labs( x = "Time", y = "Fraction Behavior") 

d |> filter(attitude_mean == 0.1, attitude_sd == 1, 
            beta_pastbehavior %in% seq(0,7.5,by=1.5), 
            beta_conformity %in% seq(0,7.5,by=1.5), 
            frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15) |> 
 ggplot(aes(step, fraction_behavior, color = factor(id))) + 
 geom_line() + 
 geom_line(aes(y=expected_fraction_behavior), color = "blue") + 
 facet_grid(beta_pastbehavior~beta_conformity, labeller = label_both) +
 ylim(c(0,1)) + guides(color = "none") + 
 scale_color_grey() + theme_minimal() +
 labs( x = "Time", y = "Fraction Behavior") 

d |> filter(attitude_mean == 0.1, attitude_sd == 1, 
            beta_pastbehavior %in% seq(0,7.5,by=1.5), 
            beta_conformity %in% seq(0,7.5,by=1.5), 
            frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15,
            step == 100) |> #count(beta_pastbehavior, beta_conformity) |> View()# |> 
ggplot(aes(fraction_behavior)) + geom_density(bw = 0.04) +
 # ggplot(aes(fraction_behavior)) + geom_histogram(aes(y = after_stat(count / sum(count))),binwidth = 0.05) +
 facet_grid(beta_pastbehavior~beta_conformity, labeller = label_both)
 



## Fig 4. Mean boost and wrong boost

ds <- d |> filter(attitude_mean == 0.1, attitude_sd == 1, 
            beta_pastbehavior %in% seq(0,8,by=0.5), 
            beta_conformity %in% seq(0,8,by=0.5), 
            frac_random_contacts == 0.2, timeline == "day-night", 
            memory_capacity == 5, avg_contacts == 15, step == 100) |> 
 group_by(beta_pastbehavior, beta_conformity) |> 
 summarize(
  n = n(),
  prevalence_mean = mean(fraction_behavior),
  prevalence_sd = sd(fraction_behavior),
  prevalence_extremity_mean = mean(abs(fraction_behavior-0.5)),
  prevalence_lower_frac = sum(fraction_behavior < 0.25)/n(),
  prevalence_higher_frac = sum(fraction_behavior > 0.75)/n(),
  prevalence_mid_frac = sum(fraction_behavior < 0.75 & fraction_behavior > 0.25)/n())
ds |> ggplot(aes(beta_conformity, beta_pastbehavior, 
                 fill = prevalence_higher_frac)) + geom_tile() + coord_fixed()
ds |> ggplot(aes(beta_conformity, beta_pastbehavior, 
                 fill = prevalence_lower_frac)) + geom_tile() + coord_fixed()
ds |> ggplot(aes(beta_conformity, beta_pastbehavior, 
                 fill = prevalence_mid_frac)) + geom_tile() + coord_fixed()
ds |> ggplot(aes(beta_conformity, beta_pastbehavior, 
                 fill = prevalence_extremity_mean)) + geom_tile() + coord_fixed()
ds |> ggplot(aes(beta_conformity, beta_pastbehavior, 
                 fill = prevalence_mean)) + geom_tile() + coord_fixed()
ds |> ggplot(aes(beta_conformity, beta_pastbehavior, 
                 fill = factor(n))) + geom_tile() + coord_fixed()





# Summary data frame
# ds <- d |> arrange(id,step) |> 
#   group_by(across(all_of(factorial_vars)), id, expected_fraction_masked) |> 
#   summarize(mean25_fraction_masked = mean(tail(fraction_masked,25)),
#             sd25_fraction_masked = sd(tail(fraction_masked,25)),
#             mean10_fraction_masked = mean(tail(fraction_masked,10)),
#             sd10_fraction_masked = sd(tail(fraction_masked,10)),
#             fraction_masked = tail(fraction_masked,1),
#             boost_fraction_masked = fraction_masked - tail(expected_fraction_masked,1),
#             boost_mean10_fraction_masked = mean10_fraction_masked - tail(expected_fraction_masked,1),
#             stab10 = fraction_masked - mean10_fraction_masked,
#             stab25 = fraction_masked - mean25_fraction_masked)
# ds |> ggplot() + geom_density(aes(stab10)) + geom_density(aes(stab25), color = "blue")
# The variation and in particular the difference between the blue and black line 
# shows that some trajectories haven't stabilized
# need to communicate this somehow but probably differently


# Figures

## Fig. 1 Flow Chart
## Set Base line configuration
## Describe the individual components and their probabilities step by step relating to the baseline
## Fig 2. Screen shots of interface: One run in baseline configuration

d |> filter(attitude_mean == 0.1, attitude_sd == 1, beta_pastbehavior == 3, beta_conformity == 6, 
            frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15) |> 
 ggplot(aes(step, fraction_behavior, color = factor(id))) + 
 geom_line() + 
 geom_line(aes(y=expected_fraction_behavior), color = "blue") + 
 ylim(c(0,1)) + guides(color = "none") + 
 scale_color_grey() + theme_minimal() +
 labs( x = "Time", y = "Fraction Behavior") 

d |> filter(attitude_mean == 0.1, attitude_sd == 1, beta_pastbehavior == 3, beta_conformity == 6, 
            frac_random_contacts == 0.2, timeline == "day-night", memory_capacity == 5, avg_contacts == 15, step == 100) |> 
 ggplot(aes(fraction_behavior)) + 
 geom_histogram() + theme_minimal() 


## Fig 3. Baseline how fraction masked evolves in differnt runs. Explain non convergence and different types of convergence
## Fig. 4Massive Figure



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
 rename(habit = beta_pastbehavior, norm = beta_conformity) |>
 ggplot(aes(step,fraction_masked,  color = factor(id) )) + 
 geom_line() + 
 geom_line(aes(y=expected_fraction_masked), color = "blue") + 
 ylim(c(0,1)) + guides(color = "none") + 
 facet_grid(habit ~ norm, 
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
# trajectories_facets(d,                   
#                     attitude_bias_ = 0, 
#                     beta_attitude_ = 0, 
#                     frac_random_contacts_ = 0.2, 
#                     #timeline_ = "day-night", 
#                     timeline_ = "updates-in-place", 
#                     memory_capacity_ = 5)
# trajectories_facets(d,                   
#                     attitude_bias_ = 0.05, 
#                     beta_attitude_ = 0, 
#                     frac_random_contacts_ = 0.2, 
#                     timeline_ = "updates-in-place", 
#                     memory_capacity_ = 5)
trajectories_facets(d,                   
                    attitude_bias_ = 0.1,
                    beta_attitude_ = 1, 
                    frac_random_contacts_ = 0.2, 
                    beta_pastbehavior_ = seq(0,8, by = 0.5), 
                    beta_conformity_ = seq(0,8, by = 0.5), 
                    timeline_ = "day-night", 
                    memory_capacity_ = 5)
trajectories_facets(d,                   
                    attitude_bias_ = 0.05,
                    beta_attitude_ = 0.5, 
                    frac_random_contacts_ = 0, 
                    beta_pastbehavior_ = seq(0,3, by = 0.5), 
                    beta_conformity_ = seq(0,3, by = 0.5), 
                    timeline_ = "day-night", 
                    memory_capacity_ = 5)

trajectories_facets(d,                   
                    attitude_bias_ = 0.1,
                    beta_attitude_ = 1, 
                    frac_random_contacts_ = 0.2, 
                    beta_pastbehavior_ = seq(3,6, by = 0.2), 
                    beta_conformity_ = seq(3,6, by = 0.2), 
                    timeline_ = "day-night", 
                    memory_capacity_ = 5)
# trajectories_facets(d,                   
#                     attitude_bias_ = 0.1, 
#                     beta_attitude_ = 1, 
#                     frac_random_contacts_ = 0.2, 
#                     timeline_ = "day-night", 
#                     memory_capacity_ = 5)
# trajectories_facets(d,                   
#                     attitude_bias_ = 0, 
#                     beta_attitude_ = 1, 
#                     frac_random_contacts_ = 0.2, 
#                     timeline_ = "updates-in-place", 
#                     memory_capacity_ = 5)



trajectories_facets(d,                   
                    attitude_bias_ = 0.05,
                    beta_attitude_ = 0.5, 
                    frac_random_contacts_ = 0, 
                    beta_pastbehavior_ = c(0.5,1,1.5,2,2.5,3), 
                    beta_conformity_ = c(0.5,1,1.5,2,2.5,3), 
                    timeline_ = "day-night", 
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






## Math
## baseline norm = 6, habit = 3, attitude_sd = 1, attitude_mean = 0.1
# Individual with attitude a has the following probability to be masked (given unobserved utility from standard normal distribution)
a = 0.1
pnorm(a)
pnorm(c(-0.9, 0.1, 1.1, 2.1))
pnorm(c(-1,0,1))
# When attitudes of individuals attitudes normally distributed with mean 0.1 and sd 1
# the expected fraction of masked individuals is
attitude_mean = 0.1
attitude_sd = 1
pnorm(attitude_mean, sd = sqrt(1+attitude_sd^2))
attitude_mean = 0.05
attitude_sd = 0.5
pnorm(attitude_mean, sd = sqrt(1+attitude_sd^2))
attitude_sd = 0 # resembles the case of a single individual with attitude 0.1
pnorm(attitude_mean, sd = sqrt(1+attitude_sd^2))
attitude_mean = 1
attitude_sd = 1
pnorm(attitude_mean, sd = sqrt(1+attitude_sd^2))
# When all individuals of frac_behavior_neighbors being 1 (and importance of habit is zero)
attitude_mean = 0.1
attitude_sd = 1
norm = 1
habit = 1
pnorm(attitude_mean+norm, sd = sqrt(1+attitude_sd^2))
pnorm(attitude_mean+habit, sd = sqrt(1+attitude_sd^2))
pnorm(attitude_mean+norm+habit, sd = sqrt(1+attitude_sd^2))
pnorm(attitude_mean-habit, sd = sqrt(1+attitude_sd^2))
pnorm(attitude_mean-norm, sd = sqrt(1+attitude_sd^2))
pnorm(attitude_mean-norm-habit, sd = sqrt(1+attitude_sd^2))

# Memory 5 distribution of behavior in memory. 
attitude_mean = 1
attitude_sd = 1
# The following distribution of behavior memorized in the last 5 days is expected when all individuals have attitude 0.1
dbinom(0:5, size = 5, prob = pnorm(attitude_mean)) |> barplot()

dnorm(qnorm(seq(0,1,0.01), mean = attitude_mean, sd = sqrt(attitude_sd^2 + 1)), mean = attitude_mean, sd = attitude_sd) |> plot(seq(0,1,0.01),y = _)
dnorm(qnorm(seq(0,1,0.01), mean = 0, sd = 1), mean = attitude_mean, sd = attitude_sd) |> plot(seq(0,1,0.01),y = _)
dbeta(seq(0,1,0.01), 1.8, 1.8) |> plot(seq(0,1,0.01),y = _)


library(extraDistr)

dbbinom(0:5, 5, alpha = 1, beta = 1) |> barplot()
dbbinom(0:5, 5, alpha = 1.8, beta = 1.8) |> barplot()
dbbinom(0:5, 5, alpha = 2.5, beta = 2.5) |> barplot()
