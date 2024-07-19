# This loads a turtles.csv file which can be produced with compliance.nlogo at any time of a
# simulation run. This writes out all parameters an all current terms from the 
# utility function of individual agents except for the random noise term epsilon
# it also records the behavior of (compliant or not) of agents

library(tidyverse)
library(tidymodels)

# Load parameters
params <- read_csv("turtles.csv", skip = 1, n_max = 1)
# Load turtles table
turtles <- read_csv("turtles.csv", skip = 4) |> mutate(compliant = as.numeric(compliant))

# Measures which can be computed

# Correlations
correlations(turtles)
# Correlation of compliance and attitude taking the bias into account
turtles |> 
 mutate(attitude_with_bias = u_attitude + params$`attitude-bias`) |> 
 select(compliant, attitude_with_bias) |> 
 correlations() # Actually it is identical which one could have known with proper math knowledge...

# Regression model, with probit function, that instead of logit seems to be appropriate
# because the error term epsilon in the utility function is normally distributed and not
# from a logistic distribution
turt_fit <- 
 logistic_reg() |>  
 set_engine("glm", family = stats::binomial(link = "probit")) |> 
 fit(factor(compliant) ~ ., data = turtles)
tidy(turt_fit)
# Compare the coefficients to the parameters
params |> 
 select(`attitude-bias`, `beta-conformity`, `beta-pastbehavior`, `beta-attitude`) |> 
 pivot_longer(c(`attitude-bias`, `beta-conformity`, `beta-pastbehavior`, `beta-attitude`)) |>
 bind_cols(tidy(turt_fit))

# simple logistic regression (probit) with only one variable
turt_attfit <- logistic_reg() |>  
 set_engine("glm", family = stats::binomial(link = "probit")) |> 
 fit(factor(compliant) ~ u_attitude, data = turtles)
turt_attfit
# roc curve, auc, and accuracy
turt_attfit |> predict(new_data = turtles, type = "prob") |> bind_cols(turtles) |> 
 roc_curve(truth = factor(compliant), estimate = .pred_1, event_level = "second") |> 
 autoplot()
turt_attfit |> predict(new_data = turtles, type = "prob") |> bind_cols(turtles) |> 
 roc_auc(truth = factor(compliant), estimate = .pred_1, event_level = "second")
turt_attfit |> predict(new_data = turtles, type = "class") |> bind_cols(turtles) |> 
 accuracy(truth = factor(compliant), estimate = .pred_class, event_level = "second")

