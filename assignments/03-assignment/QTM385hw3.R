# Set seed for reproducibility
set.seed(385)

library(fabricatr)
library(randomizr)
library(estimatr)

# Define number of observations
N <- 1000

# Correct way to assign 500 treated and 500 control
treat <- complete_ra(N = N, m = 500)  # 'm' specifies the number of treated individuals

# Generate binary gender variable (0 = male, 1 = female)
gender <- rbinom(N, 1, 0.5)

# Generate outcome variable 'interviews'
interviews <- round(rnorm(N, mean = 10, sd = 2) + 5 * treat + 2 * gender, digits = 0)

# Create DataFrame
data <- data.frame(treat, gender, interviews)

# Estimate Average Treatment Effect (ATE) using OLS regression
ate_model <- lm_robust(interviews ~ treat, data = data)
summary(ate_model)

# Simulate dataset with heterogeneous treatment effects (education)
education <- rbinom(N, 1, 0.5)  # Binary education variable (0 = low, 1 = high)
interviews_het <- round(rnorm(N, mean = 10, sd = 2) + 4 * treat + 3 * treat * education, digits = 0)

# Update DataFrame with new education variable and adjusted interviews
data$education <- education
data$interviews_het <- interviews_het

# Estimate treatment effects using an interaction term (treat * education)
interaction_model <- lm_robust(interviews_het ~ treat * education, data = data)
summary(interaction_model)
