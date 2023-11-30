library(tidyverse)
library(moderndive)
library(skimr)
library(ISLR)

evals_ch6 <- evals %>%
  select(ID, score, age, gender)

glimpse(evals_ch6)
evals_ch6 %>% sample_n(size = 5)

evals_ch6 %>% select(score, age, gender) %>% skim()

evals_ch6 %>%
  get_correlation(formula = score ~ age)

ggplot(evals_ch6, aes(x = age, y = score, color = gender)) +
  geom_point() +
  labs(x = "Age", y = "Teaching Score", color = "Gender") +
  geom_smooth(method = "lm", se = FALSE)

# Fit regression model:
score_model_interaction <- lm(score ~ age * gender, data = evals_ch6)

# Get regression table:
get_regression_table(score_model_interaction)

ggplot(evals_ch6, aes(x = age, y = score, color = gender)) +
  geom_point() +
  labs(x = "Age", y = "Teaching Score", color = "Gender") +
  geom_parallel_slopes(se = FALSE)

# Fit regression model:
score_model_parallel_slopes <- lm(score ~ age + gender, data = evals_ch6)
# Get regression table:
get_regression_table(score_model_parallel_slopes)

regression_points <- get_regression_points(score_model_interaction)
regression_points

library(ISLR)
credit_ch6 <- Credit %>% as_tibble() %>%
  select(ID, debt = Balance, credit_limit = Limit,
         income = Income, credit_rating = Rating, age = Age)

View(credit_ch6)
glimpse(credit_ch6)
credit_ch6 %>% sample_n(size = 5)

credit_ch6 %>% select(debt, credit_limit, income) %>% skim()

credit_ch6 %>% get_correlation(debt ~ credit_limit)
credit_ch6 %>% get_correlation(debt ~ income)

credit_ch6 %>%
  select(debt, credit_limit, income) %>%
  cor()

ggplot(credit_ch6, aes(x = credit_limit, y = debt)) +
  geom_point() +
  labs(x = "Credit limit (in $)", y = "Credit card debt (in $)",
       title = "Debt and credit limit") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(credit_ch6, aes(x = income, y = debt)) +
  geom_point() +
  labs(x = "Income (in $1000)", y = "Credit card debt (in $)",
       title = "Debt and income") +
  geom_smooth(method = "lm", se = FALSE)

# Fit regression model:
debt_model <- lm(debt ~ credit_limit + income, data = credit_ch6)
# Get regression table:
get_regression_table(debt_model)

# Interaction model
ggplot(MA_schools,
       aes(x = perc_disadvan, y = average_sat_math, color = size)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Percent economically disadvantaged", y = "Math SAT Score",
       color = "School size", title = "Interaction model")

# Parallel slopes model
ggplot(MA_schools,
       aes(x = perc_disadvan, y = average_sat_math, color = size)) +
  geom_point(alpha = 0.25) +
  geom_parallel_slopes(se = FALSE) +
  labs(x = "Percent economically disadvantaged", y = "Math SAT Score",
       color = "School size", title = "Parallel slopes model")

model_2_interaction <- lm(average_sat_math ~ perc_disadvan * size,
                          data = MA_schools)
get_regression_table(model_2_interaction)

model_2_parallel_slopes <- lm(average_sat_math ~ perc_disadvan + size,
                              data = MA_schools)
get_regression_table(model_2_parallel_slopes)

get_regression_points(model_2_interaction)

get_regression_points(model_2_interaction) %>%
  summarize(var_y = var(average_sat_math),
            var_y_hat = var(average_sat_math_hat),
            var_residual = var(residual))

# R-squared for interaction model:
get_regression_summaries(model_2_interaction)

# R-squared for parallel slopes model:
get_regression_summaries(model_2_parallel_slopes)

credit_ch6 %>% select(debt, income) %>%
  mutate(income = income * 1000) %>%
  cor()
