library(tidyverse)
library(moderndive)
library(skimr)
library(gapminder)

evals_ch5 <- evals %>%
  select(ID, score, bty_avg, age)
evals_ch5
glimpse(evals_ch5)
evals_ch5 %>%
  sample_n(size = 5)

evals_ch5 %>%
  summarize(mean_bty_avg = mean(bty_avg), mean_score = mean(score),
            median_bty_avg = median(bty_avg), median_score = median(score))

evals_ch5 %>% select(score, bty_avg) %>% skim()

evals_ch5 %>%
  get_correlation(formula = score ~ bty_avg)

evals_ch5 %>%
  summarize(correlation = cor(score, bty_avg))

ggplot(evals_ch5, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "Beauty Score",
       y = "Teaching Score",
       title = "Scatterplot of relationship of teaching and beauty scores")

ggplot(evals_ch5, aes(x = bty_avg, y = score)) +
  geom_jitter() +
  labs(x = "Beauty Score", y = "Teaching Score",
       title = "Scatterplot of relationship of teaching and beauty scores")

ggplot(evals_ch5, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "Beauty Score", y = "Teaching Score",
       title = "Relationship between teaching and beauty scores") +
  geom_smooth(method = "lm", se = FALSE)

# Fit regression model:
score_model <- lm(score ~ bty_avg, data = evals_ch5)
# Get regression table:
get_regression_table(score_model)

regression_points <- get_regression_points(score_model)
regression_points

library(gapminder)
gapminder2007 <- gapminder %>%
  filter(year == 2007) %>%
  select(country, lifeExp, continent, gdpPercap)

glimpse(gapminder2007)

gapminder2007 %>% sample_n(size = 5)

gapminder2007 %>%
  select(lifeExp, continent) %>%
  skim()

ggplot(gapminder2007, aes(x = lifeExp)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Life expectancy", y = "Number of countries",
       title = "Histogram of distribution of worldwide life expectancies")

ggplot(gapminder2007, aes(x = lifeExp)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Life expectancy",
       y = "Number of countries",
       title = "Histogram of distribution of worldwide life expectancies") +
  facet_wrap(~ continent, nrow = 2)

ggplot(gapminder2007, aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy",
       title = "Life expectancy by continent")

gapminder2007 %>%
  filter(country %in% c("China", "United States", "Japan", "India")) %>%
  ggplot(aes(x = country, y = lifeExp)) +
  geom_boxplot() +
  labs(x = "Country", y = "Life expectancy",
       title = "Life expectancy by Country")

gapminder |>
  filter(country %in% c("United States", "China")) %>%
  ggplot(aes(x = year, y = lifeExp, colors = country)) +
  geom_line()

lifeExp_model <- lm(lifeExp ~ continent, data = gapminder2007)
get_regression_table(lifeExp_model)

regression_points <- get_regression_points(lifeExp_model, ID = "country")
regression_points

#5.3
# Fit regression model:
score_model <- lm(score ~ bty_avg,
                  data = evals_ch5)

# Get regression points:
regression_points <- get_regression_points(score_model)
regression_points

# Compute sum of squared residuals
regression_points %>%
  mutate(squared_residuals = residual^2) %>%
  summarize(sum_of_squared_residuals = sum(squared_residuals))

# Fit regression model:
score_model <- lm(formula = score ~ bty_avg, data = evals_ch5)
# Get regression table:
get_regression_table(score_model)

library(broom)
library(janitor)
score_model %>%
  augment() %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  clean_names() %>%
  select(-c("std_resid", "hat", "sigma", "cooksd", "std_resid"))
