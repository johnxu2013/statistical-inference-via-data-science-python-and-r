library(tidyverse)
library(infer)
library(janitor)

age_at_marriage <- read_csv("https://moderndive.com/data/ageAtMar.csv")
View(age_at_marriage)

library(kableExtra)
age_summ <- age_at_marriage %>%
  summarize(
    sample_size = n(),
    mean = mean(age),
    sd = sd(age),
    minimum = min(age),
    lower_quartile = quantile(age, 0.25),
    median = median(age),
    upper_quartile = quantile(age, 0.75),
    max = max(age)
  )
kable(age_summ) %>%
  kable_styling(
    font_size = ifelse(is_latex_output(), 10, 16),
    latex_options = c("hold_position")
  )

ggplot(data = age_at_marriage, mapping = aes(x = age)) +
  geom_histogram(binwidth = 3, color = "white")

x_bar <- age_at_marriage %>%
  specify(response = age) %>%
  calculate(stat = "mean")
x_bar

set.seed(2018)
null_distn_one_mean <- age_at_marriage %>%
  specify(response = age) %>%
  hypothesize(null = "point", mu = 23) %>%
  generate(reps = 10000) %>%
  calculate(stat = "mean")

null_distn_one_mean %>% visualize()

null_distn_one_mean %>%
  visualize() +
  shade_p_value(obs_stat = x_bar, direction = "greater")


pvalue <- null_distn_one_mean %>%
  get_pvalue(obs_stat = x_bar, direction = "greater")
pvalue

boot_distn_one_mean <- age_at_marriage %>%
  specify(response = age) %>%
  generate(reps = 10000) %>%
  calculate(stat = "mean")

ci <- boot_distn_one_mean %>%
  get_ci()
ci

boot_distn_one_mean %>%
  visualize() +
  shade_ci(endpoints = ci)

ggplot(data = age_at_marriage, mapping = aes(sample = age)) +
  stat_qq()

t_test_results <- age_at_marriage %>%
  t_test(
    formula = age ~ NULL,
    alternative = "greater",
    mu = 23
  )
t_test_results

t.test(
  x = age_at_marriage$age,
  alternative = "two.sided",
  mu = 23
)$conf

elec <- c(rep("satisfied", 73), rep("unsatisfied", 27)) %>%
  enframe() %>%
  rename(satisfy = value)

ggplot(data = elec, aes(x = satisfy)) +
  geom_bar()



p_hat <- elec %>%
  specify(response = satisfy, success = "satisfied") %>%
  calculate(stat = "prop")
p_hat

View(elec)

set.seed(2018)
null_distn_one_prop <- elec %>%
  specify(response = satisfy, success = "satisfied") %>%
  hypothesize(null = "point", p = 0.8) %>%
  generate(reps = 10000) %>%
  calculate(stat = "prop")

null_distn_one_prop %>% visualize()

null_distn_one_prop %>%
  visualize() +
  shade_p_value(obs_stat = p_hat, direction = "both")

pvalue <- null_distn_one_prop %>%
  get_pvalue(obs_stat = p_hat, direction = "both")
pvalue



boot_distn_one_prop <- elec %>%
  specify(response = satisfy, success = "satisfied") %>%
  generate(reps = 10000) %>%
  calculate(stat = "prop")

boot_distn_one_prop

ci <- boot_distn_one_prop %>%
  get_ci()
ci


boot_distn_one_prop %>%
  visualize() +
  shade_ci(endpoints = ci)


#traditional methods
p_hat <- 0.73
p0 <- 0.8
n <- 100
(z_obs <- (p_hat - p0) / sqrt((p0 * (1 - p0)) / n))


elec %>%
  specify(response = satisfy, success = "satisfied") %>%
  hypothesize(null = "point", p = 0.8) %>%
  assume(distribution = "z") %>%
  visualize() +
  shade_p_value(obs_stat = z_obs, direction = "both")

2 * pnorm(z_obs)

prop.test(
  x = table(elec$satisfy),
  n = length(elec$satisfy),
  alternative = "two.sided",
  p = 0.8,
  correct = FALSE
)

#B4.1
offshore <- read_csv("https://moderndive.com/data/offshore.csv")

counts <- offshore %>% tabyl(college_grad, response)
counts

offshore <- offshore %>%
  mutate(response = fct_rev(response))

ggplot(offshore, aes(x = college_grad, fill = response)) +
  geom_bar(position = "fill") +
  labs(x = "College grad?", y = "Proportion with no opinion on drilling") +
  coord_flip()

d_hat <- offshore %>%
  specify(response ~ college_grad, success = "no opinion") %>%
  calculate(stat = "diff in props", order = c("yes", "no"))
d_hat

set.seed(2018)
null_distn_two_props <- offshore %>%
  specify(response ~ college_grad, success = "no opinion") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 10000) %>%
  calculate(stat = "diff in props", order = c("yes", "no"))

null_distn_two_props %>% visualize()

null_distn_two_props %>%
  visualize() +
  shade_p_value(obs_stat = d_hat, direction = "both")

pvalue <- null_distn_two_props %>%
  get_pvalue(obs_stat = d_hat, direction = "two_sided")
pvalue

boot_distn_two_props <- offshore %>%
  specify(response ~ college_grad, success = "no opinion") %>%
  generate(reps = 10000) %>%
  calculate(stat = "diff in props", order = c("yes", "no"))

ci <- boot_distn_two_props %>%
  get_ci()
ci

boot_distn_two_props %>%
  visualize() +
  shade_ci(endpoints = ci)

z_hat <- offshore %>%
  specify(response ~ college_grad, success = "no opinion") %>%
  calculate(stat = "z", order = c("yes", "no"))
z_hat

2 * pnorm(-3.16, lower.tail = TRUE)

cle_sac <- read.delim("https://moderndive.com/data/cleSac.txt") %>%
  rename(
    metro_area = Metropolitan_area_Detailed,
    income = Total_personal_income
  ) %>%
  na.omit()

View(cle_sac)

inc_summ <- cle_sac %>%
  group_by(metro_area) %>%
  summarize(
    sample_size = n(),
    mean = mean(income),
    sd = sd(income),
    minimum = min(income),
    lower_quartile = quantile(income, 0.25),
    median = median(income),
    upper_quartile = quantile(income, 0.75),
    max = max(income)
  )
kable(inc_summ) %>%
  kable_styling(
    font_size = ifelse(is_latex_output(), 10, 16),
    latex_options = c("hold_position")
  )


ggplot(cle_sac, aes(x = metro_area, y = income)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", color = "red")

d_hat <- cle_sac %>%
  specify(income ~ metro_area) %>%
  calculate(
    stat = "diff in means",
    order = c("Sacramento_ CA", "Cleveland_ OH")
  )
d_hat

set.seed(2018)
null_distn_two_means <- cle_sac %>%
  specify(income ~ metro_area) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 10000) %>%
  calculate(
    stat = "diff in means",
    order = c("Sacramento_ CA", "Cleveland_ OH")
  )

null_distn_two_means %>% visualize()

null_distn_two_means %>%
  visualize() +
  shade_p_value(obs_stat = d_hat, direction = "both")

pvalue <- null_distn_two_means %>%
  get_pvalue(obs_stat = d_hat, direction = "both")
pvalue


boot_distn_two_means <- cle_sac %>%
  specify(income ~ metro_area) %>%
  generate(reps = 10000) %>%
  calculate(
    stat = "diff in means",
    order = c("Sacramento_ CA", "Cleveland_ OH")
  )

ci <- boot_distn_two_means %>%
  get_ci()
ci

boot_distn_two_means %>%
  visualize() +
  shade_ci(endpoints = ci)


ggplot(cle_sac, aes(x = income)) +
  geom_histogram(color = "white", binwidth = 20000) +
  facet_wrap(~metro_area)

cle_sac %>%
  specify(income ~ metro_area) %>%
  calculate(
    stat = "t",
    order = c("Cleveland_ OH", "Sacramento_ CA")
  )

2 * pt(-1.50, df = min(212 - 1, 175 - 1), lower.tail = TRUE)

2 * pnorm(-1.50)

#exploring data analysis
zinc_tidy <- read_csv("https://moderndive.com/data/zinc_tidy.csv")
zinc_diff <- zinc_tidy %>%
  group_by(loc_id) %>%
  summarize(pair_diff = diff(concentration)) %>%
  ungroup()

d_hat <- zinc_diff %>%
  specify(response = pair_diff) %>%
  calculate(stat = "mean")
d_hat

ggplot(zinc_diff, aes(x = pair_diff)) +
  geom_histogram(binwidth = 0.04, color = "white")

#bootstrapping for hypothesis test
set.seed(2018)
null_distn_paired_means <- zinc_diff %>%
  specify(response = pair_diff) %>%
  hypothesize(null = "point", mu = 0) %>%
  generate(reps = 10000) %>%
  calculate(stat = "mean")

null_distn_paired_means %>% visualize()

null_distn_paired_means %>%
  visualize() +
  shade_p_value(obs_stat = d_hat, direction = "less")

pvalue <- null_distn_paired_means %>%
  get_pvalue(obs_stat = d_hat, direction = "less")
pvalue

#bootstrapping for ci
boot_distn_paired_means <- zinc_diff %>%
  specify(response = pair_diff) %>%
  generate(reps = 10000) %>%
  calculate(stat = "mean")

ci <- boot_distn_paired_means %>%
  get_ci()
ci

boot_distn_paired_means %>%
  visualize() +
  shade_ci(endpoints = ci)

#traditional
t_test_results <- zinc_diff %>%
  t_test(
    formula = pair_diff ~ NULL,
    alternative = "less",
    mu = 0
  )
t_test_results

zinc_diff

pt(-4.8638, df = nrow(zinc_diff) - 1, lower.tail = TRUE)
