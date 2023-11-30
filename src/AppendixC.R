library(tidyverse)
library(scales)
library(janitor)
library(dygraphs)
library(nycflights13)
movies_ex <- read_csv("https://moderndive.com/data/movies.csv") %>%
  filter(type %in% c("action", "comedy", "drama", "animated", "fantasy", "rom comedy")) %>%
  select(-over200)

movies_ex %>%
  slice(1:10)

movies_ex %>%
  summarize(mean_profit = median(millions))

movies_ex %>%
  summarize(mean_profit = median(millions, na.rm = TRUE))

movies_no_missing <- movies_ex %>%
  filter(!is.na(millions))

movies_no_missing %>%
  slice(1:10)

revenue_by_type <- movies_ex %>%
  group_by(type) %>%
  summarize(total_revenue = sum(millions))
revenue_by_type

ggplot(revenue_by_type, aes(x = type, y = total_revenue)) +
  geom_col() +
  labs(x = "Movie genre", y = "Total box office revenue (in millions of $)")

type_levels <- c("rom comedy", "action", "drama", "animated", "comedy", "fantasy")

revenue_by_type <- revenue_by_type %>%
  mutate(type = factor(type, levels = type_levels))

ggplot(revenue_by_type, aes(x = type, y = total_revenue)) +
  geom_col() +
  labs(x = "Movie genre", y = "Total boxoffice revenue (in millions of $)")

revenue_by_type <- revenue_by_type %>%
  mutate(type = reorder(type, total_revenue))

ggplot(revenue_by_type, aes(x = type, y = total_revenue)) +
  geom_col() +
  labs(
    x = "Movie genre", y = "Total boxoffice revenue (in millions of $)"
  )

revenue_by_type <- revenue_by_type %>%
  mutate(type = reorder(type, -total_revenue))

ggplot(revenue_by_type, aes(x = type, y = total_revenue)) +
  geom_col() +
  labs(
    x = "Movie genre", y = "Total boxoffice revenue (in millions of $)"
  )


movies_ex <- movies_ex %>%
  mutate(revenue = millions * 10^6)

ggplot(data = movies_ex, aes(x = rating, y = revenue)) +
  geom_boxplot() +
  labs(x = "rating", y = "Revenue in $", title = "Profits for different movie ratings")

# Don't forget to load the scales package first!
library(scales)

ggplot(data = movies_ex, aes(x = rating, y = revenue)) +
  geom_boxplot() +
  labs(x = "rating", y = "Revenue in $", title = "Profits for different movie ratings") +
  scale_y_continuous(labels = dollar)

movies_ex %>%
  mutate(type_new = if_else(type == "rom comedy", "romantic comedy", type)) %>%
  slice(1:10)

movies_ex %>%
  mutate(type = if_else(type == "rom comedy", "romantic comedy", "not romantic comedy")) %>%
  slice(1:10)

movies_ex %>%
  mutate(type_new = recode(type,
                           "action" = "Action",
                           "animated" = "Animated",
                           "comedy" = "Comedy",
                           "drama" = "Drama",
                           "fantasy" = "Fantasy",
                           "rom comedy" = "Romantic Comedy"
  )) %>%
  slice(1:10)

movies_ex %>%
  mutate(
    type_new =
      case_when(
        type == "action" & millions > 40 ~ "Big budget action",
        type == "rom comedy" & millions < 40 ~ "Small budget romcom",
        # Need this for everything else that aren't the two cases above:
        TRUE ~ "Rest"
      )
  )

movies_ex %>%
  mutate(big_budget = millions > 100) %>%
  slice(1:10)

movies_ex %>%
  mutate(score_categ = cut(score,
                           breaks = c(0, 40, 60, 80, 100),
                           labels = c("bad", "so-so", "good", "great")
  )) %>%
  slice(1:10)

rating_by_type_millions <- movies_ex %>%
  group_by(rating, type) %>%
  summarize(millions = sum(millions)) %>%
  arrange(rating, type)

rating_by_type_millions

rating_by_type_millions %>%
  group_by(rating) %>%
  mutate(
    # Compute a new column of the sum of millions split by rating:
    total_millions = sum(millions),
    # Compute the proportion within each rating:
    prop = millions / total_millions
  )

library(readr)
parse_number("10.5%")
parse_number("145,897")
parse_number("$1,234.5")

library(scales)
percent(0.105)
comma(145897)
dollar(1234.5)

library(dygraphs)
library(nycflights13)
flights_day <- mutate(flights, date = as.Date(time_hour))

flights_summarized <- flights_day %>%
  group_by(date) %>%
  summarize(median_arr_delay = median(arr_delay, na.rm = TRUE))

rownames(flights_summarized) <- flights_summarized$date

flights_summarized <- select(flights_summarized, -date)

dyRangeSelector(dygraph(flights_summarized))
