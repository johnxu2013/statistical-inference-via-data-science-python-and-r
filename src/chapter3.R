library(dplyr)
library(ggplot2)
library(nycflights13)

alaska_flights <- flights %>%
  filter(carrier == "AS")

portland_flights <- flights %>%
  filter(dest == "PDX")
View(portland_flights)

btv_sea_flights_fall <- flights %>%
  filter(origin == "JFK" & (dest == "BTV" | dest == "SEA") & month >= 10)
View(btv_sea_flights_fall)

not_BTV_SEA <- flights %>%
  filter(!(dest == "BTV" | dest == "SEA"))
View(not_BTV_SEA)

flights %>% filter(!dest == "BTV" | dest == "SEA")

many_airports <- flights %>%
  filter(dest == "SEA" | dest == "SFO" | dest == "PDX" |
           dest == "BTV" | dest == "BDL")

many_airports <- flights %>%
  filter(dest %in% c("SEA", "SFO", "PDX", "BTV", "BDL"))
View(many_airports)

summary_temp <- weather %>%
  summarize(mean = mean(temp), std_dev = sd(temp))
summary_temp

summary_temp <- weather %>%
  summarize(mean = mean(temp, na.rm = TRUE),
            std_dev = sd(temp, na.rm = TRUE))
summary_temp

summary_temp <- weather %>%
  summarize(mean = mean(temp, na.rm = TRUE),
            count = n())
summary_temp

summary_monthly_temp <- weather %>%
  group_by(month) %>%
  summarize(mean = mean(temp, na.rm = TRUE),
            std_dev = sd(temp, na.rm = TRUE))
summary_monthly_temp

diamonds

diamonds %>%
  group_by(cut) %>%
  summarize(avg_price = mean(price))

diamonds %>%
  group_by(cut) %>%
  ungroup()

by_origin <- flights %>%
  group_by(origin) %>%
  summarize(count = n())
by_origin

by_origin_monthly <- flights %>%
  group_by(origin, month) %>%
  summarize(count = n())
by_origin_monthly

by_origin_monthly_incorrect <- flights %>%
  group_by(origin) %>%
  group_by(month) %>%
  summarize(count = n())
by_origin_monthly_incorrect

by_monthly_origin <- flights %>%
  group_by(month, origin) %>%
  summarize( count = n())
by_monthly_origin

flights %>%
  filter(!dest %in% c("EWR", "JFK", "LGA")) %>%
  group_by(carrier) %>%
  summarize(count = n())

flights %>%
  group_by(carrier) %>%
  filter(!dest %in% c("EWR", "JFK", "LGA")) %>%
  summarize(count = n())

weather <- weather %>%
  mutate(temp_in_C = (temp - 32) / 1.8)
View(weather)

summary_monthly_temp <- weather %>%
  group_by(month) %>%
  summarize(mean_temp_in_F = mean(temp, na.rm = TRUE),
            mean_temp_in_C = mean(temp_in_C, na.rm = TRUE))
summary_monthly_temp

flights <- flights %>%
  mutate(gain = dep_delay - arr_delay)
View(flights)

gain_summary <- flights %>%
  summarize(
    min = min(gain, na.rm = TRUE),
    q1 = quantile(gain, 0.25, na.rm = TRUE),
    median = quantile(gain, 0.5, na.rm = TRUE),
    q3 = quantile(gain, 0.75, na.rm = TRUE),
    max = max(gain, na.rm = TRUE),
    mean = mean(gain, na.rm = TRUE),
    sd = sd(gain, na.rm = TRUE),
    missing = sum(is.na(gain))
  )
gain_summary

ggplot(data = flights, mapping = aes(x = gain)) +
  geom_histogram(color = "white", bins = 20)

flights <- flights %>%
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours
  )

#3.6
freq_dest <- flights %>%
  group_by(dest) %>%
  summarize(num_flights = n())
freq_dest

freq_dest %>%
  arrange(num_flights)

freq_dest %>%
  arrange(desc(num_flights))

View(airlines)

flights_joined <- flights %>%
  inner_join(airlines, by = "carrier")
View(flights)
View(flights_joined)

View(airports)

flights_with_airport_names <- flights %>%
  inner_join(airports, by = c("dest" = "faa"))
View(flights_with_airport_names)

named_dests <- flights %>%
  group_by(dest) %>%
  summarize(num_flights = n()) %>%
  arrange(desc(num_flights)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  rename(airport_name = name)
named_dests

flights_weather_joined <- flights %>%
  inner_join(weather, by = c("year", "month", "day", "hour", "origin"))
View(flights_weather_joined)


joined_flights <- flights %>%
  inner_join(airlines, by = "carrier")
View(joined_flights)

glimpse(flights)

flights %>%
  select(carrier, flight)

flights_no_year <- flights %>% select(-year)

flight_arr_times <- flights %>% select(month:day, arr_time:sched_arr_time)
flight_arr_times

flights_reorder <- flights %>%
  select(year, month, day, hour, minute, time_hour, everything())
glimpse(flights_reorder)

flights %>% select(starts_with("a"))
flights %>% select(ends_with("delay"))
flights %>% select(contains("time"))

flights_time_new <- flights %>%
  select(dep_time, arr_time) %>%
  rename(departure_time = dep_time, arrival_time = arr_time)
glimpse(flights_time_new)

named_dests %>% top_n(n = 10, wt = num_flights)
named_dests  %>%
  top_n(n = 10, wt = num_flights) %>%
  arrange(desc(num_flights))
