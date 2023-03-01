library(nycflights13)
library(tidyverse)
flights
nycflights13::flights
View(flights)
filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)
jan1
(dec25 <- filter(flights, month == 12, day == 25))
filter(flights, month = 1)
sqrt(2)^2 == 2
1/49 * 49
near(sqrt(2)^2, 2)
near(1/49 * 49, 1)
filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11,12))
nov_dec
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)
NA > 5
10 == NA
NA + 10
NA/2
NA == NA
x <- NA
y <-NA
x == y
is.na(x)
df <- tibble(x = c(1,NA,3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)
filter(flights, dest == "BOS")
filter(flights, carrier == "UA")
filter(flights, month %in% c(7,8,9))
filter(flights, (arr_delay > 120 & dep_delay == 0))
filter(flights, (arr_delay > 60 & dep_delay == 30))
filter(flights, (hour > 0 & hour < 6))
filter(flights, between(hour, 0, 6))
filter(flights, is.na(dep_time))
NA^0
NA | TRUE
FALSE & NA
NA * 0
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))
arrange(flights, desc(is.na(arr_delay)))
arrange(flights, desc(dep_delay))
arrange(flights, year, month, day, dep_delay)
arrange(flights, air_time)
arrange(flights, desc(air_time))
select(flights, year, month, day)
select(flights, year : day)
select(flights, -(year : day))
select(flights, starts_with("dep"))
select(flights, ends_with("time"))
select(flights, contains("arr"))
rename(flights, tail_num = tailnum)
select(flights, time_hour, air_time, everything())
vars <- c("year","month","day","dep_delay","arr_delay")
select(flights,one_of(vars))
select(flights, contains("TIME"))
flights_sml <- select(flights, year : day, ends_with("delay"), distance, air_time)
flights_sml
mutate(flights_sml, gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)
mutate(flights_sml, gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)
transmute(flights, gain = arr_delay - dep_delay, 
          hours = air_time / 60, 
          gain_per_hour = gain / hours)
transmute(flights, dep_time, hour = dep_time %/% 100, 
          minute = dep_time %% 100)
(x <- 1 : 10)
lag(x)
lead(x)
x
cumsum(x)
cummean(x)
y <- c(1,2,2,NA,3,4)
min_rank(y)
min_rank(desc(y))
row_number(y)
dense_rank(y)
percent_rank(y); cume_dist(y)
transmute(flights, air_time, arr_time, dep_time, comparacao = arr_time - dep_time)
select(flights, dep_time, sched_dep_time, dep_delay)
transmute(flights, min_rank(arr_delay))
1:3 + 1:10
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest, count = n(), 
                   dist = mean(distance, na.rm = TRUE),
                  delay = mean(arr_delay, na.rm = TRUE))
delay <-filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) + 
  geom_point(aes(size = count), alpha = 1/3) + 
  geom_smooth(se = FALSE)
delays <- flights %>% group_by(dest) %>% 
  summarise(count = n(), dist = mean(distance, na.rm = TRUE), 
            delay = mean(arr_delay, na.rm = TRUE)) %>% filter(count > 20, dest != "HNL")

flights %>% group_by(year, month, day) %>% summarise(mean = mean(dep_delay))
flights %>% group_by(year, month, day) %>% summarise(mean = mean(dep_delay, na.rm = TRUE))
not_cancelled <- flights %>% filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>% group_by(year, month, day) %>% summarise(mean = mean(dep_delay))
delays <- not_cancelled %>% group_by(tailnum) %>% 
  summarise(delay = mean(arr_delay))
ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)
delays <- not_cancelled %>% group_by(tailnum) %>% 
  summarise(delay = mean(arr_delay, na.rm = TRUE), n = n())
ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)
delays %>% filter(n > 25) %>% ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)
batting <- as_tibble(Lahman :: Batting)
batters <- batting %>% group_by(playerID) %>% 
  summarise(ba = sum(H, na.rm = TRUE)/sum(AB, na.rm = TRUE), 
            ab = sum(AB, na.rm = TRUE))
batters %>% filter(ab > 100) %>% ggplot(mapping = aes(x = ab, y = ba)) + 
  geom_point() + geom_smooth(se = FALSE)
batters %>% arrange(desc(ba))
not_cancelled %>% group_by(year, month, day) %>% 
  summarise(avg_delay1 = mean(arr_delay),
            avg_delay2 = mean(arr_delay[arr_delay > 0]))
not_cancelled %>% group_by(dest) %>% summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))
not_cancelled %>% group_by(year, month, day) %>% 
  summarise(flrst = min(dep_time),
            last = max(dep_time))
not_cancelled %>% group_by(year, month, day) %>% 
  summarise(first_dep = first(dep_time), last_dep = last(dep_time))
not_cancelled %>% group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% filter(r %in% range(r))
not_cancelled %>% group_by(dest) %>% summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))
not_cancelled %>% count(dest)
not_cancelled %>% count(tailnum, wt = distance)
not_cancelled %>% group_by(year, month, day) %>% summarise(n_early = sum(dep_time < 500))
not_cancelled %>% group_by(year, month, day) %>% 
  summarise(hour_per = mean(arr_delay > 60))
daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
daily %>% ungroup() %>% summarise(flights = n())
flights %>% group_by(carrier, dest) %>% summarise(n())
flights_sml %>% group_by(year, month, day) %>% filter(rank(desc(arr_delay)) < 10)
popular_dests <- flights %>% group_by(dest) %>% filter(n() > 365)
popular_dests %>% filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)


  
flights %>% group_by(sched_dep_time, dep_time) %>% 
  mutate(atraso15 = sched_dep_time - dep_time, !is.na(atraso15)) %>% 
  filter(atraso15 == 15) %>% summarise(atraso15)

flights %>% group_by(sched_dep_time, dep_time) %>% 
  mutate(atraso15 = sched_dep_time - dep_time, !is.na(atraso15)) %>% 
  filter( atraso15 == -120) %>% summarise(atraso15)
not_cancelled %>% count(dest)
not_cancelled %>% group_by(dest) %>% summarise(tot = n())
not_cancelled %>% count(tailnum, wt = distance)
not_cancelled %>% group_by(tailnum) %>% summarise(tot = sum(distance))
summarise(is.na(dep_delay)|(is.na(arr_delay)))

flights %>% group_by(carrier, dest, dep_delay, arr_delay) %>% summarise(n())

flights  %>% group_by(dest) %>%
  summarise(count = n(),atraso_total = sum(sched_dep_time - dep_time, na.rm = TRUE) )
flights %>% group_by(dest) %>% summarise(n())
flights %>% group_by(dest, sched_dep_time, dep_time) %>% 
  summarise(atraso = sum(sched_dep_time - dep_time, na.rm = TRUE))

flights %>% group_by(dest, sched_dep_time, dep_time) %>% 
  summarise(atraso = sum(sched_dep_time - dep_time, na.rm = TRUE), 
            diferenca = atraso - lag(atraso)) 
