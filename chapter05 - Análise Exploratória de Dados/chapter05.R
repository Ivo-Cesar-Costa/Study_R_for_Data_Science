library(tidyverse)
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))
diamonds %>% count(cut)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat), binwidth = 0.5)
diamonds %>% count(cut_width(carat, 0.5))
smaller <- diamonds %>% filter(carat < 3)
ggplot(data = smaller, mapping = aes(x = carat)) + geom_histogram(binwidth = 0.1)
ggplot(data = smaller, mapping = aes(x = carat, color = cut)) + 
  geom_freqpoly(binwidth = 0.1)
ggplot(data = smaller, mapping = aes(x = carat)) + 
  geom_histogram(binwidth = 0.01)
ggplot(data = faithful, mapping = aes(x = eruptions)) + geom_histogram(binwidth = 0.25)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5) + 
  coord_cartesian(ylim = c(0,50))
unusual <- diamonds %>% filter(y < 3 | y > 20) %>% arrange(y)
unusual
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = x), binwidth = 0.5)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = z), binwidth = 0.5)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = price), binwidth = 2)
diamonds %>% filter(carat == 0.99) %>% count(carat)
diamonds %>% count(carat == 1)
diamonds %>% filter(carat == 1) %>% count(carat)
 ggplot(data = diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5) + 
  coord_cartesian(ylim = c(0,50)) + xlim(0,10)
diamonds2 <- diamonds %>% filter(between(y,3,20))
diamonds2
diamonds2 <-diamonds %>% mutate(y = ifelse(y < 3 | y > 20, NA, y))
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + geom_point()
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + geom_point(na.rm = TRUE)
nycflights13::flights
nycflights13::flights %>% mutate(cancelled = is.na(dep_time),
                                 sched_hour = sched_dep_time %/% 100,
                                 sched_min = sched_dep_time %% 100,
                                 sched_dep_time = sched_hour + sched_min / 60) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(color = cancelled), binwidth = 1/4 )

mean(c(0,1,2,3,4,10, NA), na.rm = TRUE)
sum(c(0,1,2,3,4,10, NA), na.rm = TRUE)
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)
ggplot(diamonds) + geom_bar(mapping = aes(x = cut))
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) + geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot()
ggplot(data = mpg) + geom_boxplot(mapping = aes(
  x = reorder(class, hwy, FUN = median),
  y = hwy
))
ggplot(data = mpg) + geom_boxplot(mapping = aes(
  x = reorder(class, hwy, FUN = median),
  y = hwy
)) + coord_flip()
ggplot(data = diamonds) + geom_count(mapping = aes(x = cut, y = color))
diamonds %>% count(color, cut)
diamonds %>% count(color, cut) %>% ggplot(mapping = aes(x = color, y = cut)) + 
  geom_tile(mapping = aes(fill = n))
ggplot(data = diamonds) + geom_point(mapping = aes(x = carat, y = price))
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1/100)
ggplot(data = smaller) + geom_bin2d(mapping = aes(x = carat, y = price))
install.packages("hexbin")
ggplot(data = smaller) + geom_hex(mapping = aes(x = carat, y = price))
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))
ggplot(data = diamonds) + geom_point(mapping = aes(x = x, y = y )) + 
  coord_cartesian(xlim = c(4,11), ylim = c(4,11))
ggplot(data = faithful) + geom_point(mapping = aes(x = eruptions, y = waiting))
library(modelr)
mod <- lm(log(price) ~ log(carat), data = diamonds)
mod
diamonds2 <- diamonds %>% add_residuals(mod) %>% mutate(resid = exp(resid))
ggplot(data = diamonds2) + geom_point(mapping = aes(x = carat, y = resid))
ggplot(data = diamonds2) + geom_boxplot(mapping = aes(x = cut, y = resid))
ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_freqpoly(binwidth = 0.25)
ggplot(faithful, aes(eruptions)) + geom_freqpoly(binwidth = 0.25)
diamonds %>% count(cut, clarity) %>% ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile()
