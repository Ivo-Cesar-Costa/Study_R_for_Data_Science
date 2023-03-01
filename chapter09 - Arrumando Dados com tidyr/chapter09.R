library(tidyverse)
table1
table2
table3
table4a
table4b
table1 %>% mutate(rate = cases / population * 10000)
table1 %>% count(year, wt = cases)
library(ggplot2)
ggplot(table1, aes(year, cases)) + geom_line(aes(group = country), color = "grey50") + 
  geom_point(aes(color = country))
table4a
table4a %>% gather(`1999`, `2000`, key = "year", value = "cases")
table4b %>% gather(`1999`, `2000`, key = "year", value = "population")
tidy4a <- table4a %>% gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>% gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b)
table2
spread(table2, key = type, value = count)
stocks <- tibble(
  year = c(2015,2015,2016,2016),
  half = c(1,2,1,2),
  return = c(1.88,0.59,0.92,0.17)
)
stocks %>% spread(year, return) %>% gather("year","return",`2015`:`2016`)
#table4a %>% gather(1999,2000,key = "year", value = "cases")
people <- tribble(
  ~name, ~key, ~value,
  #------/-----/------
  "Phillip Woods", "age", 45,
  "Phillip Woods", "height", 186,
  "Phillip Woods", "age", 50,
  "Jessica Cordero", "age", 37,
  "Jessica Cordero", "height", 156
)

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes", NA, 10,
  "no", 20, 12
)
table3
table3 %>% separate(rate, into = c("cases","population"))
table3 %>% separate(rate, into = c("cases","population"), sep = "/")
table3 %>% separate(rate, into = c("cases","population"), convert = TRUE)
table3 %>% separate(year, into = c("century","year"), sep = 2)
tidyr::table5
table5 %>% unite(new, century, year)
table5 %>% unite(new, century, year, sep = "")
tibble(x = c("a,b,c","d,e,f,g","h,i,j")) %>% separate(x, c("one","two","three"))
tibble(x = c("a,b,c","d,e","f,g,i")) %>% separate(x, c("one","two","three"))
stocks <- tibble(
  year = c(2015,2015,2015,2015,2016,2016,2016),
  qtr = c(1,2,3,4,2,3,4),
  return = c(1.88,0.59,0.35,NA,0.92,0.17,2.66)
)
stocks %>% spread(year,return)
stocks %>% spread(year,return) %>% gather(year,return,`2015`,`2016`,na.rm = TRUE)
stocks %>% complete(year, qtr)
treatment <-tribble(
  ~person, ~treatment, ~response,
  "Derrick Whitmore",1,7,
  NA,2,10,
  NA,3,9,
  "Katherine Burke",1,4
)
treatment %>% fill(person)
tidyr::who
who
who1 <- who %>% gather(
  new_sp_m014:newrel_f65, key = "key",
  value = "cases",
  na.rm = TRUE
)
who1
who1 %>% count(key)
who2 <- who1 %>% mutate(key = stringr::str_replace(key,"newrel","new_rel"))
who2
who3 <- who2 %>% separate(key, c("new","type","sexage"), sep = "_")
who3
who3 %>% count(new)
who4 <- who3 %>% select(-new, -iso2, -iso3)
who4
who5 <- who4 %>% separate(sexage, c("sex","age"), sep = 1)
who5
who %>% gather(code, value, new_ep_f014:newrel_f65, na.rm = TRUE) %>% 
  mutate(code = stringr::str_replace(code,"newrel","new_rel")) %>% 
  separate(code, c("new","var","sexage")) %>% select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex","age"), sep = 1)
