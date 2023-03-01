mtcars[1:5, ]

knitr::kable(
  mtcars[1:5, ], 
  caption = "A knitr kable."
)

comma <- function(x) format(x, digits = 2, big.mark = ",")
comma(3452345)
comma(.12358124331)

reports <- tibble(
  class = unique(mpg$class),
  filename = stringr::str_c("fuel-economy-", class, ".html"),
  params = purrr::map(class, ~ list(my_class = .))
)
reports

reports %>% 
  select(output_file = filename, params) %>% 
  purrr::pwalk(rmarkdown::render, input = "fuel-economy.Rmd")
