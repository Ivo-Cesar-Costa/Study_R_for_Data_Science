install.packages("tidyverse")
library(tidyverse)
tidyverse_update()
install.packages(c("jsonlite", "pillar", "purrr", "readr", "rlang", "stringr"))
install.packages(c("nycflights13","gapminder","Lahman"))
1+2
library(tidyverse)
mpg
?mpg
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + geom_point(mapping = aes(x = hwy, y = cyl))
ggplot(data = mpg) + geom_point(mapping = aes(x = class, y = drv))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = class))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, stroke = displ))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ manufacturer, nrow = 2)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + geom_point(mapping = aes(x = drv, y = cyl))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ . )

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid( . ~ cyl )

ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv), show.legend = TRUE)
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv), show.legend = FALSE)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point() + geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point() + 
  geom_smooth(se = FALSE)

#1
ggplot() + geom_point(data = mpg, mapping = aes(x = displ, y = hwy, stroke = displ)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy), se = FALSE)

#2
ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy, stroke = displ)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy, group = drv), se = FALSE)

#3
ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy, stroke = displ, color = drv)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy, group = drv, color = drv), se = FALSE)

#4
ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy, stroke = displ, color = drv)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy), se = FALSE)

#5
ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy, color = drv, stroke = displ)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy, linetype = drv), se = FALSE)

#6
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, fill = drv)) + 
         geom_point(shape = 21, size = 4, color = "white", stroke = 2)

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))
ggplot(data = diamonds) + stat_count(mapping = aes(x = cut))
demo <- tribble(~a, ~b,
                "bar_1", 20,
                "bar_2", 30,
                "bar_3", 40
                )
ggplot(data = demo) + geom_bar(mapping = aes(x = a, y =b), stat = "identity")
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

ggplot(data = diamonds) + 
  stat_summary(mapping = aes(x = cut, y = depth), 
               fun.ymin = min,
               fun.ymax = max,
               fun.y = median)

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, y = ..prop..))
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = color, y = ..prop..))
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, color = cut))
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = cut))
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity))
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, color = clarity)) + 
  geom_bar(fill = NA, position = "identity")
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + geom_point()
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot() + coord_flip()

nz <- map_data("nz")
ggplot(nz, aes(long, lat, group = group)) + geom_polygon(fill = "white", color = "black")
ggplot(nz, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", color = "black") + coord_quickmap()

bar <- ggplot(data = diamonds) + geom_bar(mapping =  aes(x = cut, fill = cut),
                                          show.legend = FALSE,
                                          width = 1) + theme(aspect.ratio = 1) + 
  labs(x = NULL, y = NULL)
bar + coord_flip()
bar + coord_polar()

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1)) + 
  coord_polar()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + geom_point() + geom_abline() +
  coord_fixed()
