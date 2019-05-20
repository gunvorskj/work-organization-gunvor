library(vegan)
library(tidyverse)

####reshaping data####
#make tidy data
#gather, spread

iris <- as_tibble(iris)
iris

#select
iris %>% select(Sepal.Length, Species)

iris %>% select(-Sepal.Width)
iris %>% select(Sepal.Length:Petal.Length)

iris %>% rename(sepal_length = Sepal.Length, spp = Species)
iris %>% rename(`sepal_length` = Sepal.Length)
iris %>% rename(`1` = Sepal.Length)

iris %>% filter(Sepal.Length > 5, Petal.Length < 2) %>%
 select(Species)

iris[iris$Sepal.Length > 5 & iris$Petal.Length < 2, "Species"]

iris %>% group_by(Species)

#mutate
iris %>% mutate(petal.area = Petal.Length * Petal.Width)
iris %>%  mutate(Species = toupper(Species))

iris %>% group_by(Species) %>% summarise (mean_petal_length = mean(Petal.Length), sd(Petal.Length))

iris %>% group_by(Species) %>% 
  mutate(mean_petal_length = mean(Petal.Length)) %>% 
ungroup() 

iris %>% arrange(Petal.Length)
iris %>% arrange(desc(Petal.Length))

iris %>% group_by(Species) %>%  arrange(Petal.Length) %>% slice(1:3)

iris %>% group_by(Species) %>% nest()

iris %>% group_by(Species) %>% 
  nest() %>% 
  mutate(mod = map(data, ~lm(Sepal.Length ~ Sepal.Width, data= . ))) %>% 
  mutate(coef = map(mod, broom::tidy)) %>% 
  unnest(coef)

iris %>% 
  rownames_to_column() %>% 
  gather(key = variable, value = measurement, -Species, -rowname ) %>% 
  group_by(Species, variable) %>% 
  summarise(mean = mean(measurement))

iris %>%
  rownames_to_column() %>% 
  gather(key = variable, value = measurement, -Species, -rowname) %>% 
  ggplot(aes(x = variable, y = measurement, fill = Species)) + geom_violin()
         