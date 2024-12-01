library(tidyverse)
file <- readLines("./inputs/input1.txt")

## Part 1
df <- 
file %>% 
  tibble() %>% 
  separate(col = ".", into = c("v1", "v2"), sep = "   ", convert = T) 

df %>% 
  mutate(v1 = sort(v1),
         v2 = sort(v2),
         dif = abs(v1-v2)) %>%
  pull(dif) %>% 
  sum()
  
## Part 2
count_map <- 
  df %>% 
  count(v2)

df %>% 
  select(v1) %>% 
  left_join(count_map, by = c("v1"="v2")) %>% 
  mutate(id = v1*n) %>% 
  pull(id) %>% 
  sum(na.rm = T)


