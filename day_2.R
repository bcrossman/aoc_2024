library(tidyverse)
file <- readLines("./inputs/input2.txt")

## Part 1

tibble(value = file) %>% 
  rowid_to_column() %>% 
  separate_rows(value, sep = " ", convert = T) %>% 
  group_by(rowid) %>% 
  mutate(increasing_check = value>lag(value,1),
         decreasing_check = value<lag(value,1),
         dif_check = abs(value-lag(value,1))>=1 & abs(value-lag(value,1))<=3) %>% 
  group_by(rowid) %>% 
  summarise(increasing_check = all(increasing_check, na.rm = T),
            decreasing_check = all(decreasing_check, na.rm = T),
            dif_check = all(dif_check, na.rm = T)) %>% 
  mutate(safe = (increasing_check|decreasing_check) & dif_check) %>% 
  filter(safe) %>% 
  nrow()
  

## Part 2
safe <- 
tibble(value = file) %>% 
  rowid_to_column() %>% 
  separate_rows(value, sep = " ", convert = T) %>% 
  group_by(rowid) %>% 
  mutate(increasing_check = value>lag(value,1),
         decreasing_check = value<lag(value,1),
         dif_check = abs(value-lag(value,1))>=1 & abs(value-lag(value,1))<=3) %>% 
  group_by(rowid) %>% 
  summarise(increasing_check = all(increasing_check, na.rm = T),
            decreasing_check = all(decreasing_check, na.rm = T),
            dif_check = all(dif_check, na.rm = T)) %>% 
  mutate(safe = (increasing_check|decreasing_check) & dif_check) %>% 
  filter(safe) %>% 
  pull(rowid)

max_try <- 
tibble(value = file) %>% 
  rowid_to_column() %>% 
  filter(!(rowid %in% safe)) %>% 
  separate_rows(value, sep = " ", convert = T) %>% 
  group_by(rowid) %>% 
  mutate(id = row_number()) %>% 
  pull(id) %>% max()

for(i in 1:max_try){
  
  new_safe <- 
    tibble(value = file) %>% 
    rowid_to_column() %>% 
    filter(!(rowid %in% safe)) %>% 
    separate_rows(value, sep = " ", convert = T) %>% 
    group_by(rowid) %>% 
    slice(-i) %>% 
    mutate(increasing_check = value>lag(value,1),
           decreasing_check = value<lag(value,1),
           dif_check = abs(value-lag(value,1))>=1 & abs(value-lag(value,1))<=3) %>% 
    group_by(rowid) %>% 
    summarise(increasing_check = all(increasing_check, na.rm = T),
              decreasing_check = all(decreasing_check, na.rm = T),
              dif_check = all(dif_check, na.rm = T)) %>% 
    mutate(safe = (increasing_check|decreasing_check) & dif_check) %>% 
    filter(safe) %>% 
    pull(rowid)
  
  safe <- c(safe, new_safe)
  
}

length(safe)

