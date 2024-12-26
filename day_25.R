library(tidyverse)

# Prep

input <- tibble(value = readLines("inputs/input25.txt"))

locks_keys <- 
  input %>% 
  mutate(id = cumsum(value=="")) %>% 
  filter(value !="") %>% 
  group_by(id) %>% 
  mutate(rowid = row_number()) %>% 
  separate_rows(value, sep="", convert = TRUE) %>% 
  drop_na(value) %>% 
  filter(value != "") %>% 
  group_by(id, rowid) %>% 
  mutate(colid = row_number()) %>% 
  ungroup() %>% 
  mutate(key = paste(colid, rowid, sep=", ")) %>% 
  select(id, value, rowid, colid, key) %>% 
  group_by(id, rowid) %>% 
  mutate(Lock = all(rowid == 1 & value == "#")) %>% 
  group_by(id) %>% 
  mutate(Lock = any(Lock)) %>% 
  filter(value == "#")

id_combinations <- 
  locks_keys %>% 
  filter(Lock) %>% 
  select(id) %>% 
  distinct() %>% 
  rename(id_locks = id) %>% 
  tidyr::crossing(
    locks_keys %>% 
      filter(!Lock) %>% 
      select(id) %>% 
      distinct() %>% 
      rename(id_keys = id)
  )

id_combinations <- 
  id_combinations %>% 
  mutate(combined_id = paste(id_locks, id_keys, sep = ","))

stacked_combinations <- 
  id_combinations %>% 
  pivot_longer(
    cols = -combined_id,
    names_to = c(".value", "type"), 
    names_sep = "_"
  ) %>% 
  left_join(locks_keys)

stacked_combinations %>% 
  count(combined_id, key) %>% 
  group_by(combined_id) %>% 
  summarise(fit = all(n==1)) %>% 
  pull(fit) %>% 
  sum()
  
