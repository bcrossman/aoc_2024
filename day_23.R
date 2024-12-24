library(tidyverse)
library(igraph)

# Prep

input <- tibble(connections = readLines("inputs/input23.txt"))

base <- 
  input %>% 
  separate(connections, into = c("start", "end"), sep = "-")

all_connections <- 
  base %>% 
  bind_rows(base %>% rename(end = start, start = end))

#Part 1

all_connections %>% 
  filter(grepl("^t", start)) %>% 
  distinct(start, end) %>% 
  left_join(all_connections, by = c("end" = "start"), relationship = "many-to-many") %>% 
  distinct(start, end, end.y) %>% 
  filter(!grepl("^t", end.y)) %>% 
  left_join(all_connections, by = c("end.y" = "start"), relationship = "many-to-many") %>% 
  filter(end.y.y == start) %>% 
  select(start, end.x, end.y) %>% 
  rowwise() %>%
  mutate(sorted_row = list(sort(c_across(everything())))) %>%
  ungroup() %>%
  distinct(sorted_row) %>%
  nrow()


## Part 2

g <- graph_from_data_frame(base, directed = FALSE)
names(largest_cliques(g)[[1]]) %>% sort() %>% paste0(collapse = ",")

