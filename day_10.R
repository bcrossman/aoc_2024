library(tidyverse)
library(igraph)

# Prep

input <- (readLines("inputs/input10.txt"))

create_grid <- function(df){
  
  df %>% 
    rowid_to_column() %>%
    separate_rows(value, sep="", convert = T) %>% 
    drop_na(value) %>% 
    filter(value != "") %>% 
    mutate(rowid = rowid) %>% 
    group_by(rowid) %>% 
    mutate(colid = row_number()) %>% 
    ungroup() %>% 
    mutate(key = paste(colid, rowid, sep=", ")) %>% 
    select(value, rowid, colid, key)
}

data <- create_grid(as_tibble(input)) %>% rename(map = value)

##  Join edges
edge_points <- 
  data %>% 
  left_join(data %>% mutate(rowid = rowid-1),
            by = c("rowid","colid"),
            suffix = c("", "_down")) %>% 
  left_join(data %>% mutate(rowid = rowid+1),
            by = c("rowid","colid"),
            suffix = c("", "_up")) %>% 
  left_join(data %>% mutate(colid = colid+1),
            by = c("rowid","colid"),
            suffix = c("", "_left")) %>% 
  left_join(data %>% mutate(colid = colid-1),
            by = c("rowid","colid"),
            suffix = c("", "_right")) %>% 
  rename(key_start= key,
         map_start = map) %>% 
  pivot_longer(cols = -c(rowid,colid),
               names_to = c(".value","direction"),
               names_sep = "_") %>% 
  filter(direction != "start") %>% 
  rename(end = key,
         map_end = map) %>%
  mutate(start = paste(colid, rowid, sep=", ")) %>% 
  left_join(data %>% select(key, map) , by = c("start"="key")) %>% 
  select(start, end, map, map_end, direction) %>% 
  drop_na(map_end) %>% 
  drop_na(map)

edge_list <- 
  edge_points %>% 
  filter(map_end-map == 1) %>% 
  select(start, end)

g <- graph_from_data_frame(d = edge_list, directed = TRUE)

# Part I
trail_head_score <- 0

for(beg in edge_points %>% filter(map == 0) %>% pull(start) %>% unique()) {
  # beg <- "3, 1"
  for(end in edge_points %>% filter(map == 9) %>% pull(start) %>% unique()) {
    # end <- "2, 1"
    shortest_path <- get.shortest.paths(g, from = beg, to = end)  
    if (length(shortest_path$vpath[[1]]) > 0) {  
      trail_head_score <- trail_head_score + 1
    }
  }
}

trail_head_score

# Part II

trail_head_score <- 0

for(beg in edge_points %>% filter(map == 0) %>% pull(start) %>% unique()) {
  for(end in edge_points %>% filter(map == 9) %>% pull(start) %>% unique()) {
    all_paths <- all_simple_paths(g, from = beg, to = end)
    trail_head_score <- trail_head_score + length(all_paths)
  }
}

trail_head_score
