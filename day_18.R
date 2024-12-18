library(tidyverse)
library(igraph)

create_edge_points <- function(data) {
  
  edge_points <- data %>% 
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
    rename(key_start = key,
           map_start = map) %>% 
    pivot_longer(cols = -c(rowid, colid),
                 names_to = c(".value", "direction"),
                 names_sep = "_") %>% 
    filter(direction != "start") %>% 
    rename(end = key,
           map_end = map) %>%
    mutate(start = paste(colid, rowid, sep=",")) %>% 
    left_join(data %>% select(key, map), by = c("start" = "key")) %>% 
    select(start, end, map, map_end, direction) %>% 
    drop_na(map_end) %>% 
    drop_na(map)
  
  return(edge_points)
}

# Prep

input <- (readLines("inputs/input18.txt"))

width <- 70
height <- 70
first_blocks <- 1024

base_map <- 
  tibble(colid = 0:(width)) %>% 
  tidyr::crossing(rowid = 0:(height)) %>% 
  mutate(key = paste(colid, rowid, sep=","),
         map = ".") %>% 
  select(map, rowid, colid, key)

blocks <- 
  tibble(key = input) %>% 
  separate(key, into = c("colid", "rowid"), sep=",", remove = F) %>% 
  rowid_to_column("order")

usable_map <- 
  base_map %>% 
  anti_join(blocks %>% slice(1:first_blocks), by = "key")

edge_points <- create_edge_points(usable_map)

edge_list <- 
  edge_points %>% 
  # filter(map != "#",
  #        map_end != "#") %>% 
  select(start, end, map, direction) 

g <- graph_from_data_frame(edge_list, directed = TRUE)
beg <- "0,0"
end <- paste(width,height,sep = ",")

sp <- shortest_paths(g, from = beg, to = end)
length(sp$vpath[[1]])-1

## Part 2

for(first_blocks in 1:nrow(blocks)){
  # first_blocks <- 3450
  print(first_blocks)
  usable_map <- 
    base_map %>% 
    anti_join(blocks %>% slice(1:first_blocks), by = "key")
  
  edge_points <- create_edge_points(usable_map)
  
  edge_list <- 
    edge_points %>% 
    select(start, end, map, direction) 
  
  g <- graph_from_data_frame(edge_list, directed = TRUE)
  beg <- "0,0"
  end <- paste(width,height,sep = ",")
  
  sp <- shortest_paths(g, from = beg, to = end)
  if(length(sp$vpath[[1]])==0){break}
}

blocks %>% slice(first_blocks) %>% pull(key)
# 
# reverse_grid <- function(df) {
#   max_colids <- df %>%
#     group_by(rowid) %>%
#     summarize(max_colid = max(colid, na.rm = TRUE), .groups = "drop")
#   
#   reconstructed_df <- df %>%
#     complete(rowid, colid = full_seq(c(1, max(df$colid, na.rm = TRUE)), 1), fill = list(map = "#")) %>%
#     group_by(rowid) %>%
#     summarize(
#       map = paste(map[order(colid)], collapse = ""),
#       .groups = "drop"
#     ) %>%
#     ungroup()
#   
#   return(reconstructed_df)
# }
# 
# reverse_grid(usable_map %>% 
#                mutate(map = if_else(key %in% names(sp$vpath[[1]]), "O", map)))
