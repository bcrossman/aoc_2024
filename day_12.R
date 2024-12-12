library(tidyverse)
library(igraph)

# Prep

input <- (readLines("inputs/input12.txt"))

create_grid <- function(df, add_border = FALSE) {
  
  grid <- df %>% 
    rowid_to_column() %>%
    separate_rows(value, sep="", convert = TRUE) %>% 
    drop_na(value) %>% 
    filter(value != "") %>% 
    mutate(rowid = rowid) %>% 
    group_by(rowid) %>% 
    mutate(colid = row_number()) %>% 
    ungroup() %>% 
    mutate(key = paste(colid, rowid, sep=", ")) %>% 
    select(value, rowid, colid, key)
  
  if (add_border) {
    max_row <- max(grid$rowid)
    max_col <- max(grid$colid)
    
    # Add borders
    top_border <- tibble(value = "@", rowid = 0, colid = 1:(max_col + 2))
    bottom_border <- tibble(value = "@", rowid = max_row + 1, colid = 1:(max_col + 2))
    left_border <- tibble(value = "@", rowid = 1:max_row, colid = 0)
    right_border <- tibble(value = "@", rowid = 1:max_row, colid = max_col + 1)
    
    grid <- bind_rows(grid, 
                      top_border, 
                      bottom_border, 
                      left_border, 
                      right_border) %>%
      arrange(rowid, colid) %>%
      mutate(key = paste(colid, rowid, sep=", "))
  }
  
  return(grid)
}

data <- create_grid(as_tibble(input), add_border = FALSE) %>% rename(map = value)

create_edge_points <- function(data) {
  library(dplyr)
  library(tidyr)
  
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
    mutate(start = paste(colid, rowid, sep=", ")) %>% 
    left_join(data %>% select(key, map), by = c("start" = "key")) %>% 
    select(start, end, map, map_end, direction) %>% 
    drop_na(map_end) %>% 
    drop_na(map)
  
  return(edge_points)
}

edge_points <- create_edge_points(data)

edge_list <- 
  edge_points %>% 
  filter(map_end==map) %>% 
  select(start, end, map)

g <- graph_from_data_frame(edge_list, directed = FALSE)

output <- list()
group <- 1
checking_list <- edge_list %>% pull(start)
while(length(checking_list)>0){
  current_point <- checking_list[1]
  all_points_reachable <- names(subcomponent(g, current_point, mode = "all"))
  checking_list <- setdiff(checking_list, all_points_reachable)
  output[[as.character(group)]] <- all_points_reachable
  group = group+1
}

## Points with Edges
output_df <- output %>%
  imap_dfr(~ tibble(key = .x, group = as.integer(.y))) %>% 
  left_join(data)


## Part 1
Points_w_edges <- 
  output_df  %>% 
  left_join(edge_list %>% count(start) %>% mutate(outer_edges = 4-n) %>% rename(key = start)) %>% 
  group_by(map, group) %>% 
  summarise(area = n(),
            perimeter = sum(outer_edges))

single_points <- 
  data %>% 
  filter(!(key %in% output_df$key)) %>% 
  mutate(group = max(Points_w_edges$group) + row_number(),
         area = 1, perimeter = 4) %>% 
  select(map, group, area, perimeter)

Points_w_edges%>% 
  bind_rows(single_points) %>% 
  mutate(cost = area * perimeter) %>% 
  pull(cost) %>% 
  sum()

## Part II

data <- create_grid(as_tibble(input), add_border = TRUE) %>% rename(map = value)

edge_points <- create_edge_points(data)

get_sides_ready <- 
  edge_points %>% 
  filter(map_end != map) %>% 
  select(key = start, direction) %>% 
  left_join(output_df) %>% 
  drop_na(map)  #drops border group from calcs but leaves in as border for real groups

down_up <- 
  get_sides_ready %>% 
  filter(direction == "down" | direction == "up") %>% 
  arrange(group, rowid, direction, colid) %>% 
  group_by(group, direction, rowid) %>% 
  mutate(side_change = !(colid-1 == lag(colid,1, default = -1))) %>% 
  group_by(group, map) %>% 
  summarise(sides = sum(side_change))

left_right <- 
  get_sides_ready %>% 
  filter(direction == "left" | direction == "right") %>% 
  arrange(group, colid, direction, rowid) %>% 
  group_by(group, direction, colid) %>% 
  mutate(side_change = !(rowid-1 == lag(rowid,1, default = -1))) %>% 
  group_by(group, map) %>% 
  summarise(sides = sum(side_change))

sides <- bind_rows(down_up, left_right) %>% group_by(group, map) %>% summarise(sides = sum(sides))

Points_w_edges <- 
  output_df %>% 
  left_join(data) %>% 
  group_by(map, group) %>% 
  summarise(area = n()) %>% 
  left_join(sides)

single_points <- 
  data %>% 
  filter(!(key %in% output_df$key)) %>% 
  mutate(group = max(Points_w_edges$group) + row_number(),
         area = 1, sides = 4) %>% 
  select(map, group, area, sides) %>% 
  filter(map != "@")# get rid of border points

Points_w_edges%>% 
  bind_rows(single_points) %>% 
  mutate(cost = area * sides) %>% 
  pull(cost) %>% 
  sum()

