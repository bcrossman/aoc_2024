library(tidyverse)
library(gganimate)

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

# g <- graph_from_data_frame(edge_list, directed = FALSE)
## Get Point Groups
output <- list()
group <- 1
checking_list <- edge_list %>% pull(start)
animation_log <- tibble(time = integer(), colid = numeric(), rowid = numeric(), group = integer())
parse_point <- function(point) {
  as.numeric(str_split(point, ", ", simplify = TRUE))
}

while(length(checking_list)>0){
  connected_edges <- checking_list[1]
  all_points_reachable <- c()
  while(length(connected_edges)>0){
    current_point <- connected_edges[1]
    new_connected_edges <- c(edge_list %>% filter(start == current_point) %>% pull(end), connected_edges[-1])
    connected_edges <- setdiff(new_connected_edges, all_points_reachable)
    all_points_reachable <- c(all_points_reachable, current_point)
    
    coords <- parse_point(current_point)
    colid <- coords[1]
    rowid <- coords[2]
  
    animation_log <- animation_log %>% 
      add_row(time = nrow(animation_log) + 1, 
              colid = colid, 
              rowid = rowid, 
              group = group)
  }
  checking_list <- setdiff(checking_list, all_points_reachable)
  output[[as.character(group)]] <- all_points_reachable
  group = group+1
}

edge_list <- edge_list %>%
  mutate(
    start_col = map_dbl(start, ~ parse_point(.x)[1]),
    start_row = map_dbl(start, ~ parse_point(.x)[2]),
    end_col = map_dbl(end, ~ parse_point(.x)[1]),
    end_row = map_dbl(end, ~ parse_point(.x)[2])
  )

animation_plot <- animation_log %>%
  ggplot(aes(x = colid, y = rowid, color = factor(group))) +
  geom_point(size = 2) +
  scale_color_viridis_d(guide = "none") + 
  theme_minimal() +
  labs(
    title = 'Group Assignment Animation: Frame {closest_state}',
    color = 'Group'
  ) +
  transition_states(time, transition_length = 0, state_length = 1) + 
  shadow_mark()

# Render and save as a GIF
animate(
  animation_plot, 
  nframes = 100, 
  fps = 10, 
  width = 800, 
  height = 600, 
  renderer = gifski_renderer("gradual_appearance_animation.gif")
)
