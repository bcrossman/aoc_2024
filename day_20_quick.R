library(tidyverse)
library(igraph)

# Prep

input <- (readLines("inputs/input20.txt"))

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

## Shortest no cheats
edge_list <- 
  edge_points %>% 
  filter(map != "#",
         map_end != "#") %>% 
  select(start, end, map, direction) 

g <- graph_from_data_frame(edge_list, directed = TRUE)

beg <- data %>% filter(map == "S") %>% pull(key)
end <- data %>% filter(map == "E") %>% pull(key)
sp <- shortest_paths(g, from = beg, to = end)
# no_cheat_time <- length(sp$vpath[[1]])-1
no_cheat_path <- sp$vpath[[1]]
path_names <- names(no_cheat_path)
path_points <- data %>% filter(key %in% path_names) 

teleport_time_saver <- function(distance_max, time_saved_min) {
  valid_teleports <- path_points %>% 
    select(-map) %>% 
    tidyr::crossing(
      path_points %>% 
        select(rowid_2 = rowid, colid_2 = colid, key_2 = key)
    ) %>% 
    mutate(
      distance = abs(rowid_2 - rowid) + abs(colid_2 - colid),
      track_spot_1 = match(key, path_names),
      track_spot_2 = match(key_2, path_names),
      orig_distance = track_spot_2 - track_spot_1,
      time_saved = orig_distance - distance
    ) %>% 
    filter(distance <= distance_max, time_saved >= time_saved_min) %>% 
    nrow()
  
  return(valid_teleports)
}


##Part I
teleport_time_saver(2, 100)


##Part II
teleport_time_saver(2, 100)
