library(tidyverse)
library(igraph)

# Prep

input <- (readLines("inputs/input16.txt"))

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

edge_list <- 
  edge_points %>% 
  filter(map != "#",
         map_end != "#") %>% 
  select(start, end, map, direction) 

straight_paths <- 
  edge_list %>% 
  mutate(start = paste(start, direction, sep = "_"),
         end = paste(end, direction, sep = "_"),
         weight = 1)

turns <- 
  data %>% 
  tidyr::crossing(direction = c("down", "up", "right", "left")) %>% 
  mutate(start = paste(key, direction, sep = "_")) %>% 
  select(key, start, map)

turn_paths <- 
  turns %>% left_join(turns %>% rename(end = start), relationship = "many-to-many") %>% 
  filter(start != end) %>% 
  mutate(weight = 1000) %>% 
  select(start, end, weight, map)

full_edge_list <- bind_rows(straight_paths, turn_paths)  

g <- graph_from_data_frame(full_edge_list, directed = TRUE)
beg <- full_edge_list %>% filter(map == "S" & direction == "right" ) %>% pull(start) %>% unique()
ends <- turns %>% filter(map == "E" ) %>% pull(start) ## multiple E
results <- list()

for (end in ends) {
  sp <- shortest_paths(g, from = beg, to = end, weights = E(g)$weights)
  path_weight <- distances(g, v = beg, to = end, weights = E(g)$weights)
  results[[end]] <- list(
    end = end,
    path = sp$vpath[[1]], 
    weight = path_weight
  )
}

path_weights <- sapply(results, function(res) res$weight)

min_weight <- min(path_weights)
min_weight

## Part II

results <- list()

for (end in min_ends) {
  sp <- all_shortest_paths(g, from = beg, to = end, weights = E(g)$weights)
  path_weight <- distances(g, v = beg, to = end, weights = E(g)$weights)

  for (i in seq_along(sp$res)) {
    results[[length(results) + 1]] <- list(
      end = as.character(end),
      path = sp$res[[i]],
      weight = path_weight
    )
  }
}

all_points <- map_dfr(results, ~ {
  list(
    end = .x$end,
    weight = .x$weight,                  
    points = list(V(g)[.x$path]$name)     
  )
})

all_points %>%
  unnest(points) %>%  
  separate(points, into= c("points", "direction"), sep = "_") %>%
  distinct(points)  %>% 
  nrow()
