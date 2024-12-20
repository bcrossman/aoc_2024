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
no_cheat_time <- length(sp$vpath[[1]])-1
no_cheat_path <- sp$vpath[[1]]

##Part I

block_list <- data %>% filter(map == "#") %>% pull(key)

time_saved <- c()
for(cheat in block_list){
  # cheat <- block_list[1]
  actual_block_list <- setdiff(block_list, cheat)
  
  edge_list <- 
    edge_points %>% 
    filter(!(start %in% actual_block_list),
           !(end %in% actual_block_list)) %>% 
    select(start, end, map, direction) 
  
  g <- graph_from_data_frame(edge_list, directed = TRUE)
  
  sp <- shortest_paths(g, from = beg, to = end)
  cheat_time <- length(sp$vpath[[1]])-1
  saved_time = no_cheat_time-cheat_time
  time_saved <- c(time_saved, saved_time)
}


time_saved[time_saved>=100] %>% length()

## Part II

# time_saved <- c()
# count <- 0
# total_length <- length(no_cheat_path)
# for(cheat_start in names(no_cheat_path)){
#   # count <- count+1
#   # print(paste(count, "out of", total_length))
#   # cheat_start <- names(no_cheat_path)[1]
#   cheat <- data %>% filter(key == cheat_start)
#   
#      ending_stops <- 
#       data %>% 
#       filter(key %in%  names(no_cheat_path)) %>%  ## get valid path, have to return within 20 steps
#       mutate(distance = abs(rowid-cheat$rowid) + abs(colid-cheat$colid)) %>% 
#       filter(distance<=20) %>%
#        arrange(distance)
#      
#   for(i in 2:nrow(ending_stops)){   
#     
#   edge_list <- 
#     edge_points %>% 
#     filter(!(start %in% block_list),
#            !(end %in% block_list)) %>% 
#     bind_rows(tibble(start = cheat_start, end = ending_stops$key[i], map = ".", direction = "teleport")) %>% 
#     select(start, end, map, direction) 
#   
#   g <- graph_from_data_frame(edge_list, directed = TRUE)
#   
#   sp <- shortest_paths(g, from = beg, to = end)
#   cheat_time <- length(sp$vpath[[1]])-1+ending_stops$distance[i]-1
#   saved_time = no_cheat_time-cheat_time
#   time_saved <- c(time_saved, saved_time)
#   }
# }
# 
# time_saved %>% table()
# 
# time_saved[time_saved>=55] %>% length()


library(future)
library(future.apply)

plan(multisession, workers = parallel::detectCores() - 1)

process_cheat_start <- function(cheat_start, data, no_cheat_path, edge_points, block_list, beg, end, no_cheat_time) {
  cheat <- data %>% filter(key == cheat_start)
  
  ending_stops <- 
    data %>% 
    filter(key %in% names(no_cheat_path)) %>%  # Get valid paths
    mutate(distance = abs(rowid - cheat$rowid) + abs(colid - cheat$colid)) %>% 
    filter(distance <= 20) %>%
    arrange(distance)
  
  time_saved_local <- numeric()
  
  for (i in 2:nrow(ending_stops)) {
    edge_list <- 
      edge_points %>% 
      filter(!(start %in% block_list),
             !(end %in% block_list)) %>% 
      bind_rows(tibble(start = cheat_start, end = ending_stops$key[i], map = ".", direction = "teleport")) %>% 
      select(start, end, map, direction)
    
    g <- graph_from_data_frame(edge_list, directed = TRUE)
    sp <- shortest_paths(g, from = beg, to = end)
    
    cheat_time <- length(sp$vpath[[1]]) - 1 + ending_stops$distance[i] - 1
    saved_time <- no_cheat_time - cheat_time
    time_saved_local <- c(time_saved_local, saved_time)
  }
  
  return(time_saved_local)
}

time_saved_list <- future_lapply(
  names(no_cheat_path), 
  process_cheat_start, 
  data = data, 
  no_cheat_path = no_cheat_path, 
  edge_points = edge_points, 
  block_list = block_list, 
  beg = beg, 
  end = end, 
  no_cheat_time = no_cheat_time
)

time_saved <- unlist(time_saved_list)
time_saved[time_saved>=100] %>% length()
plan(sequential)
