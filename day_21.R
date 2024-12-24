library(tidyverse)
library(igraph)

# Functions
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
create_edge_points <- function(data) {
  
  edge_points <- data %>% 
    left_join(data %>% mutate(rowid = rowid-1),
              by = c("rowid","colid"),
              suffix = c("", "_v")) %>% 
    left_join(data %>% mutate(rowid = rowid+1),
              by = c("rowid","colid"),
              suffix = c("", "_^")) %>% 
    left_join(data %>% mutate(colid = colid+1),
              by = c("rowid","colid"),
              suffix = c("", "_<")) %>% 
    left_join(data %>% mutate(colid = colid-1),
              by = c("rowid","colid"),
              suffix = c("", "_>")) %>% 
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

movement <- c("789", "456", "123", "#0A", "", "#^A", "<v>")
# Use cumsum to create grouping indices based on empty strings
split_result <- split(movement, cumsum(movement == ""))

## Key Pad paths
keypad_data <- create_grid(as_tibble(split_result[[1]]), add_border = FALSE) %>% rename(map = value)

keypad <- keypad_data %>% filter(map !="#")
edge_points <- create_edge_points(keypad)
edge_list <- 
  edge_points %>% 
  select(start, end, map, direction) 
g <- graph_from_data_frame(edge_list, directed = TRUE)

## Calc shortest path for each start_end combination
keypad_paths <- 
  keypad %>% 
  tidyr::crossing(
    keypad %>% 
      select(map_2 = map, rowid_2 = rowid, colid_2 = colid, key_2 = key)) %>% 
  # filter(map != map_2) %>% 
  rowwise() %>% 
  mutate(
    paths = list(all_shortest_paths(g, from = key, to = key_2)$res), # Get all shortest paths
    path_data = list(
      map_dfr(seq_along(paths), ~ data.frame(
        path = names(paths[[.x]]),
        shortest_path_option = .x
      ))
    )
  ) %>% 
  rowid_to_column("num_routes") %>% 
  unnest(path_data) %>%
  select(-paths) %>% 
  group_by(num_routes, shortest_path_option) %>% 
  mutate(path_end = lead(path)) %>%  
  left_join(edge_list %>% select(-map), 
            by = c("path" = "start", "path_end" = "end")) %>% 
  mutate(direction = if_else(is.na(direction), "A", direction)) %>% 
  ungroup()

## Repeat for direction pad 

direction_data <- create_grid(as_tibble(split_result[[2]]), add_border = FALSE) %>% rename(map = value)

direction <- direction_data %>% filter(map !="#")
edge_points <- create_edge_points(direction)
edge_list <- 
  edge_points %>% 
  select(start, end, map, direction) 
g <- graph_from_data_frame(edge_list, directed = TRUE)

## Calc shortest path for each start_end combination
direction_paths <- 
  direction %>% 
  tidyr::crossing(
    direction %>% 
      select(map_2 = map, rowid_2 = rowid, colid_2 = colid, key_2 = key)) %>% 
  # filter(map != map_2) %>% 
  rowwise() %>% 
  mutate(
    paths = list(all_shortest_paths(g, from = key, to = key_2)$res), # Get all shortest paths
    path_data = list(
      map_dfr(seq_along(paths), ~ data.frame(
        path = names(paths[[.x]]),
        shortest_path_option = .x
      ))
    )
  ) %>% 
  rowid_to_column("num_routes") %>%  
  unnest(path_data) %>%
  select(-paths) %>% 
  group_by(num_routes, shortest_path_option) %>% 
  mutate(path_end = lead(path)) %>% 
  left_join(edge_list %>% select(-map), 
            by = c("path" = "start", "path_end" = "end")) %>% 
  mutate(direction = if_else(is.na(direction), "A", direction)) %>% 
  ungroup()

## Ok actually start problem
input <- (readLines("inputs/input21.txt"))

# input_adj <- paste0("A",input)

codes_orig <- 
  tibble(code = input) %>% 
  rowid_to_column("code_num") 

codes <- 
  codes_orig %>% 
  separate_rows(code, sep = "", convert = F) %>% 
  filter(code != "") %>% 
  group_by(code_num) %>% 
  mutate(code_start = lag(code, default = "A"),
         code_end = code) %>% 
  group_by(code_num) %>% 
  mutate(code_position = row_number()) %>% 
  ungroup() 

first_robot_movement <- 
  codes %>% #filter(code_num == 1 & code_position==1) %>% 
  left_join(keypad_paths %>% select(map, map_2, shortest_path_option , direction), 
            by =  c("code_start" = "map", "code_end" = "map_2"), 
            relationship = "many-to-many") %>% 
  group_by(code_num, code_position, shortest_path_option) %>% #View()
  mutate(direction_start_1 = lag(direction, default = "A"),
         direction_end_1 = direction) %>% #View()
  rename(direction_1 = direction,
         shortest_path_option_1 = shortest_path_option) %>% # View()
  group_by(code_num, code_position, shortest_path_option_1) %>% 
  mutate(direction_position_1 = row_number()) %>% 
  ungroup()

next_robot_movement <- 
  first_robot_movement %>% #filter(code_num == 1 & code_position==1) %>% 
  left_join(direction_paths %>% select(map, map_2, shortest_path_option , direction), 
            by =  c("direction_start_1" = "map", "direction_end_1" = "map_2"), 
            relationship = "many-to-many") %>% 
  group_by(code_num, code_position, shortest_path_option_1, 
           direction_position_1, shortest_path_option) %>% 
  mutate(direction_start_2 = lag(direction, default = "A"),
         direction_end_2 = direction) %>% #View()
  rename(direction_2 = direction,
         shortest_path_option_2 = shortest_path_option) %>% 
  group_by(code_num, code_position, shortest_path_option_1, direction_position_1, shortest_path_option_2) %>% 
  mutate(direction_position_2 = row_number()) %>% 
  ungroup()

my_movement <- 
  next_robot_movement %>% #filter(code_num == 1 & code_position==1) %>% 
  left_join(direction_paths %>% select(map, map_2, shortest_path_option , direction), 
            by =  c("direction_start_2" = "map", "direction_end_2" = "map_2"), 
            relationship = "many-to-many") %>% #View()
  group_by(code_num, code_position, shortest_path_option_1, 
           direction_position_1, shortest_path_option_2, 
           direction_position_2, shortest_path_option) %>% 
  mutate(direction_start_3 = lag(direction, default = "A"),
         direction_end_3 = direction) %>% 
  rename(direction_3 = direction,
         shortest_path_option_3 = shortest_path_option) %>% 
  group_by(code_num, code_position, shortest_path_option_1, direction_position_1, 
           shortest_path_option_2, direction_position_2, shortest_path_option_3) %>% 
  mutate(direction_position_3 = row_number()) %>% 
  ungroup()


my_movement %>% # filter(code_num == 1 & code_position !=-1) %>% #View()
  ungroup() %>% 
  arrange(code_num, 
          code_position, 
          shortest_path_option_1, 
          direction_position_1, 
          shortest_path_option_2,
          direction_position_2,
          shortest_path_option_3) %>% 
  rowid_to_column("order") %>% #View()
  group_by(
    code_num,
    code_end,
    code_position, 
    shortest_path_option_1,
    direction_start_1,
    direction_end_1,
    direction_position_1, 
    shortest_path_option_2,
    direction_end_2,
    direction_position_2,
    shortest_path_option_3) %>% 
  summarise(n = n(),
            my_presses = paste0(direction_end_3, collapse = ""),
            order  = mean(order)) %>% #View()
  slice_min(n, n = 1, with_ties = FALSE) %>% 
  arrange(order) %>% 
  {assign("save_map_later_1", ., envir = .GlobalEnv)} %>%
  group_by(
    code_num, 
    code_end,
    code_position, 
    shortest_path_option_1,
    direction_start_1,
    direction_end_1,
    direction_position_1, 
    shortest_path_option_2) %>% # View()
  summarise(n = sum(n),
            robot_2_presses = paste0(direction_end_2, collapse = ""),
            my_presses = paste0(my_presses, collapse = ""),
            order  = mean(order)) %>% 
  slice_min(n, n = 1, with_ties = FALSE) %>% 
  arrange(order) %>% #View()
  group_by(
    code_num,
    code_end,
    code_position, 
    shortest_path_option_1) %>% #View()
  summarise(n = sum(n),
            robot_1_presses = paste0(direction_end_1, collapse = ""),
            robot_2_presses = paste0(robot_2_presses, collapse = ""),
            my_presses = paste0(my_presses, collapse = ""),
            order  = mean(order)) %>% 
  slice_min(n, n = 1, with_ties = FALSE) %>% 
  arrange(order) %>% 
  {assign("save_map_later_2", ., envir = .GlobalEnv)} %>%
  group_by(code_num) %>% 
  summarise(n = sum(n),
            key_pad_press = paste0(code_end, collapse = ""),
            robot_1_presses = paste0(robot_1_presses, collapse = ""),
            robot_2_presses = paste0(robot_2_presses, collapse = ""),
            my_presses = paste0(my_presses, collapse = ""),
            order  = mean(order)) %>% 
  mutate(key_pad_press = as.numeric(gsub("A","", key_pad_press))) %>% 
  mutate(score = n * key_pad_press) %>% 
  pull(score) %>% 
  sum(na.rm=T)

## Part two

# Create mapping of robot move and eventual moves 2 steps away

need_map_base <- 
  direction_data %>% filter(map != "#") %>% select(direction_start_1 = map) %>% 
  tidyr::crossing(direction_data %>% filter(map != "#") %>% select(direction_end_1 = map))


next_robot_movement_base <- 
  need_map_base %>% #filter(code_num == 1 & code_position==1) %>% 
  left_join(direction_paths %>% select(map, map_2, shortest_path_option , direction), 
            by =  c("direction_start_1" = "map", "direction_end_1" = "map_2"), 
            relationship = "many-to-many") %>% 
  group_by(shortest_path_option) %>%   #might need direction_position_1
  mutate(direction_start_2 = lag(direction, default = "A"),
         direction_end_2 = direction) %>% #View()
  rename(direction_2 = direction,
         shortest_path_option_2 = shortest_path_option) %>% 
  group_by(shortest_path_option_2) %>% 
  mutate(direction_position_2 = row_number()) %>% 
  ungroup()

my_movement_base <- 
  next_robot_movement_base %>% #filter(code_num == 1 & code_position==1) %>% 
  left_join(direction_paths %>% select(map, map_2, shortest_path_option , direction), 
            by =  c("direction_start_2" = "map", "direction_end_2" = "map_2"), 
            relationship = "many-to-many") %>% #View()
  group_by(shortest_path_option_2, 
           direction_position_2, shortest_path_option) %>% 
  mutate(direction_start_3 = lag(direction, default = "A"),
         direction_end_3 = direction) %>% 
  rename(direction_3 = direction,
         shortest_path_option_3 = shortest_path_option) %>% 
  group_by(
    shortest_path_option_2, direction_position_2, shortest_path_option_3) %>% 
  mutate(direction_position_3 = row_number()) %>% 
  ungroup()

needed_map <- 
  my_movement_base %>% # filter(code_num == 1 & code_position !=-1) %>% #View()
  ungroup() %>% 
  arrange(
    direction_start_1,
    direction_end_1,
    shortest_path_option_2,
    direction_position_2,
    shortest_path_option_3) %>% 
  rowid_to_column("order") %>% #View()
  group_by(
    direction_start_1,
    direction_end_1,
    shortest_path_option_2,
    direction_end_2,
    direction_position_2,
    shortest_path_option_3) %>% 
  summarise(n = n(),
            my_presses = paste0(direction_end_3, collapse = ""),
            order  = mean(order)) %>% #View()
  slice_min(n, n = 1, with_ties = FALSE) %>% 
  arrange(order) %>% 
  group_by(
    direction_start_1,
    direction_end_1,
    shortest_path_option_2) %>% # View()
  summarise(n = sum(n),
            robot_2_presses = paste0(direction_end_2, collapse = ""),
            my_presses = paste0(my_presses, collapse = ""),
            order  = mean(order)) %>% 
  slice_min(n, n = 1, with_ties = FALSE) %>% 
  arrange(order) 


## Build long term map 12 deep, then use it to jump 12 more
intermediate_step <- 
needed_map %>% select(direction_start_1, direction_end_1, my_presses) %>% 
  separate_rows(my_presses, sep = "") %>% 
  filter(my_presses != "") %>% 
  mutate(direction_start_3 = lag(my_presses, default = "A"),
         direction_end_3 = my_presses) %>% 
  select(-my_presses) %>% 
  left_join(needed_map %>% 
              select(direction_start_1, direction_end_1, my_presses) %>% 
              rename(direction_start_3 = direction_start_1,
                     direction_end_3 = direction_end_1)) %>% 
  separate_rows(my_presses, sep = "") %>% 
  filter(my_presses != "") %>% 
  mutate(direction_start_5 = lag(my_presses, default = "A"),
         direction_end_5 = my_presses) %>%
  select(-my_presses) %>% 
  left_join(needed_map %>% 
              select(direction_start_1, direction_end_1, my_presses) %>% 
              rename(direction_start_3 = direction_start_1,
                     direction_end_3 = direction_end_1)) %>%
  separate_rows(my_presses, sep = "") %>% 
  filter(my_presses != "") %>% 
  mutate(direction_start_5 = lag(my_presses, default = "A"),
         direction_end_5 = my_presses) %>% 
  select(-my_presses) %>% 
  left_join(needed_map %>% 
              select(direction_start_1, direction_end_1, my_presses) %>% 
              rename(direction_start_5 = direction_start_1,
                     direction_end_5 = direction_end_1)) %>%
  separate_rows(my_presses, sep = "") %>% 
  filter(my_presses != "") %>% 
  mutate(direction_start_7 = lag(my_presses, default = "A"),
         direction_end_7 = my_presses) %>% 
  select(-my_presses) %>% 
  left_join(needed_map %>% 
              select(direction_start_1, direction_end_1, my_presses) %>% 
              rename(direction_start_7 = direction_start_1,
                     direction_end_7 = direction_end_1)) %>%
  separate_rows(my_presses, sep = "") %>% 
  filter(my_presses != "") %>% 
  mutate(direction_start_9 = lag(my_presses, default = "A"),
         direction_end_9 = my_presses) %>% 
  select(-my_presses) %>% 
  left_join(needed_map %>% 
              select(direction_start_1, direction_end_1, my_presses) %>% 
              rename(direction_start_9 = direction_start_1,
                     direction_end_9 = direction_end_1)) %>%
  separate_rows(my_presses, sep = "") %>% 
  filter(my_presses != "") %>% 
  mutate(direction_start_11 = lag(my_presses, default = "A"),
         direction_end_11 = my_presses) %>% 
  select(-my_presses) %>% 
  left_join(needed_map %>% 
              select(direction_start_1, direction_end_1, my_presses) %>% 
              rename(direction_start_11 = direction_start_1,
                     direction_end_11 = direction_end_1)) %>%
  separate_rows(my_presses, sep = "") %>% 
  filter(my_presses != "") %>% 
  mutate(direction_start_13 = lag(my_presses, default = "A"),
         direction_end_13 = my_presses) %>% 
  select(-my_presses) 

length_multiplier <- 
  intermediate_step %>% group_by(direction_start_1, direction_end_1) %>% summarise(n = n())
  
intermediate_step_2 <- 
  intermediate_step %>% 
  left_join(length_multiplier %>% 
              select(direction_start_1, direction_end_1, n) %>% 
              rename(direction_start_13 = direction_start_1,
                     direction_end_13 = direction_end_1))

final_length_multiplier <- 
  intermediate_step_2 %>% group_by(direction_start_1, direction_end_1) %>% summarise(n = sum(n))
  

save_map_later_2 %>% select(code_num:shortest_path_option_1) %>% 
  left_join(save_map_later_1 %>% select(code_num:direction_position_2)) %>% #robot 1
  left_join(final_length_multiplier %>% select(direction_start_2 = direction_start_1, direction_end_2 = direction_end_1, n)) %>% 
  group_by(code_num, code_end) %>% View()
  summarise(
            key_pad_press = paste0(unique(code_end), collapse = ""),
            n = sum(n)) %>% 
  mutate(key_pad_press = as.numeric(gsub("A","", key_pad_press))) %>% 
  mutate(score = n * key_pad_press) %>% 
  pull(score) %>% 
  sum(na.rm = T) %>% as.character()
# 703226416812693>48020846524734
