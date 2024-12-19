library(tidyverse)

input <- readLines("inputs/input19.txt")
input_split <- split(input, cumsum(input == ""))

towels <- 
  tibble(towel = input_split[[1]]) %>% 
  separate_rows(towel, sep=", ") %>% 
  rowid_to_column() %>% 
  separate_rows(towel, sep = "") %>% 
  filter(towel !="") %>% 
  mutate(value = paste(towel, rowid,sep= "_")) %>% 
  group_by(rowid) %>% 
  mutate(position = row_number()) %>% 
  mutate(value = paste(value, position,sep= "_"))

full_towels <- 
  towels %>% 
  bind_rows(
    towels %>% 
      group_by(rowid) %>% 
      summarise(position = min(position)-1) %>% 
      mutate(towel = "start",
             value = paste(towel, rowid, sep="_"))
  ) %>% 
  bind_rows(
    towels %>% 
      group_by(rowid) %>% 
      summarise(position = max(position)+1) %>% 
      mutate(towel = "end",
             value = paste(towel, rowid, sep ="_"))
  ) %>% 
  arrange(rowid, position) %>% 
  ungroup()

edge_list_end_start <- 
  full_towels %>% 
  filter(towel == "end") %>% 
  select(start = value,
         map_start = towel) %>% 
  tidyr::crossing( full_towels %>% 
                     filter(towel == "start") %>% 
                     select(end = value,
                            map_end = towel))
word_edges <- 
  full_towels %>% 
  left_join(full_towels %>% mutate(position = position-1), by = c("rowid", "position")) %>% 
  select(start = value.x,
         end = value.y,
         map_start = towel.x,
         map_end = towel.y)

edge_list <- 
  bind_rows(edge_list_end_start, word_edges) %>% 
  drop_na(end)

# g <- graph_from_data_frame(edge_list, directed = TRUE)

patterns <-
  tibble(pattern = input_split[[2]])  %>% 
  filter(pattern != "") %>% 
  rowid_to_column() %>% 
  separate_rows(pattern, sep = "") %>% 
  filter(pattern !="") %>% 
  mutate(value = paste(pattern, rowid,sep= "_")) %>% 
  group_by(rowid) %>% 
  mutate(position = row_number())

full_patterns <- 
  patterns %>% 
  bind_rows(
    patterns %>% 
      group_by(rowid) %>% 
      summarise(position = min(position)-1) %>% 
      mutate(pattern = "start",
             value = paste(pattern, rowid, sep="_"))
  ) %>% 
  bind_rows(
    patterns %>% 
      group_by(rowid) %>% 
      summarise(position = max(position)+1) %>% 
      mutate(pattern = "end",
             value = paste(pattern, rowid, sep ="_"))
  ) %>% 
  arrange(rowid, position) %>% 
  ungroup()

works <- 0
combinations <- 0

for (word in 1:max(full_patterns$rowid)) {
  print(paste("Word", word, "Out of", max(full_patterns$rowid)))
  current_word <- full_patterns %>% filter(rowid == word)
  
  test_start <- "start"
  potential_paths <- edge_list %>% 
    filter(map_start == test_start, map_end == current_word$pattern[2]) %>%
    mutate(n = 1) 
  
  for (i in 3:length(current_word$pattern)) {
    test_end <- current_word$pattern[i]
    
    potential_paths <-
      potential_paths %>%
      left_join(edge_list, by = c("end" = "start"), relationship = "many-to-many") %>%
      select(map_start = end, end = end.y, map_end = map_end.y, n)
    
    if (test_end == "end") {
      if (nrow(potential_paths %>% filter(map_end == "end")) > 0) {
        works = works+1
        combinations <- combinations + sum(potential_paths %>% filter(map_end == "end") %>% pull(n) %>% sum())
      } else {
        break
      }
    }
    
    ends <-
      potential_paths %>%
      filter(map_end == "end") %>%
      left_join(edge_list, by = c("end" = "start"), relationship = "many-to-many") %>%
      select(map_start = end, end = end.y, map_end = map_end.y, n) %>%
      drop_na(end) %>%
      left_join(edge_list, by = c("end" = "start"), relationship = "many-to-many") %>%
      select(map_start = end, end = end.y, map_end = map_end.y, n) %>%
      drop_na(end)
    
    potential_paths <-
      potential_paths %>% filter(map_end != "end") %>%
      bind_rows(ends) %>%
      filter(map_end == test_end) %>% 
      group_by(end, map_end) %>%
      summarise(n = sum(n), .groups = "drop") #initially I didn't have the n's and just did distinct in part 1
  }
}

works
combinations %>% format(scientific = FALSE)