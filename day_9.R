library(tidyverse)
input <- (readLines("inputs/input9.txt"))

data <- 
  tibble(value = input) %>%
  separate_rows(value, sep = "") %>%
  filter(value != "") %>% 
  mutate(
    value = as.integer(value),
  ) %>% 
  rowid_to_column("index")

file_size <- data %>% filter(index %% 2 != 0)
free_space <- data %>% filter(index %% 2 == 0)

free_space_remaining <- free_space %>% mutate(assigned_index = NA_integer_)
output_list <- list()

# PART 1

for (i in nrow(file_size):1) {
  # i <- 10
  # if(i == 5282){asd}
  
  current_index <- file_size$index[i]
  translated_index <- (current_index-1)/2
  current_value <- file_size$value[i]
  # print(i)
  # print(current_index)
  # print(free_space_remaining$index[1])
  
  assigned_rows <- free_space_remaining %>%
    mutate(cumulative_sum = cumsum(value)) %>%
    filter(cumulative_sum <= current_value) %>% 
    mutate(assigned_index = current_index)
  
  if(nrow(assigned_rows)!=0 && max(assigned_rows$index)>current_index){
    
    assigned_rows <- free_space_remaining %>%
      filter(index<current_index) %>% 
      mutate(assigned_index = current_index)
    
    assigned_rows <- 
      assigned_rows %>% 
      bind_rows(
        last_file_size <- tibble(index = current_index, assigned_index = current_index, value = current_value- sum(assigned_rows$value))
      )
    output_list[[as.character(i)]] <- assigned_rows
    break}
  
  remaining_value <- current_value - sum(assigned_rows$value, na.rm = TRUE)
  
  if (remaining_value > 0) {
    partial_row <- free_space_remaining[nrow(assigned_rows) + 1, ]
    partial_row$value <- remaining_value
    partial_row$assigned_index <- current_index
    
    free_space_remaining <- free_space_remaining %>% filter(!(index %in% assigned_rows$index))
    
    free_space_remaining$value[1] <- free_space_remaining$value[1] - remaining_value
    assigned_rows <- bind_rows(assigned_rows, partial_row)
    
  }else{
    free_space_remaining <- free_space_remaining %>% filter(!(index %in% assigned_rows$index))
  }
  
  assigned_rows <- assigned_rows %>% select(-cumulative_sum)
  remaining_value_unprocessed <- current_value-sum(assigned_rows$value)
  
  output_list[[as.character(i)]] <- assigned_rows
  if(nrow(free_space_remaining)==0){break}
}
# 
# if(remaining_value_unprocessed>0){
# last_file_size <- tibble(index = current_index, value = remaining_value_unprocessed)}


used_free_space <- bind_rows(output_list)

df <- 
  file_size %>% 
  filter(!(index %in% used_free_space$assigned_index)) %>% 
  mutate(check_sum_1 = (index-1)/2) %>% 
  bind_rows(used_free_space) %>% 
  mutate(check_sum_2 = (assigned_index-1)/2) %>% 
  arrange(index) %>% 
  mutate(checksum = coalesce(check_sum_1, check_sum_2)) %>% 
  filter(value !=0) %>% 
  mutate(cumsum = cumsum(value)) %>% 
  mutate(
    lag_cumsum = lag(cumsum, default = 0),
    multiplier = map2(lag_cumsum + 1, cumsum, `:`)
  ) %>% #View()
  select(-lag_cumsum) %>% 
  unnest(multiplier) %>% 
  mutate(multiplier = multiplier - 1)

# df %>% View()

df %>%
  mutate(product = checksum * multiplier) %>% 
  mutate(cum_product = as.character(cumsum(product))) %>% #View()
  pull(product) %>%
  sum() %>% as.character()

## Part II
free_space_remaining <- free_space %>% mutate(assigned_index = NA_integer_)
output_list <- list()

for (i in nrow(file_size):1) {
  # i <- 10
  # if(i == 5282){asd}
  
  current_index <- file_size$index[i]
  translated_index <- (current_index-1)/2
  current_value <- file_size$value[i]
  # print(i)
  # print(current_index)
  # print(free_space_remaining$index[1])
  possible <- free_space_remaining %>%
    filter(value >= current_value) %>% 
    filter(index<current_index)
  
  if(nrow(possible)<1){next}
  
  assigned_rows <- possible %>%
    slice(1) %>% 
    mutate(assigned_index = current_index) 
  
  remaining_value <- sum(assigned_rows$value, na.rm = TRUE) - current_value
  
  if (remaining_value > 0) {
    free_space_remaining$value[free_space_remaining$index== assigned_rows$index] <- remaining_value
    assigned_rows$value <- current_value
    
    free_space_remaining <- 
      free_space_remaining %>% 
      bind_rows(
        tibble(index = current_index, value = current_value)) %>% 
      arrange(index)
  }else{
    free_space_remaining <- free_space_remaining %>% filter(!(index %in% assigned_rows$index)) 
    
    free_space_remaining <- 
      free_space_remaining %>% 
      bind_rows(
        tibble(index = current_index, value = current_value)) %>% 
      arrange(index)
  }
output_list[[as.character(i)]] <- assigned_rows
}
# 
# if(remaining_value_unprocessed>0){
# last_file_size <- tibble(index = current_index, value = remaining_value_unprocessed)}


used_free_space <- bind_rows(output_list)

df <- 
  file_size %>% 
  filter(!(index %in% used_free_space$assigned_index)) %>% 
  mutate(check_sum_1 = (index-1)/2) %>% 
  bind_rows(used_free_space) %>% 
  mutate(check_sum_2 = (assigned_index-1)/2) %>%
  mutate(index = as.numeric(index)) %>% 
  bind_rows(free_space_remaining %>% 
              mutate(check_sum_1 = 0) %>% 
              mutate(index = as.numeric(index)+.1)) %>% 
  arrange(index) %>% 
  mutate(checksum = coalesce(check_sum_1, check_sum_2)) %>% 
  filter(value !=0) %>% 
  mutate(cumsum = cumsum(value)) %>% 
  mutate(
    lag_cumsum = lag(cumsum, default = 0),
    multiplier = map2(lag_cumsum + 1, cumsum, `:`)
  ) %>% #View()
  select(-lag_cumsum) %>% 
  unnest(multiplier) %>% 
  mutate(multiplier = multiplier - 1)

df %>%
  mutate(product = checksum * multiplier) %>% 
  mutate(cum_product = as.character(cumsum(product))) %>% #View()
  pull(product) %>%
  sum() %>% as.character()

