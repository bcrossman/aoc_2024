library(tidyverse)

#Prep

data <- 
  as_tibble(readLines("inputs/input11.txt")) %>% 
  separate_rows(value, sep = " ", convert = TRUE) %>% 
  count(value)

blink_run <- function(data, blinks) {
  for (blink in seq_len(blinks)) {
    data <- data %>% 
      rowid_to_column() %>% 
      mutate(len = str_length(value))
    
    zeros <- data %>% filter(value == 0)
    evens <- data %>% filter(len %% 2 == 0)
    remaining <- data %>% filter(!(rowid %in% c(zeros$rowid, evens$rowid)))
    
    recreated_data <- 
      bind_rows(list(
        zeros %>% mutate(value = 1),
        evens %>% mutate(value1 = as.integer(value %/% 10^(len / 2)),
                         value2 = value %% 10^(len / 2)) %>% 
          select(-value, -len) %>% 
          pivot_longer(
            cols = c(value1, value2),
            names_to = "part",
            values_to = "value"
          ) %>%
          select(n, value),
        remaining %>% mutate(value = value * 2024)
      ))
    
    data <- recreated_data %>%  
      group_by(value) %>%
      summarize(n = sum(n))
  }
  
  return(data$n %>% sum %>% as.character())
}

# Part 1
blink_run(data, 25)

# Part 2
blink_run(data, 75)