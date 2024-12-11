library(tidyverse)

# Prep
data <- 
  as_tibble(readLines("inputs/input11.txt")) %>% 
  separate_rows(value, sep = " ", convert = TRUE) %>% 
  count(value)

blink_run <- function(data, blinks) {
  for (blink in seq_len(blinks)) {
    data <- data %>%
      mutate(
        len = str_length(value),
        group = case_when(
          value == 0 ~ "zeros",
          len %% 2 == 0 ~ "evens",
          TRUE ~ "remaining"
        )
      ) %>%
      group_by(group) %>%
      nest() %>% ungroup()
    
    data <- data %>%
      mutate(
        data = map2(data, group, ~{
          if (.y == "zeros") {
            .x %>% mutate(value = 1)
          } else if (.y == "evens") {
            .x %>%
              mutate(value1 = as.integer(value %/% 10^(len / 2)),
                     value2 = value %% 10^(len / 2)) %>%
              select(-value, -len) %>%
              pivot_longer(
                cols = c(value1, value2),
                names_to = "part",
                values_to = "value"
              ) %>%
              select(n, value)
          } else if (.y == "remaining") {
            .x %>% mutate(value = value * 2024)
          } else {
            .x
          }
        })
      ) %>%
      unnest(data) %>%
      select(-group)
    
    data <- data %>%
      group_by(value) %>%
      summarize(n = sum(n), .groups = "drop")
  }
  
  return(data$n %>% sum %>% as.character())
}

# Part 1
blink_run(data, 25)

# Part 2
blink_run(data, 75)