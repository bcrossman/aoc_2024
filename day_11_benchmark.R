library(tidyverse)
library(microbenchmark)
# Prep
data <- 
  as_tibble(readLines("inputs/input11.txt")) %>% 
  separate_rows(value, sep = " ", convert = TRUE) %>% 
  count(value)

blink_run <- function(data, blinks) {
  for (blink in seq_len(blinks)) {
    data <- data %>%
      mutate(len = str_length(value), 
             group = case_when(
               value == 0 ~ "zeros",
               len %% 2 == 0 ~ "evens",
               TRUE ~ "remaining"
             ))
    
    zeros <- data %>% filter(group == "zeros") %>% mutate(value = 1)
    
    evens <- data %>%
      filter(group == "evens") %>%
      mutate(
        value1 = as.integer(value %/% 10^(len / 2)),
        value2 = value %% 10^(len / 2)
      ) %>%
      select(-value, -len) %>%
      pivot_longer(
        cols = c(value1, value2),
        names_to = "part",
        values_to = "value"
      ) %>%
      select(n, value)
    
    remaining <- data %>%
      filter(group == "remaining") %>%
      mutate(value = value * 2024)
    
    data <- bind_rows(zeros, evens, remaining) %>%
      group_by(value) %>%
      summarize(n = sum(n))
  }
  
  return(data$n %>% sum %>% as.character())
}


blink_run_2 <- function(data, blinks) {
  for (blink in seq_len(blinks)) {
    data <- data %>%
      mutate(len = floor(log10(value))+1,  ##Per Sarah Harris, I could have used my base 10 idea below to come up with length
             group = case_when(
               value == 0 ~ "zeros",
               len %% 2 == 0 ~ "evens",
               TRUE ~ "remaining"
             ))
    
    zeros <- data %>% filter(group == "zeros") %>% mutate(value = 1)
    
    evens <- data %>%
      filter(group == "evens") %>%
      mutate(
        value1 = as.integer(value %/% 10^(len / 2)),
        value2 = value %% 10^(len / 2)
      ) %>%
      select(-value, -len) %>%
      pivot_longer(
        cols = c(value1, value2),
        names_to = "part",
        values_to = "value"
      ) %>%
      select(n, value)
    
    remaining <- data %>%
      filter(group == "remaining") %>%
      mutate(value = value * 2024)
    
    data <- bind_rows(zeros, evens, remaining) %>%
      group_by(value) %>%
      summarize(n = sum(n))
  }
  
  return(data$n %>% sum %>% as.character())
}

# Part 1
# blink_run(data, 25)

# Part 2
# blink_run(data, 75)

benchmark_results <- microbenchmark(
  Method1 = blink_run(data, 75),
  Method2 = blink_run_2(data, 75),
  times = 20 
)

benchmark_results

#Unit: milliseconds
#expr      min       lq     mean   median       uq      max neval
#Method1 708.6825 721.9882 730.5786 731.5521 734.7662 770.1641    20
#Method2 650.3303 665.8900 681.0032 670.2369 676.8711 832.1300    20
#> 681/730.57
#> [1] 0.9321489