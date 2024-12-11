library(tidyverse)

# Prep

input <- (readLines("inputs/input11.txt"))

data <- 
  as_tibble(input) %>% 
  separate_rows(value,sep =  " ", convert = T) %>% 
  count(value) 

for(blinks in 1:25){
  data <- data %>% 
    rowid_to_column()%>% 
    mutate(len = str_length(value)) 
  
  zeros <- data %>% filter(value==0)
  evens <- data %>% filter(len %% 2 ==0)
  remaining <- data %>% filter(!(rowid %in% c(zeros$rowid, evens$rowid)))
  
  recreated_data <- 
    bind_rows(list(
      zeros %>% mutate(value = 1),
      evens %>% mutate(value1 = as.integer(value %/% 10^(len/2)),
                       value2 = as.integer(value - value1*10^(len/2))) %>% 
        select(-value, -len) %>% 
        pivot_longer(
          cols = c(value1, value2),
          names_to = "part",
          values_to = "value"
        ) %>%
        select(n, value),
      remaining%>% mutate(value =  value*2024)
    ))
  data <- recreated_data %>%  
    group_by(value) %>%
    summarize(n = sum(n))
}


data$n %>% sum

## Part 2

data <- 
  as_tibble(input) %>% 
  separate_rows(value,sep =  " ", convert = T) %>% 
  count(value) 

for(blinks in 1:75){
  data <- data %>% 
    rowid_to_column()%>% 
    mutate(len = str_length(value)) 
  
  zeros <- data %>% filter(value==0)
  evens <- data %>% filter(len %% 2 ==0)
  remaining <- data %>% filter(!(rowid %in% c(zeros$rowid, evens$rowid)))
  
  recreated_data <- 
    bind_rows(list(
      zeros %>% mutate(value = 1),
      evens %>% mutate(value1 = as.integer(value %/% 10^(len/2)),
                       value2 = as.integer(value - value1*10^(len/2))) %>% 
        select(-value, -len) %>% 
        pivot_longer(
          cols = c(value1, value2),
          names_to = "part",
          values_to = "value"
        ) %>%
        select(n, value),
      remaining%>% mutate(value =  value*2024)
    ))
  data <- recreated_data %>%  
    group_by(value) %>%
    summarize(n = sum(n))
}

data$n %>% sum %>% as.character()
