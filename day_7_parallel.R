library(tidyverse)
library(future)
library(future.apply)
plan(multisession) 

input <- as_tibble((readLines("./inputs/input7.txt")))

`-` <- function(x, y) {x * y}

operations <- c(" + ", " - ")

##Part 1
loop1 <- function(count){
  input %>% 
    slice(count) %>% 
    separate(value, into = c("answer", "inputs"), sep = ": ") %>% 
    separate_rows(inputs, sep = " ") %>% 
    group_by(answer) %>%
    summarize(
      values = list(inputs),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      combinations = list(
        expand.grid(rep(list(operations), length(values) - 1), stringsAsFactors = FALSE) %>%
          asplit(1)
      ),
      result = list(
        map(combinations, ~ paste(values, .x, collapse = " "))
      )
    ) %>%
    unnest(result) %>%
    select(answer, result) %>% 
    mutate(result = unlist(result)) %>% 
    mutate(
      result = str_remove(result, "(?<=\\d)[^\\d]*$") # Remove everything after the last digit
    ) %>% 
    rowwise() %>% 
    mutate(value = eval(parse(text = result))) %>%
    group_by(answer) %>% 
    mutate(works = answer == value) %>% 
    summarise(works = any(works)) %>% 
    filter(works) %>% 
    pull(answer) %>% 
    as.numeric() %>% 
    sum()
}

results <- future_lapply(1:nrow(input), loop1)

sum(unlist(results))


##Part 2

loop2 <- function(count) {
  
  `%conc%` <- function(x, y) {as.numeric(paste0(x,y))}
  `%add%` <- function(x, y) {as.numeric(x+y)}
  `%mult%` <- function(x, y) {as.numeric(x*y)}
  
  operations <- c("%add%", "%mult%", "%conc%")
  
  input %>%
    slice(count) %>% 
    separate(value, into = c("answer", "inputs"), sep = ": ") %>% 
    separate_rows(inputs, sep = " ") %>% 
    group_by(answer) %>%
    summarize(
      values = list(inputs),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      combinations = list(
        expand.grid(rep(list(operations), length(values) - 1), stringsAsFactors = FALSE) %>%
          asplit(1)
      ),
      result = list(
        map(combinations, ~ paste(values, .x, collapse = " "))
      )
    ) %>%
    unnest(result) %>%
    select(answer, result) %>% 
    mutate(result = unlist(result)) %>% 
    mutate(
      result = str_remove(result, "(?<=\\d)[^\\d]*$") # Remove everything after the last digit
    ) %>% 
    rowwise() %>% 
    mutate(value = eval(parse(text = result))) %>% 
    group_by(answer) %>% 
    mutate(works = answer == value) %>% 
    summarise(works = any(works)) %>% 
    filter(works) %>% 
    pull(answer) %>% 
    as.numeric() %>% 
    sum()
}

results <- future_lapply(1:nrow(input), loop2)

sum(unlist(results)) %>% as.character()

plan(sequential)