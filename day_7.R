library(tidyverse)
input <- as_tibble((readLines("./inputs/input7.txt")))

`-` <- function(x, y) {x * y}

operations <- c(" + ", " - ")

##Part 1
input %>% 
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

##Part 2

`%conc%` <- function(x, y) {as.numeric(paste0(x,y))}
`%add%` <- function(x, y) {as.numeric(x+y)}
`%mult%` <- function(x, y) {as.numeric(x*y)}

operations <- c("%add%", "%mult%", "%conc%")

input %>% 
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
