library(tidyverse)
file <- readLines("./inputs/input3.txt")

## Part 1

mul <- function(x,y){x*y}

extracts <- str_extract_all(string = file,  pattern = "mul\\(\\d+,\\d+\\)") %>% unlist()

sum(sapply(extracts, function(x) eval(parse(text = x))))


## Part 2

extracts <- str_extract_all(
  string = file,
  pattern = "mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)"
) %>% unlist()

tibble(extracts) %>% 
  mutate(switch = case_when(
    extracts == "don't()" ~ 0,
    extracts == "do()" ~ 1,
    TRUE ~ NA_integer_)) %>% 
  fill(switch, .direction = "down") %>% 
  filter(grepl("mul", extracts)) %>%
  rowwise() %>% 
  mutate(value = switch*eval(parse(text = extracts))) %>% 
  ungroup() %>% 
  pull(value) %>% 
  sum()
