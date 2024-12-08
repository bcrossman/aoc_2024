library(tidyverse)

input <- as_tibble((readLines("./inputs/input7.txt")))

generate_values <- function(inputs, operations) {
  reduce(
    inputs[-1], # Start with the second input onward
    .init = inputs[1], # Initialize with the first input
    ~ unlist(map(operations, function(op) op(.x, .y))), # Apply all operations dynamically
    .dir = "forward"
  )
}

# Function to process data
process_input <- function(input, operations) {
  input %>%
    separate(value, into = c("answer", "inputs"), sep = ": ") %>%
    separate_rows(inputs, sep = " ") %>%
    group_by(answer) %>%
    summarize(
      values = list(as.numeric(inputs)),
      .groups = "drop"
    ) %>%
    mutate(
      results = map(values, ~ generate_values(.x, operations)),
      is_match = map2_lgl(results, as.numeric(answer), ~ .y %in% .x)
    ) %>%
    filter(is_match) %>%
    pull(answer) %>%
    as.numeric() %>%
    sum() %>% as.character()
}

# Part 1`
operations_part1 <- list(
  add = function(x, y) as.numeric(x + y),
  mult = function(x, y) as.numeric(x * y)
)

process_input(input, operations_part1)

# Part 2
operations_part2 <- list(
  add = function(x, y) as.numeric(x + y),
  mult = function(x, y) as.numeric(x * y),
  conc = function(x, y) as.numeric(paste0(x, y))
)

process_input(input, operations_part2)