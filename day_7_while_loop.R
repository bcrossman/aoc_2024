library(tidyverse)

input <- as_tibble((readLines("./inputs/input7.txt")))

##Part 1

`add` <- function(x, y) {as.numeric(x+y)}
`mult` <- function(x, y) {as.numeric(x*y)}

df <- 
  input %>%
  separate(value, into = c("answer", "inputs"), sep = ": ") %>%
  separate_rows(inputs, sep = " ") %>%
  group_by(answer) %>%
  summarize(
    values = list(inputs),
    .groups = "drop"
  ) 

count <- 0
for(i in 1:nrow(df)){
  # i <- 1
  # print(i)
  inputs <- as.numeric(df$values[[i]])
  answer <- as.numeric(df$answer[[i]])
  current_values <- inputs[1]
  inputs <- inputs[-1]
  while(length(inputs)>0){
    current_values_add <- add(current_values, inputs[1])
    current_values_mult <- mult(current_values, inputs[1])
    current_values <- c(current_values_add, current_values_mult)
    inputs <- inputs[-1]
  }
  if(answer %in% current_values){
    count = count+answer
  }
  
}

print(count %>% as.character)  

##Part 2

`conc` <- function(x, y) {as.numeric(paste0(x,y))}
`add` <- function(x, y) {as.numeric(x+y)}
`mult` <- function(x, y) {as.numeric(x*y)}

df <- 
  input %>%
  separate(value, into = c("answer", "inputs"), sep = ": ") %>%
  separate_rows(inputs, sep = " ") %>%
  group_by(answer) %>%
  summarize(
    values = list(inputs),
    .groups = "drop"
  ) 

count <- 0
for(i in 1:nrow(df)){
  # i <- 1
  # print(i)
  inputs <- as.numeric(df$values[[i]])
  answer <- as.numeric(df$answer[[i]])
  current_values <- inputs[1]
  inputs <- inputs[-1]
  while(length(inputs)>0){
    current_values_add <- add(current_values, inputs[1])
    current_values_mult <- mult(current_values, inputs[1])
    current_values_conc <- conc(current_values, inputs[1])
    current_values <- c(current_values_add, current_values_mult, current_values_conc)
    inputs <- inputs[-1]
  }
  if(answer %in% current_values){
    count = count+answer
  }
  
}

print(count %>% as.character)    



