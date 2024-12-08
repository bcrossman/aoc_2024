library(tidyverse)
input <- as_tibble((readLines("inputs/input8.txt")))

create_grid <- function(df){
  
  df %>% 
    rowid_to_column() %>%
    separate_rows(value, sep="", convert = T) %>% 
    drop_na(value) %>% 
    filter(value != "") %>% 
    mutate(rowid = rowid) %>% 
    group_by(rowid) %>% 
    mutate(colid = row_number()) %>% 
    ungroup() %>% 
    mutate(key = paste(colid, rowid, sep=", ")) %>% 
    select(value, rowid, colid, key)
}

data <- create_grid(input) 

radios <- 
  data %>% 
  filter(value != ".") 

all_connections <- 
  crossing(radios, 
           radios %>% rename(rowid_2 = rowid, colid_2 = colid, key_2=key, value_2 = value)) %>%
  filter(value == value_2,
         key != key_2)

#Part 1

all_connections %>%  
  mutate(row_distance = rowid_2-rowid) %>% 
  mutate(col_distance = colid_2-colid) %>% 
  mutate(antinode_rowid = rowid+row_distance*2,
         antinode_colid = colid+col_distance*2) %>% 
  filter(antinode_rowid>=min(data$rowid),
         antinode_rowid<=max(data$rowid),
         antinode_colid>=min(data$colid),
         antinode_colid<=max(data$colid)) %>% 
  mutate(antinode_key = paste(antinode_colid, antinode_rowid, sep=", ")) %>% 
  # filter(!(antinode_key %in% radios$key)) %>%
  distinct(antinode_colid, antinode_rowid, antinode_key) %>% nrow()


# Part 2
output <- list()
for(i in 1:100){
  antinodes <- 
    all_connections %>%  
    mutate(row_distance = rowid_2-rowid) %>% 
    mutate(col_distance = colid_2-colid) %>% 
    mutate(antinode_rowid = rowid+row_distance*i,
           antinode_colid = colid+col_distance*i) %>% 
    filter(antinode_rowid>=min(data$rowid),
           antinode_rowid<=max(data$rowid),
           antinode_colid>=min(data$colid),
           antinode_colid<=max(data$colid)) %>% 
    mutate(antinode_key = paste(antinode_colid, antinode_rowid, sep=", ")) %>% 
    # filter(!(antinode_key %in% radios$key)) %>%
    distinct(antinode_colid, antinode_rowid, antinode_key)
  output[[as.character(i)]] <- antinodes
}

output %>% 
  bind_rows() %>% 
  distinct(antinode_colid, antinode_rowid, antinode_key) %>% 
  nrow()
