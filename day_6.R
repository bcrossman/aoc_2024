library(tidyverse)

input <- as_tibble((readLines("./inputs/input6.txt")))

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

start <- data %>% filter(value=="^")

blocks <- data %>% filter(value=="#")
directions <- c("up", "right", "down", "left")

visited <- start
direction <- "up"
inside_grid <- TRUE


while(inside_grid){
  
  current <- visited %>% slice(n())
  
  if(direction == "up"){
    hit_block <- 
      blocks %>% 
      filter(rowid<current$rowid & colid == current$colid) %>% 
      arrange(desc(rowid)) %>% 
      slice(1)
    
    if(nrow(hit_block)==0){
      
      new_path <- data %>% 
        filter(rowid<current$rowid & colid == current$colid) %>% 
        filter(rowid>=min(data$rowid) & colid == current$colid) %>% 
        arrange(desc(rowid))
      inside_grid <- FALSE
    }else{
      new_path <- data %>% 
        filter(rowid<current$rowid & colid == current$colid) %>% 
        filter(rowid>hit_block$rowid & colid == current$colid) %>% 
        arrange(desc(rowid))
    }
  }
  if(direction == "down"){
    hit_block <- 
      blocks %>% 
      filter(rowid>current$rowid & colid == current$colid) %>% 
      arrange((rowid)) %>% 
      slice(1)
    
    if(nrow(hit_block)==0){
      
      new_path <- data %>% 
        filter(rowid>current$rowid & colid == current$colid) %>% 
        filter(rowid<=max(data$rowid) & colid == current$colid) %>% 
        arrange((rowid))
      inside_grid <- FALSE
    }else{
      new_path <- data %>% 
        filter(rowid>current$rowid & colid == current$colid) %>% 
        filter(rowid<hit_block$rowid & colid == current$colid) %>% 
        arrange((rowid))
    }
  }
  if(direction == "right"){
    hit_block <- 
      blocks %>% 
      filter(colid>current$colid & rowid == current$rowid) %>% 
      arrange((colid)) %>% 
      slice(1)
    
    if(nrow(hit_block)==0){
      
      new_path <- data %>% 
        filter(colid>current$colid & rowid == current$rowid) %>% 
        filter(colid<=max(data$colid) & rowid == current$rowid) %>% 
        arrange((colid))
      inside_grid <- FALSE
    }else{
      new_path <- data %>% 
        filter(colid>current$colid & rowid == current$rowid) %>% 
        filter(colid<hit_block$colid & rowid == current$rowid) %>% 
        arrange((colid))
    }
  }
  
  if(direction == "left"){
    hit_block <- 
      blocks %>% 
      filter(colid<current$colid & rowid == current$rowid) %>% 
      arrange(desc(colid)) %>% 
      slice(1)
    
    if(nrow(hit_block)==0){
      
      new_path <- data %>% 
        filter(colid<current$colid & rowid == current$rowid) %>% 
        filter(colid>=min(data$colid) & rowid == current$rowid) %>% 
        arrange(desc(colid))
      inside_grid <- FALSE
    }else{
      new_path <- data %>% 
        filter(colid<current$colid & rowid == current$rowid) %>% 
        filter(colid>hit_block$colid & rowid == current$rowid)%>% 
        arrange(desc(colid))
    }
  }
  direction <- directions[(match(direction, directions) %% length(directions)) + 1]
  visited <- visited %>% bind_rows(new_path)
}

visited %>% distinct_all() %>%  nrow()


## Part 2

library(future)
plan(multisession) 

no_loop <- TRUE
item <- which(data$key %in% (visited$key[-1]))
orig_data <- data
found_loop_maker <- 0

check_loop <- function(count) {
  data <- orig_data
  data$value[item[[count]]] <- "#"
  blocks <- data %>% filter(value=="#")
  
  hit_block_list <- data %>% slice(0)
  visited <- start
  direction <- "up"
  inside_grid <- TRUE
  while(inside_grid){
    
    current <- visited %>% slice(n())
    
    if(direction == "up"){
      hit_block <- 
        blocks %>% 
        filter(rowid<current$rowid & colid == current$colid) %>% 
        arrange(desc(rowid)) %>% 
        slice(1) %>% 
        mutate(direction = direction)
      
      hit_block_list <- hit_block_list %>% bind_rows(hit_block)
      if(nrow(unique(hit_block_list[, c("key", "direction")])) < nrow(hit_block_list)){
        found_loop_maker <- found_loop_maker+1
        inside_grid <- FALSE
      }
      
      if(nrow(hit_block)==0){
        
        new_path <- data %>% 
          filter(rowid<current$rowid & colid == current$colid) %>% 
          filter(rowid>=min(data$rowid) & colid == current$colid) %>% 
          arrange(desc(rowid))
        inside_grid <- FALSE
      }else{
        new_path <- data %>% 
          filter(rowid<current$rowid & colid == current$colid) %>% 
          filter(rowid>hit_block$rowid & colid == current$colid) %>% 
          arrange(desc(rowid))
      }
    }
    if(direction == "down"){
      hit_block <- 
        blocks %>% 
        filter(rowid>current$rowid & colid == current$colid) %>% 
        arrange((rowid)) %>% 
        slice(1) %>% 
        mutate(direction = direction)
      
      hit_block_list <- hit_block_list %>% bind_rows(hit_block)
      if(nrow(unique(hit_block_list[, c("key", "direction")])) < nrow(hit_block_list)){
        found_loop_maker <- found_loop_maker+1
        inside_grid <- FALSE
      }
      
      if(nrow(hit_block)==0){
        
        new_path <- data %>% 
          filter(rowid>current$rowid & colid == current$colid) %>% 
          filter(rowid<=max(data$rowid) & colid == current$colid) %>% 
          arrange((rowid))
        inside_grid <- FALSE
      }else{
        new_path <- data %>% 
          filter(rowid>current$rowid & colid == current$colid) %>% 
          filter(rowid<hit_block$rowid & colid == current$colid) %>% 
          arrange((rowid))
      }
    }
    if(direction == "right"){
      hit_block <- 
        blocks %>% 
        filter(colid>current$colid & rowid == current$rowid) %>% 
        arrange((colid)) %>% 
        slice(1) %>% 
        mutate(direction = direction)
      
      hit_block_list <- hit_block_list %>% bind_rows(hit_block)
      if(nrow(unique(hit_block_list[, c("key", "direction")])) < nrow(hit_block_list)){
        found_loop_maker <- found_loop_maker+1
        inside_grid <- FALSE
      }
      
      if(nrow(hit_block)==0){
        
        new_path <- data %>% 
          filter(colid>current$colid & rowid == current$rowid) %>% 
          filter(colid<=max(data$colid) & rowid == current$rowid) %>% 
          arrange((colid))
        inside_grid <- FALSE
      }else{
        new_path <- data %>% 
          filter(colid>current$colid & rowid == current$rowid) %>% 
          filter(colid<hit_block$colid & rowid == current$rowid) %>% 
          arrange((colid))
      }
    }
    
    if(direction == "left"){
      hit_block <- 
        blocks %>% 
        filter(colid<current$colid & rowid == current$rowid) %>% 
        arrange(desc(colid)) %>% 
        slice(1) %>% 
        mutate(direction = direction)
      
      hit_block_list <- hit_block_list %>% bind_rows(hit_block)
      if(nrow(unique(hit_block_list[, c("key", "direction")])) < nrow(hit_block_list)){
        found_loop_maker <- found_loop_maker+1
        inside_grid <- FALSE
      }
      
      if(nrow(hit_block)==0){
        
        new_path <- data %>% 
          filter(colid<current$colid & rowid == current$rowid) %>% 
          filter(colid>=min(data$colid) & rowid == current$rowid) %>% 
          arrange(desc(colid))
        inside_grid <- FALSE
      }else{
        new_path <- data %>% 
          filter(colid<current$colid & rowid == current$rowid) %>% 
          filter(colid>hit_block$colid & rowid == current$rowid)%>% 
          arrange(desc(colid))
      }
    }
    direction <- directions[(match(direction, directions) %% length(directions)) + 1]
    visited <- visited %>% bind_rows(new_path)
  }
  return(found_loop_maker)
}

results <- future_lapply(1:length(item), check_loop)

found_loop_maker_total <- sum(unlist(results))

plan(sequential)

print(found_loop_maker_total)
