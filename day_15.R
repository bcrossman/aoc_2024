library(tidyverse)
library(gganimate)

input <- (readLines("./inputs/input15.txt"))

split_index <- which(input == "")
map <- input[1:(split_index - 1)]
orders <- input[(split_index + 1):length(input)]
orders <- paste(orders, collapse = "")
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
reverse_grid <- function(df) {
  max_colids <- df %>%
    group_by(rowid) %>%
    summarize(max_colid = max(colid, na.rm = TRUE), .groups = "drop")
  
  reconstructed_df <- df %>%
    complete(rowid, colid = full_seq(c(1, max(df$colid, na.rm = TRUE)), 1), fill = list(value = ".")) %>%
    group_by(rowid) %>%
    summarize(
      value = paste(value[order(colid)], collapse = ""),
      .groups = "drop"
    ) %>%
    ungroup()
  
  return(reconstructed_df)
}


stuff <- create_grid(as_tibble(map) )
stuff <- stuff %>% filter(value!=".")
start <- stuff %>% filter(value=="@")

reverse_grid(stuff)
count <- 1
for(direction in str_split(orders, pattern = "")[[1]]){
  # print(direction)
  # print(count)

  # if(count == 12){
  #   print(reverse_grid(stuff))
  #   print(start)
  #   asd
  #   }
  if(direction == "^"){

    get_key_area <-
      stuff %>%
      filter(rowid <= start$rowid,
             colid == start$colid) %>%
      arrange(desc(rowid)) %>%
      mutate(space = abs(rowid-lag(rowid)+1)) %>%
      mutate(space =  cumsum(replace_na(space, 0))) %>%
      mutate(hash_flag = cumsum(lag(value == "#", default = FALSE))) %>%
      mutate(hash_flag = pmax(hash_flag, lag(space), na.rm = T)) %>%
      filter(hash_flag == 0) %>%
      mutate(rowid = pmax(last(rowid)+(n()-row_number()), rowid-1))

    stuff <-
      stuff %>%
      filter(!(key %in% get_key_area$key)) %>%
      bind_rows(
        get_key_area
      )

    start <- stuff %>% filter(value=="@")

  }

  if(direction == "v"){

    get_key_area <-
      stuff %>%
      filter(rowid >= start$rowid,
             colid == start$colid) %>%
      arrange((rowid)) %>%
      mutate(space = abs(rowid-lag(rowid)-1)) %>%
      mutate(space =  cumsum(replace_na(space, 0))) %>%
      mutate(hash_flag = cumsum(lag(value == "#", default = FALSE))) %>%
      mutate(hash_flag = pmax(hash_flag, lag(space), na.rm = T)) %>%
      filter(hash_flag == 0) %>%
      mutate(rowid = pmin(last(rowid)-(n()-row_number()), rowid+1))

    stuff <-
      stuff %>%
      filter(!(key %in% get_key_area$key)) %>%
      bind_rows(get_key_area)

    start <- stuff %>% filter(value=="@")
  }
  if(direction == ">"){

    get_key_area <-
      stuff %>%
      filter(rowid == start$rowid,
             colid >= start$colid) %>%
      arrange((colid)) %>%
      mutate(space = colid-lag(colid)-1) %>%
      mutate(space =  cumsum(replace_na(space, 0))) %>%
      mutate(hash_flag = cumsum(lag(value == "#", default = FALSE))) %>%
      mutate(hash_flag = pmax(hash_flag, lag(space), na.rm = T)) %>%
      filter(hash_flag == 0) %>%
      mutate(colid = pmin(last(colid)-(n()-row_number()), colid+1))

    stuff <-
      stuff %>%
      filter(!(key %in% get_key_area$key)) %>%
      bind_rows(get_key_area)

    start <- stuff %>% filter(value=="@")
  }

  if(direction == "<"){

    get_key_area <-
      stuff %>%
      filter(rowid == start$rowid,
             colid <= start$colid) %>%
      arrange(desc(colid)) %>%
      mutate(space = abs(colid-lag(colid)+1)) %>%
      mutate(space =  cumsum(replace_na(space, 0))) %>%
      mutate(hash_flag = cumsum(lag(value == "#", default = FALSE))) %>%
      mutate(hash_flag = pmax(hash_flag, lag(space), na.rm = T)) %>%
      filter(hash_flag == 0) %>%
      mutate(colid = pmax(last(colid)+(n()-row_number()), colid-1))

    stuff <-
      stuff %>%
      filter(!(key %in% get_key_area$key)) %>%
      bind_rows(get_key_area)

    start <- stuff %>% filter(value=="@")
  }
  # print(reverse_grid(stuff))
  count= count+1
}

print(reverse_grid(stuff))
stuff %>%
  filter(value == "O") %>%
  mutate(score = (rowid-1)*100+(colid-1)) %>%
  pull(score) %>%
  sum()

## Part II

stuff <- create_grid(as_tibble(map) )
stuff2 <-
  stuff %>%
  mutate(colid = colid*2) %>%
  mutate(value = if_else(value == "O", "]", value)) %>%
  mutate(value = if_else(value == "@", ".", value))

stuff1 <- stuff %>% mutate(colid = colid*2-1) %>% mutate(value = if_else(value == "O", "[", value))

stuff <- bind_rows(stuff1, stuff2) %>% mutate(key = paste(colid, rowid, sep=", "))

stuff <- stuff %>% filter(value!=".")
start <- stuff %>% filter(value=="@")

reverse_grid(stuff)
count <- 1
for(direction in str_split(orders, pattern = "")[[1]]){
  print(direction)
  print(count)
  print(str_length(orders))
  # if(count == 349){
  #   print(reverse_grid(stuff),n =500)
  #   print(start)
  #   asd
  # }
  if(direction == "^"){
    
    still_looking <- T
    row_range <- start$rowid
    col_range <- start$colid
    depth <- 0
    get_key_area_list <- list()
    while(still_looking){
      depth=depth+1
      next_value <- 
        stuff %>% 
        filter(rowid %in% (row_range-depth) & colid %in% col_range)
      
      if(nrow(next_value)==0){
        still_looking <- F
        result = -1 ## add to row, will be 0 if any wall, and +1 if down
        get_key_area_list[[as.character(depth)]] <- next_value
        break
      }
      
      if(any(next_value$value=="#")){
        still_looking <- F
        result = 0 ## add to row, will be 0 if any wall, and +1 if down
        get_key_area_list[[as.character(depth)]] <- next_value
        break
      }
      right_side <- next_value %>% arrange(colid) %>% slice(n()) %>% pull(value)
      left_side <- next_value %>% arrange(colid) %>% slice(1) %>% pull(value)
      if(depth == 1){
        if(left_side == "["){
          col_range <- (min(next_value$colid)-0):(max(next_value$colid)+1)
        }
        if(right_side == "]"){
          col_range <- (min(next_value$colid)-1):(max(next_value$colid)+0)
        }
      }else{
        col_range <- min(next_value$colid):max(next_value$colid)
        if(right_side == "["){
          col_range <- c(col_range,(max(next_value$colid)+1))
        }
        if(left_side == "]"){
          col_range <- c(col_range,(min(next_value$colid)-1))
        }
      }
      get_key_area_list[[as.character(depth)]] <-  
        stuff %>% 
        filter(rowid %in% (row_range-(depth)) & colid %in% col_range)
    }
    
    get_key_area <- bind_rows(get_key_area_list) %>% bind_rows(start) %>% mutate(rowid= rowid+result)
    
    stuff <- 
      stuff %>% 
      filter(!(key %in% get_key_area$key)) %>% 
      bind_rows(
        get_key_area
      )
    
    start <- stuff %>% filter(value=="@")
    
  }
  
  if(direction == "v"){
    
    still_looking <- T
    row_range <- start$rowid
    col_range <- start$colid
    depth <- 0
    get_key_area_list <- list()
    while(still_looking){
      depth=depth+1
      next_value <- 
        stuff %>% 
        filter(rowid %in% (row_range+depth) & colid %in% col_range)
      
      if(nrow(next_value)==0){
        still_looking <- F
        result = +1 ## add to row, will be 0 if any wall, and +1 if down
        get_key_area_list[[as.character(depth)]] <- next_value
        break
      }
      
      if(any(next_value$value=="#")){
        still_looking <- F
        result = 0 ## add to row, will be 0 if any wall, and +1 if down
        get_key_area_list[[as.character(depth)]] <- next_value
        break
      }
      right_side <- next_value %>% arrange(colid) %>% slice(n()) %>% pull(value)
      left_side <- next_value %>% arrange(colid) %>% slice(1) %>% pull(value)
      if(depth == 1){
      if(left_side == "["){
        col_range <- (min(next_value$colid)-0):(max(next_value$colid)+1)
      }
      if(right_side == "]"){
        col_range <- (min(next_value$colid)-1):(max(next_value$colid)+0)
      }
      }else{
        col_range <- min(next_value$colid):max(next_value$colid)
        if(right_side == "["){
          col_range <- c(col_range,(max(next_value$colid)+1))
        }
        if(left_side == "]"){
          col_range <- c(col_range,(min(next_value$colid)-1))
        }
      }
      get_key_area_list[[as.character(depth)]] <-  
        stuff %>% 
        filter(rowid %in% (row_range+(depth)) & colid %in% col_range)
    }
    
    get_key_area <- bind_rows(get_key_area_list) %>% bind_rows(start) %>% mutate(rowid= rowid+result)
    
    stuff <- 
      stuff %>% 
      filter(!(key %in% get_key_area$key)) %>% 
      bind_rows(
        get_key_area
      )
    
    start <- stuff %>% filter(value=="@")
  }
  if(direction == ">"){
    
    get_key_area <- 
      stuff %>% 
      filter(rowid == start$rowid, 
             colid >= start$colid) %>% 
      arrange((colid)) %>% 
      mutate(space = colid-lag(colid)-1) %>% 
      mutate(space =  cumsum(replace_na(space, 0))) %>% 
      mutate(hash_flag = cumsum(lag(value == "#", default = FALSE))) %>% 
      mutate(hash_flag = pmax(hash_flag, lag(space), na.rm = T)) %>% 
      filter(hash_flag == 0) %>% 
      mutate(colid = pmin(last(colid)-(n()-row_number()), colid+1))
    
    stuff <- 
      stuff %>% 
      filter(!(key %in% get_key_area$key)) %>% 
      bind_rows(get_key_area)
    
    start <- stuff %>% filter(value=="@")
  }
  
  if(direction == "<"){
    
    get_key_area <- 
      stuff %>% 
      filter(rowid == start$rowid, 
             colid <= start$colid) %>% 
      arrange(desc(colid)) %>% 
      mutate(space = abs(colid-lag(colid)+1)) %>% 
      mutate(space =  cumsum(replace_na(space, 0))) %>% 
      mutate(hash_flag = cumsum(lag(value == "#", default = FALSE))) %>% 
      mutate(hash_flag = pmax(hash_flag, lag(space), na.rm = T)) %>% 
      filter(hash_flag == 0) %>% 
      # mutate(last = last(colid)) %>% 
      # mutate(n = n()) %>% 
      # mutate(rownum = row_number()) %>% 
      mutate(colid = pmax(last(colid)+(n()-row_number()), colid-1))
    
    stuff <- 
      stuff %>% 
      filter(!(key %in% get_key_area$key)) %>% 
      bind_rows(get_key_area)
    
    start <- stuff %>% filter(value=="@")
  }
  # print(reverse_grid(stuff), n=50)
  count= count+1
}

print(reverse_grid(stuff), n = 100)
stuff %>% 
  filter(value == "[") %>% 
  mutate(score = (rowid-1)*100+(colid-1)) %>% 
  pull(score) %>% 
  sum()
