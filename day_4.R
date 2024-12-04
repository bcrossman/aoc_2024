library(tidyverse)

input <- as_tibble((readLines("./inputs/input4.txt")))

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

## Part 1
straight_count <- function(data, group_var, arrange_var, arrange_desc = FALSE) {
  data %>%
    group_by({{group_var}}) %>%
    arrange(if (arrange_desc) desc({{arrange_var}}) else {{arrange_var}}) %>%
    filter(value == "X",
           lead(value, 1) == "M",
           lead(value, 2) == "A",
           lead(value, 3) == "S") %>%
    nrow()
}

right <- straight_count(data, rowid, colid, arrange_desc = FALSE)
left <- straight_count(data, rowid, colid, arrange_desc = TRUE)
down <- straight_count(data, colid, rowid, arrange_desc = FALSE)
up <- straight_count(data, colid, rowid, arrange_desc = TRUE)

diag_count <- function(df){
  df %>% 
    left_join(df %>% rename_with(~ paste0(., "_2")), c("diag_row" = "rowid_2", "diag_col" = "colid_2")) %>% 
    left_join(df %>% rename_with(~ paste0(., "_3")), c("diag_row_2" = "rowid_3", "diag_col_2" = "colid_3")) %>% 
    left_join(df %>% rename_with(~ paste0(., "_4")), c("diag_row_3" = "rowid_4", "diag_col_3" = "colid_4")) %>% 
    drop_na(value_4) %>% 
    filter(value=="X", value_2=="M", value_3=="A", value_4=="S") %>% 
    nrow()
}

down_right <- diag_count(data %>% mutate(diag_row = rowid+1, diag_col = colid+1))
down_left <- diag_count(data %>% mutate(diag_row = rowid+1, diag_col = colid-1))
up_right <- diag_count(data %>% mutate(diag_row = rowid-1, diag_col = colid+1))
up_left <- diag_count(data %>% mutate(diag_row = rowid-1, diag_col = colid-1))

right+left+down+up+down_right+down_left+up_right+up_left

##Part 2
df <- data %>% mutate(diag_row = rowid+1, diag_col = colid+1)
df2 <- data %>% mutate(diag_row = rowid-1, diag_col = colid+1)

df %>% 
  left_join(df %>% rename_with(~ paste0(., "_2")), c("diag_row" = "rowid_2", "diag_col" = "colid_2")) %>% 
  left_join(df %>% rename_with(~ paste0(., "_3")), c("diag_row_2" = "rowid_3", "diag_col_2" = "colid_3")) %>% 
  drop_na(value_3) %>% 
  filter((value=="M" & value_2=="A" & value_3=="S")|(value=="S" & value_2=="A" & value_3=="M")) %>% 
  mutate(new_row_start = rowid+2) %>% 
  left_join(df2 %>% rename_with(~ paste0(., "_x")), c("new_row_start" = "rowid_x", "colid" = "colid_x")) %>% 
  left_join(df2 %>% rename_with(~ paste0(., "_x_2")), c("diag_row_x" = "rowid_x_2", "diag_col_x" = "colid_x_2")) %>% 
  left_join(df2 %>% rename_with(~ paste0(., "_x_3")), c("diag_row_x_2" = "rowid_x_3", "diag_col_x_2" = "colid_x_3")) %>% 
  drop_na(value_x_3) %>% 
  filter((value_x=="M" & value_x_2=="A" & value_x_3=="S")|(value_x=="S" & value_x_2=="A" & value_x_3=="M")) %>% 
  nrow()
