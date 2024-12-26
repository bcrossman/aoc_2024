library(tidyverse)
library(igraph)

# Prep

input <- (readLines("inputs/input24.txt"))

split_index <- which(input == "")
initial_setting <- input[1:(split_index - 1)]
formulas <- input[(split_index + 1):length(input)]

named_inputs <- setNames(as.numeric(gsub(".*: ", "", initial_setting)), gsub(":.*", "", initial_setting))

keys <- gsub(".* -> ", "", formulas)  
values <- gsub(" ->.*", "", formulas)
values <- paste0("(", values, ")")
named_formulas <- setNames(values, keys)

full_lookup <- c(named_inputs, named_formulas)

full_lookup_start <- full_lookup
still_different <- TRUE
while(still_different){
  for(i in names(full_lookup)){
    # i <- names(named_formulas)[1]
    full_lookup <- gsub(pattern = i, replacement = full_lookup[[i]], x= full_lookup)
    
  }
  still_different <- identical(full_lookup, full_lookup_start)
  full_lookup_start <- full_lookup
}

`%AND%` <- function(x, y) {as.numeric(x&y)}
`%OR%` <- function(x, y) {as.numeric(x|y)}
`%XOR%` <- function(x, y) {as.numeric(x!=y)}

full_lookup_eval <- gsub(pattern = "AND", replacement = "%AND%", x = full_lookup_start, fixed = T)
full_lookup_eval <- gsub(pattern = "XOR", replacement = "%XOR%", x = full_lookup_eval, fixed = T)
full_lookup_eval <- gsub(pattern = " OR ", replacement = " %OR% ", x = full_lookup_eval, fixed = T)

full_lookup_eval <- gsub(" ", "", full_lookup_eval)

##Should have just been able to parse eval this all string...but again...too big ...so this->

pattern <- "\\((1|0)(%AND%|%XOR%|%OR%)(1|0)\\)"

for(j in 1:500){
  for(i in seq_along(full_lookup_eval[grepl("^z", names(full_lookup_eval))])) {
    key_expr <- full_lookup_eval[grepl("^z", names(full_lookup_eval))][[i]]
    while (grepl("\\(\\d\\)", key_expr)) {
      key_expr <- str_replace_all(key_expr, "\\((\\d)\\)", "\\1")
    }
    if(str_length(key_expr)==1){
      full_lookup_eval[grepl("^z", names(full_lookup_eval))][[i]] <- key_expr
      next}
    matches <- as_tibble(str_match_all(key_expr, pattern)[[1]])
    matches <- matches %>% 
      distinct(V1) %>% 
      rowwise() %>% 
      mutate(replace = as.numeric(eval(parse(text = V1)))) %>%  
      ungroup()
    
    replacements <- setNames(as.character(matches$replace), matches$V1)
    
    full_lookup_eval[grepl("^z", names(full_lookup_eval))][[i]] <- str_replace_all(key_expr, replacements)
    
  }
}

filtered_names <- names(full_lookup_eval)[grepl("^z", names(full_lookup_eval))]
sorted_filtered_names <- sort(filtered_names, decreasing = FALSE)
sorted_result <- as.numeric(full_lookup_eval[sorted_filtered_names])
binar_vec <- 2^(0:(length(sorted_result)-1))*sorted_result 
sum(binar_vec) %>% as.character()

#ugh too big for strtoi
# binary_to_decimal <- function(binary_string) {
#   sum(as.numeric(rev(strsplit(binary_string, "")[[1]])) * 2^(seq_along(binary_string) - 1))
# }
# binary_to_decimal(binary_result)
