library(tidyverse)

input <- readLines("./inputs/input5.txt")

# Part 1
split_index <- which(input == "")
orders_raw <- input[1:(split_index - 1)]
numbers_raw <- input[(split_index + 1):length(input)]

numbers <- numbers_raw %>% str_split(pattern = ",")

bad_rows <- c()
for(i in 1:length(numbers)){
  # i <- 4
  array <- numbers[[i]]
  for(j in length(array):2){
    # j <- 2
    ## reverse the actual order to see if it is in the list of right orders, if so, it violates it
    fails <- 
      expand.grid(array[j], array[1:(j-1)]) %>% 
      mutate(check_order = paste0(Var1,"|", Var2)) %>%
      filter(check_order %in% orders_raw) %>% 
      nrow()
    
    if(fails !=0){
      bad_rows <- unique(c(bad_rows,i))
    }
  }
}
bad_rows

good_rows <- numbers[-bad_rows]

get_middle <- function(vec) {
  vec[ceiling(length(vec) / 2)]
}

lapply(good_rows, get_middle) %>% unlist() %>% as.numeric() %>% sum()

## Part 2

bad_rows <- numbers[bad_rows]
still_finding_issues <- TRUE

while(still_finding_issues){
  still_finding_issues <- FALSE
  
  for(i in 1:length(bad_rows)){
    # i <- 4
    array <- bad_rows[[i]]
    for(j in length(array):2){
      for(l in (j-1):1){
        is_bad <- paste0(array[j],"|", array[l]) %>% grepl(x = orders_raw, fixed = T) %>% any()
        if(is_bad){
          still_finding_issues <- TRUE
          temp <- array[j]
          array[j] <- array[l]
          array[l] <- temp
        }
      }
      bad_rows[[i]] <- array
    }
  }
}

lapply(bad_rows, get_middle) %>% unlist() %>% as.numeric() %>% sum()
