
## Looked at Jonathan Paulson to understand how to do recursively https://www.youtube.com/watch?v=pSqvQiqOVO0

p1 <- 0
p2 <- 0

# Read the input file
D <- readLines("./inputs/input7.txt")

# Define the is_valid function
is_valid <- function(target, ns, p2) {
  # Base case: If ns has one element, check if it matches the target
  if (length(ns) == 1) {
    return(ns[1] == target)
  }
  
  # If ns becomes empty, return FALSE
  if (length(ns) == 0) {
    return(FALSE)
  }
  
  # Recursive checks for addition and multiplication
  if (length(ns) >= 2) {
    if (is_valid(target, c(ns[1] + ns[2], ns[-c(1, 2)]), p2)) {
      return(TRUE)
    }
    
    if (is_valid(target, c(ns[1] * ns[2], ns[-c(1, 2)]), p2)) {
      return(TRUE)
    }
    
    # Concatenation check (if p2 is TRUE)
    if (p2) {
      concatenated <- as.numeric(paste0(ns[1], ns[2]))
      if (!is.na(concatenated) && is_valid(target, c(concatenated, ns[-c(1, 2)]), p2)) {
        return(TRUE)
      }
    }
  }
  
  return(FALSE)
}

# Process each line in the data
for (line in trimws(D)) {
  # line <- trimws(D)[1]
  parts <- strsplit(line, ":")[[1]]
  target <- as.numeric(parts[1])
  ns <- as.numeric(unlist(strsplit(trimws(parts[2]), "\\s+")))
  
  if (is_valid(target, ns, p2 = FALSE)) {
    p1 <- p1 + target
  }
  
  if (is_valid(target, ns, p2 = TRUE)) {
    p2 <- p2 + target
  }
}

# Print the results
print(p1)
print(as.character(p2))
