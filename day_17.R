library(unglue)
library(tidyverse)
library(gmp)

input <- readLines("inputs/input17.txt")
input_split <- split(input, cumsum(input == ""))

registers <- unglue_data(input_split[[1]], "Register {name}: {value}") %>%
  mutate(value = as.integer(value)) %>%
  deframe() %>%
  as.list()

program <- input_split[[2]][2] %>%
  str_remove("Program: ") %>%
  str_split(",") %>%
  unlist() %>%
  as.integer()


eval_combo <- function(operand, registers) {
  if (operand %in% 0:3) return(operand)
  if (operand == 4) return(registers$A)
  if (operand == 5) return(registers$B)
  if (operand == 6) return(registers$C)
  return(-1)
}

execute_program <- function(initial_A, program) {
  registers <- list(A = initial_A, B = 0, C = 0)  
  output <- c()  
  instruction_pointer <- 1  
  
  while (instruction_pointer <= length(program)) {
    opcode <- program[instruction_pointer]
    operand <- program[instruction_pointer + 1]
    combo <- eval_combo(operand, registers)
    
    if (opcode == 0) {
      denominator <- 2^combo
      registers$A <- trunc(registers$A / denominator)
    } else if (opcode == 1) {
      registers$B <- bitXor(registers$B, operand)  
    } else if (opcode == 2) {
      registers$B <- combo %% 8
    } else if (opcode == 3 && registers$A != 0) {
      instruction_pointer <- operand + 1
      next
    } else if (opcode == 4) {
      registers$B <- bitXor(registers$B, registers$C) 
    } else if (opcode == 5) {
      output <- c(output, combo %% 8)
    } else if (opcode == 6) {
      denominator <- 2^combo
      registers$B <- trunc(registers$A / denominator)
    } else if (opcode == 7) {
      denominator <- 2^combo
      registers$C <- trunc(registers$A / denominator)
    }
    instruction_pointer <- instruction_pointer + 2  
  }
  return(output)
}

target_output <- program

## Part 1
paste(execute_program(registers$A, program), collapse = ",")

## This part of the code isn't needed but it was how I figured out the 8^x pattern

digits <- 2
max_length_reached <- 0
specific_test <-NULL
increment <- 0
repeat {
  A <- 1 + increment
  output <- execute_program(A, program)
  current_length <- length(output)
  last_output_digits <- output[max((current_length - digits + 1), 1):current_length]
  last_program_digits <- program[max((length(program) - digits + 1), 1):length(program)]
  if(!is.null(specific_test)){last_program_digits <- specific_test}
  if (current_length>max_length_reached && all(last_output_digits == last_program_digits)) {
    print(output)
    print(A)
    max_length_reached <- current_length
  }
  if(increment ==100000){break}
  increment <- increment + 1
}

## This actually solves it
current_test <- 0  
for(i in 0:(length(program)-1)){
  # i <- 12
  print(i)
  small_adjust <- 0
  
  working <- TRUE
  while(working){
    temp_test <- current_test * 8 + small_adjust
  
    result <- execute_program(temp_test, program)
    result <- na.omit(result)
    if (all(result == program[(length(program) - i):length(program)])) {
      current_test <- temp_test
      working <- FALSE
      print(small_adjust)
    }
    small_adjust <- small_adjust + 1
    # print(small_adjust)
  }
  
}
format(current_test, scientific = FALSE)
