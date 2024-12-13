library(tidyverse)
library(unglue)

# Prep

input <- (readLines("inputs/input13.txt"))

patterns <- c(
  "Button A: X+{A_X}, Y+{A_Y}",
  "Button B: X+{B_X}, Y+{B_Y}",
  "Prize: X={Prize_X}, Y={Prize_Y}"
)

result <- unglue_data(
  input,
  patterns = patterns, convert = T
) %>% as.data.frame()

clean_df <- result %>%
  mutate(row_group = cumsum(!is.na(A_X))) %>% 
  group_by(row_group) %>%
  summarise(
    A_X = first(na.omit(A_X)),
    A_Y = first(na.omit(A_Y)),
    B_X = first(na.omit(B_X)),
    B_Y = first(na.omit(B_Y)),
    Prize_X = first(na.omit(Prize_X)),
    Prize_Y = first(na.omit(Prize_Y))
  ) %>%
  ungroup() %>%
  select(-row_group) 

expanded_df <- clean_df %>%
  crossing(multiplier = 1:180) %>% 
  mutate(
    A_X_scaled = A_X * multiplier,
    A_Y_scaled = A_Y * multiplier,
    A_Cost = 3 * multiplier
  )

expanded_df %>%
  crossing(multiplier_B = 1:180) %>%  
  mutate(
    B_X_scaled = B_X * multiplier_B,
    B_Y_scaled = B_Y * multiplier_B,
    B_Cost = 1 * multiplier_B
  ) %>% 
  select(
    A_X_scaled, A_Y_scaled, A_Cost, B_X_scaled, B_Y_scaled, B_Cost,
    Prize_X, Prize_Y, multiplier, multiplier_B) %>% 
  mutate(final_x = A_X_scaled + B_X_scaled,
         final_y = A_Y_scaled + B_Y_scaled,
         total_cost = A_Cost+B_Cost) %>% 
  filter(final_x == Prize_X, final_y == Prize_Y) %>% 
  group_by(Prize_X, Prize_Y) %>% 
  slice_min(total_cost) %>% 
  pull(total_cost) %>%
  sum()


## Part II
library(ROI)
library(ROI.plugin.glpk)
updated_df <- clean_df %>% 
  mutate(Prize_X = Prize_X + 10000000000000, 
         Prize_Y = Prize_Y + 10000000000000)

total_cost <- 0

for (i in 1:nrow(updated_df)) {
  # i <- 1
  costs <- c(3, 1) 
  XA <- updated_df$A_X[i] 
  YA <- updated_df$A_Y[i] 
  XB <- updated_df$B_X[i] 
  YB <- updated_df$B_Y[i]  
  X_goal <- updated_df$Prize_X[i]  
  Y_goal <- updated_df$Prize_Y[i]  
  
  constraints <- L_constraint(
    L = matrix(c(XA, XB, 
                 YA, YB), 
               nrow = 2, byrow = TRUE),
    dir = c("==", "=="), 
    rhs = c(X_goal, Y_goal) 
  )
  
  bounds <- V_bound(li = 1:2, lb = 0) 
  
  model <- OP(objective = L_objective(costs),  
              constraints = constraints, 
              bounds = bounds, 
              types = c("I", "I"))  
  
  solution <- ROI_solve(model, solver = "glpk")
  
  if (solution$status$code == 0) {  
    total_cost <- total_cost + solution$message$optimum
  } 
}

 total_cost %>% as.character()
  