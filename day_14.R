library(tidyverse)
library(unglue)

# Prep

input <- (readLines("inputs/input14.txt"))

patterns <- c(
  "p={x},{y} v={vx},{vy}"
)

wide <- 101
tall <- 103

robots <- unglue_data(
  input,
  patterns = patterns, convert = T
) %>% as.data.frame()

robots %>% 
  mutate(x = (x+vx*100) %% wide,
         y = (y+vy*100) %% tall) %>% 
  mutate(quad = case_when(
    x<((wide-1)/2) & y<((tall-1)/2) ~ 1,
    x>((wide-1)/2) & y<((tall-1)/2) ~ 2,
    x<((wide-1)/2) & y>((tall-1)/2) ~ 3,
    x>((wide-1)/2) & y>((tall-1)/2) ~ 4,
    TRUE ~ 5)) %>%
  filter(quad !=5) %>% 
  count(quad) %>% 
  pull(n) %>% 
  prod()

#Part II

good_seconds <- 
  robots %>% 
  rowid_to_column() %>% 
  crossing(seconds = c(1:15000)) %>%
  mutate(
    x = (x + vx*seconds) %% wide,  
    y = (y + vy*seconds) %% tall)  %>% 
  group_by(seconds) %>% 
    arrange(x,y) %>% 
    filter(y - lag(y,1,default=-1) == 1) %>% 
    arrange(y,x) %>% 
    filter(x - lag(x,1,default=-1) == 1) %>% 
    summarise(n = n()) %>% 
  arrange(desc(n), seconds)
  
p <- robots %>%
  rowid_to_column() %>%
  crossing(seconds = good_seconds$seconds[1]) %>%
  mutate(
    x = (x + vx * seconds) %% wide,  
    y = (y + vy * seconds) %% tall
  ) %>%
  ggplot(aes(x = x, y = -y)) +
  geom_point(size = 3, color = "green") +
  scale_color_viridis_d(guide = "none") +
  theme_minimal()

# Save the plot as an image
ggsave("day14_tree.png", plot = p, width = 8, height = 6, dpi = 300)

#  alternative method to look for dispersion
#  robots %>% 
#   rowid_to_column() %>% 
#   crossing(seconds = c(1:15000)) %>%
#   mutate(
#     x = (x + vx*seconds) %% wide,  
#     y = (y + vy*seconds) %% tall) %>% 
# group_by(seconds) %>%
#   summarize(
#     sd_x = sd(x, na.rm = TRUE),   
#     sd_y = sd(y, na.rm = TRUE),  
#     dispersion = sqrt(sd_x^2 + sd_y^2) 
#   ) %>% 
#   arrange((dispersion)) 

