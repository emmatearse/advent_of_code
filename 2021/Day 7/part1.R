library(dplyr)
library(purrr)

#input <- read.csv('Day 7/example.txt', header = FALSE)[1,] %>% as.integer()
input <- read.csv('Day 7/input.txt', header = FALSE)[1,] %>% as.integer()

distance_range <- min(input):max(input)

fuel_costs <- purrr::map(distance_range, function(a){
  abs(input - a) %>% sum()
}) %>% unlist()

print(paste0("Answer is ", min(fuel_costs) %>% sum()))
