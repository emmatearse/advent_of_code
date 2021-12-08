library(dplyr)
library(purrr)

#input <- read.csv('Day 7/example.txt', header = FALSE)[1,] %>% as.integer()
input <- read.csv('Day 7/input.txt', header = FALSE)[1,] %>% as.integer()

distance_range <- min(input):max(input)

fuel_costs <- purrr::map(distance_range, function(a){
  distance_diff <- abs(input - a)
  map(distance_diff, function(b){
    cumsum(0:b)[b+1]
  }) %>% unlist() %>% sum()
}) %>% unlist()

optimal_position <- distance_range[which(fuel_costs == min(fuel_costs))]

print(paste0("Answer is ", min(fuel_costs)))
