library(dplyr)

#input <- read.csv("Day 1/part1_example.txt", header = FALSE)[,1]
input <- read.csv("Day 1/part1_input.txt", header = FALSE)[,1]

depth_change_tbl <- tibble(`current_depth` = input, `previous_depth` = c(NA, input[1:(length(input)-1)]))

depth_change_tbl <-depth_change_tbl %>% dplyr::mutate(increase = dplyr::case_when(
  previous_depth < current_depth ~ TRUE
))

print(paste0("Answer is ",sum(depth_change_tbl$increase, na.rm=TRUE)))
