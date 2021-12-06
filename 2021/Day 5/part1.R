library(purrr)
library(plyr)
library(tidyverse)
#input <- read.delim('Day 5/example.txt', header = FALSE, sep = '-')
input <- read.delim('Day 5/input.txt', header = FALSE, sep = '-')

start_points <- strsplit(input$V1, ",") %>% 
  do.call(rbind.data.frame, .) %>%
  `colnames<-`(c('x1', 'y1'))
end_points <- strsplit(input$V2, ",") %>% 
  do.call(rbind.data.frame, .) %>%
  `colnames<-`(c('x2', 'y2'))
coords <- cbind(start_points, end_points)
coords$x2 <- gsub(">", "", coords$x2)
coords <- map_dfc(coords, function(x){as.integer(x)})
coords <- coords %>% 
  dplyr::filter(x1 == x2 | y1 == y2)
x_covered <- pmap(coords %>% dplyr::select(c('x1', 'x2')), function(x1, x2){x1:x2})
x_covered <- plyr::ldply(x_covered, rbind)
colnames(x_covered) <- paste0("x", 1:ncol(x_covered))
y_covered <- pmap(coords %>% dplyr::select(c('y1', 'y2')), function(y1, y2){y1:y2})
y_covered <- plyr::ldply(y_covered, rbind)
colnames(y_covered) <- paste0("y", 1:ncol(y_covered))
coords_covered <- cbind(x_covered, y_covered) %>%
  pivot_longer(cols = paste0("x", 1:ncol(x_covered)),
               names_to = "x_no",
               values_to = "x") %>%
  dplyr::filter(!is.na(x)) %>%
  pivot_longer(cols = paste0("y", 1:ncol(y_covered)),
               values_to = "y") %>%
  dplyr::filter(!is.na(y)) %>%
  dplyr::mutate(coords = paste0("(", x, ",", y, ")")) %>%
  dplyr::group_by(coords)
coverage_summary_hv <- coords_covered %>% dplyr::summarise(times_covered = n())

paste0("Answer is ", sum(coverage_summary_hv$times_covered >= 2))
