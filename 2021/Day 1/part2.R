library(dplyr)

#input <- read.csv("Day 1/part1_example.txt", header = FALSE) %>% `colnames<-`("current_depth")
input <- read.csv("Day 1/part1_input.txt", header = FALSE) %>% `colnames<-`("current_depth")


input$window1 <- 1:nrow(input)
input$window2 <- c(NA, 1:(nrow(input)-1))
input$window3 <- c(NA, NA, 1:(nrow(input)-2))

input <- rbind(input %>% dplyr::select(current_depth, window1) %>% dplyr::rename("window" = "window1"),
               input %>% dplyr::select(current_depth, window2) %>% dplyr::rename("window" = "window2"),
               input %>% dplyr::select(current_depth, window3) %>% dplyr::rename("window" = "window3"))

depth_change_tbl <- input %>%
  dplyr::filter(!is.na(window)) %>%
  dplyr::group_by(window) %>%
  dplyr::summarise(current_depth = sum(current_depth))

depth_change_tbl$previous_depth <- c(NA, depth_change_tbl$current_depth[1:(nrow(depth_change_tbl)-1)])

depth_change_tbl <-depth_change_tbl %>% dplyr::mutate(increase = dplyr::case_when(
  previous_depth < current_depth ~ TRUE
))

print(paste0("Answer is ",sum(depth_change_tbl$increase, na.rm=TRUE)))