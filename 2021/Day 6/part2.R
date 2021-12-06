library(dplyr)
library(gmp)

#input <- read.csv('Day 6/example.txt', header = FALSE)[1,] %>% as.numeric()
input <- read.csv('Day 6/input.txt', header = FALSE)[1,] %>% as.integer()

day <- 0
x <- c(sum(input == 0),
       sum(input == 1),
       sum(input == 2),
       sum(input == 3),
       sum(input == 4),
       sum(input == 5),
       sum(input ==6)) %>% as.bigz()
x_less_1 <- x
x_less_2 <- x
x_less_3 <- x

while (day < 256) {
  new_x <- x
  new_x[((day + 2) %% 7) + 1] <- new_x[((day + 2) %% 7) + 1] + x_less_3[(day %% 7) + 1]
  x_less_3 <- x_less_2
  x_less_2 <- x_less_1
  x_less_1 <- x
  x <- new_x
  day <- day + 1
}

print(paste0("Answer is ", sum(x), " lanternfish"))
