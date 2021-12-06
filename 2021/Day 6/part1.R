library(dplyr)

input <- read.csv('Day 6/example.txt', header = FALSE)[1,] %>% as.integer()
input <- read.csv('Day 6/input.txt', header = FALSE)[1,] %>% as.integer()

day <- 0

while (day < 80) {
  day <- day + 1
  input <- input - 1
  reproducing_fish <- sum(input == -1)
  if (reproducing_fish >= 1){
    input[input == -1] <- 6
    input <- c(input, rep(8, times = reproducing_fish))
  }
}

print(paste0("Answer is ", length(input), " lanternfish"))
