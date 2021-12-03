library(purrr)

#input <- read.csv("Day 3/example.txt", header = FALSE, colClasses = c("V1" = "character"))[,1]
input <- read.csv("Day 3/input.txt", header = FALSE, colClasses = c("V1" = "character"))[,1]

report <- input %>% strsplit("") %>%
  do.call(rbind.data.frame, .)

report_length <- nrow(report)

report <- purrr::map_dfc(report, function(x){as.numeric(x)})

report <- colSums(report)

gamma <- report > report_length/2
epsilon <- !gamma

gamma <- paste0(as.character(as.numeric(gamma)), collapse = "")
epsilon <- paste0(as.character(as.numeric(epsilon)), collapse = "")

paste0("Answer is ", strtoi(gamma, base = 2) * strtoi(epsilon, base = 2))
