library(dplyr)
library(purrr)

#input <- read.csv('Day 8/example.txt', header = FALSE, sep = "")
#patterns <- input[c(TRUE, FALSE),]
#outputs <- input[c(FALSE, TRUE),] %>% dplyr::select(1:4)
#input <- cbind(patterns, outputs)
input <- read.csv('Day 8/input.txt', header = FALSE, sep = "")

patterns <- input[1:10]
outputs <- input[12:15]

zero <- c("a", "b", "c", "e", "f", "g")
one <- c("c", "f")
two <- c("a", "c", "d", "e", "g")
three <- c("a", "c", "d", "f", "g")
four <- c("b", "c", "d", "f")
five <- c("a", "b", "d", "f", "g")
six <- c("a", "b", "d", "e", "f", "g")
seven <- c("a", "c", "f")
eight <- c("a", "b", "c", "d", "e", "f", "g")
nine <- c("a", "b", "c", "d", "f", "g")

output_count <- map_dfc(outputs, function(col){
  nchar(col)
})

valid_lengths <- c(length(one),
                   length(four),
                   length(seven),
                   length(eight))

unique_values_count <- map_dfc(output_count, function(col){
  sum(col %in% valid_lengths)
})

paste0("Answer is ", rowSums(unique_values_count))

  