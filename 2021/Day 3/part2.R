library(dplyr)

#input <- read.csv("Day 3/example.txt", header = FALSE, colClasses = c("V1" = "character"))[,1]
input <- read.csv("Day 3/input.txt", header = FALSE, colClasses = c("V1" = "character"))[,1]

report <- input %>% strsplit("") %>%
  do.call(rbind.data.frame, .)

report_length <- nrow(report)

report <- purrr::map_dfc(report, function(x){as.numeric(x)})

most_common <- colSums(report)
most_common <- most_common >= report_length/2
most_common <- as.numeric(most_common)

col_no <- 1
filtered_report <- report
while((nrow(filtered_report) > 1) & (col_no <= ncol(report))){
  filtered_report <- filtered_report[filtered_report[,col_no] == most_common[col_no],]
  most_common <- colSums(filtered_report)
  most_common <- most_common >= nrow(filtered_report)/2
  most_common <- as.numeric(most_common)
  col_no <- col_no + 1
}

oxygen_generator_rating <- paste0(as.character(as.numeric(filtered_report[1,])), collapse = "")

least_common <- colSums(report)
least_common <- least_common < report_length/2
least_common <- as.numeric(least_common)

col_no <- 1
filtered_report <- report
while((nrow(filtered_report) > 1) & (col_no <= ncol(report))){
  filtered_report <- filtered_report[filtered_report[,col_no] == least_common[col_no],]
  col_no <- col_no + 1
  least_common <- colSums(filtered_report)
  least_common <- least_common < nrow(filtered_report)/2
  least_common <- as.numeric(least_common)
}

CO2_scrubber_rating <- paste0(as.character(as.numeric(filtered_report[1,])), collapse = "")

paste0("Life support rating is ", strtoi(oxygen_generator_rating, base = 2)*strtoi(CO2_scrubber_rating, base = 2))
