library(dplyr)

#input <- read.csv("Day 2/example.txt", header = FALSE)[,1]
input <- read.csv("Day 2/input.txt", header = FALSE)[,1]

directions <- input %>% strsplit(" ") %>%
  do.call(rbind.data.frame, .) %>%
  `colnames<-`(c("direction", "change"))

directions$change <- as.numeric(directions$change)

directions <- directions %>% dplyr::group_by(direction) %>%
  dplyr::summarise(change = sum(change))

horizontal <- directions$change[directions$direction == "forward"]
vertical <- directions$change[directions$direction == "down"] - directions$change[directions$direction == "up"]

print(paste0("Answer is ", horizontal * vertical))
