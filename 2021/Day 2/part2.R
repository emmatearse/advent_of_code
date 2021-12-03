library(dplyr)

#input <- read.csv("Day 2/example.txt", header = FALSE)[,1]
input <- read.csv("Day 2/input.txt", header = FALSE)[,1]

directions <- input %>% strsplit(" ") %>%
  do.call(rbind.data.frame, .) %>%
  `colnames<-`(c("direction", "change"))

directions$change <- as.numeric(directions$change)

directions$aim <- 0
directions$depth <- 0

i <- 1

if (directions$direction[i] == "down"){
  directions$depth[i] <- directions$change[i]
  directions$aim[i] <- directions$change[i]
} else if (directions$direction[i] == "up"){
  directions$depth[i] <- -directions$depth[i]
  directions$aim[i] <- -directions$change[i]
} else if (directions$direction[i] == "forward"){
  directions$depth[i] <- 0 * directions$aim[i]
}


for (i in 2:nrow(directions)){
  if (directions$direction[i] == "down"){
    directions$depth[i] <- directions$depth[i-1]
    directions$aim[i] <- directions$aim[i-1] + directions$change[i]
  } else if (directions$direction[i] == "up"){
    directions$depth[i] <- directions$depth[i-1]
    directions$aim[i] <- directions$aim[i-1] - directions$change[i]
  } else if (directions$direction[i] == "forward"){
    directions$aim[i] <- directions$aim[i-1]
    directions$depth[i] <- directions$depth[i-1] + (directions$change[i] * directions$aim[i])
  }
}

depth <- directions$depth[nrow(directions)]
horizontal <- sum(directions$change[directions$direction == "forward"])

print(paste0("Answer is ", horizontal * depth))
