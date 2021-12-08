library(plyr)
library(dplyr)
library(purrr)

#input <- read.csv('Day 8/example.txt', header = FALSE, sep = "")
#input <- read.csv('Day 8/example2.txt', header = FALSE, sep = "")
#patterns <- input[c(TRUE, FALSE),]
#outputs <- input[c(FALSE, TRUE),] %>% dplyr::select(1:4)
#input <- cbind(patterns, outputs)
input <- read.csv('Day 8/input.txt', header = FALSE, sep = "")

patterns <- input[1:10]
outputs <- input[12:15]

number_lookup <- tibble(number = 0:9,
                   string = c("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"))
number_lookup$string_c <- NA
for (i in 1:nrow(patterns)){
  row <- patterns[i,]
  strings <- row %>%
    gsub("", " ", .) %>%
    gsub("^ ", "", .) %>%
    gsub(" $", "", .) %>%
    strsplit(split = " ")
  string_lengths <- purrr::map(strings, function(x){length(x)}) %>% unlist()
  strings <- plyr::ldply(strings, rbind)
  letter_lookup <- tibble(alpha_c = letters[1:7],
                          counts = c(sum(strings == "a", na.rm = TRUE),
                                     sum(strings == "b", na.rm = TRUE),
                                     sum(strings == "c", na.rm = TRUE),
                                     sum(strings == "d", na.rm = TRUE),
                                     sum(strings == "e", na.rm = TRUE),
                                     sum(strings == "f", na.rm = TRUE),
                                     sum(strings == "g", na.rm = TRUE)))
                                     
  letter_lookup$alpha <- NA
  #identify letters with unique counts
  letter_lookup$alpha[which(letter_lookup$counts == 4)] <- "e"
  letter_lookup$alpha[which(letter_lookup$counts == 6)] <- "b"
  letter_lookup$alpha[which(letter_lookup$counts == 9)] <- "f"
  
  #find a using diff between one and seven
  one_c <- strings[which(string_lengths == 2),]
  seven_c <- strings[which(string_lengths == 3),]
  one_c <- one_c[!is.na(one_c)]
  seven_c <- seven_c[!is.na(seven_c)]
  letter_lookup$alpha[which(letter_lookup$alpha_c == seven_c[!(seven_c %in% one_c)])] <- "a"

  #find c("b", "d") using diff between four and one
  four_c <- strings[which(string_lengths == 4),]
  four_c <- four_c[!is.na(four_c)]
  bd <- four_c[!(four_c %in% one_c)]
  
  #find d using diff between bd and b
  letter_lookup$alpha[which(letter_lookup$alpha_c == bd[!(bd %in% letter_lookup$alpha_c[letter_lookup$alpha == "b"])])] <- "d"
  
  #find nine
  nine_c <- strings[which(string_lengths == 6),]
  #compared to zero and six, nine contains all of four
  nine_c$contains_four <- NA
  for (j in 1:nrow(nine_c)){
    nine_c$contains_four[j] <- all(four_c %in% nine_c[j,])
  }
  nine_c <- nine_c %>% dplyr::filter(contains_four == TRUE) %>% dplyr::select(-contains_four)
  nine_c <- nine_c[!is.na(nine_c)]
  
  #find g by finding diff between nine and a and four
  letter_lookup$alpha[which(letter_lookup$alpha_c == nine_c[!(nine_c %in% c(four_c, letter_lookup$alpha_c[letter_lookup$alpha == "a"]))])] <- "g"
  
  #find c using diff between one and b
  letter_lookup$alpha[which(letter_lookup$alpha_c == one_c[!(one_c %in% letter_lookup$alpha_c[letter_lookup$alpha == "f"])])] <- "c"
  
  letter_lookup$alpha_c <- toupper(letter_lookup$alpha_c)
  #amend numbers_lookup
  number_lookup$string_c <- number_lookup$string %>% 
    gsub("a", letter_lookup$alpha_c[letter_lookup$alpha =="a"], .) %>%
    gsub("b", letter_lookup$alpha_c[letter_lookup$alpha =="b"], .) %>%
    gsub("c", letter_lookup$alpha_c[letter_lookup$alpha =="c"], .) %>%
    gsub("d", letter_lookup$alpha_c[letter_lookup$alpha =="d"], .) %>%
    gsub("e", letter_lookup$alpha_c[letter_lookup$alpha =="e"], .) %>%
    gsub("f", letter_lookup$alpha_c[letter_lookup$alpha =="f"], .) %>%
    gsub("g", letter_lookup$alpha_c[letter_lookup$alpha =="g"], .) %>%
    tolower()
  
  number_lookup$string_c <- map(strsplit(number_lookup$string_c, ""), function(x){
    sort(x) %>% paste0(collapse = "")
  }) %>% unlist()
  
  for (k in 1:4){
    outputs[i, k] <- strsplit(outputs[i, k], "") %>% unlist() %>% sort() %>% paste0(collapse = "")
    outputs[i, k] <- number_lookup$number[which(number_lookup$string_c == outputs[i, k])]
  }
}

outputs <- outputs %>% dplyr::mutate(
  value = paste0(V12, V13, V14, V15) %>% as.numeric()
)

paste0("Answer is ", sum(outputs$value))