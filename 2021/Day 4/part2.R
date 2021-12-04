library(dplyr)
library(purrr)

#input <- read.csv("Day 4/example.txt", header = FALSE)
input <- read.csv("Day 4/input.txt", header = FALSE)

callout <- c(input[1,]) %>% unlist()

cards_chr <- input %>% 
  dplyr::slice(-1)
cards_chr <- cards_chr[,1]

cards_tbl <- tibble(col_1 = cards_chr %>% substr(1,2) %>% gsub(" ", "", .),
                    col_2 = cards_chr %>% substr(4,5) %>% gsub(" ", "", .),
                    col_3 = cards_chr %>% substr(7,8) %>% gsub(" ", "", .),
                    col_4 = cards_chr %>% substr(10,11) %>% gsub(" ", "", .),
                    col_5 = cards_chr %>% substr(13, 14) %>% gsub(" ", "", .))

cards_tbl$card_no <- 1:nrow(cards_tbl)
cards_tbl$card_no <- ceiling(cards_tbl$card_no/5)

original_cards_tbl <- cards_tbl

i <- 1
while((nrow(cards_tbl) > 5) & (i < length(callout))){
  current_number <- callout[i]
  number_removing <- cards_tbl[, 1:5]
  number_removing[number_removing == current_number] <- "X"
  number_removing$card_no <- cards_tbl$card_no
  cards_tbl <- number_removing
  i <- i+1
  
  bingo_check <- cards_tbl %>% dplyr::mutate(
    row_bingo = (col_1 == "X" & col_2 == "X" & col_3 == "X" & col_4 == "X" & col_5 == "X")
  )
  if(sum(bingo_check$row_bingo)>1){
    winning_bingo_card_id <- cards_tbl$card_no[which(bingo_check$row_bingo == TRUE)]
    cards_tbl <- cards_tbl %>% dplyr::filter(!(card_no %in% winning_bingo_card_id))
  }
  
  bingo_check <- cards_tbl[, 1:5] == "X"
  bingo_check <- data.frame(bingo_check)
  bingo_check$card_no <- cards_tbl$card_no
  bingo_check <- bingo_check %>% dplyr::group_by(card_no) %>%
    dplyr::summarise(col_1 = sum(col_1),
                     col_2 = sum(col_2),
                     col_3 = sum(col_3),
                     col_4 = sum(col_4),
                     col_5 = sum(col_5))
  bingo_check <- bingo_check %>% dplyr::mutate(
    col_bingo = (col_1 == 5 | col_2 == 5 | col_3 == 5 | col_4 == 5 | col_5 == 5)
  ) 
  if(sum(bingo_check$col_bingo)>1){
    winning_bingo_card_id <- bingo_check$card_no[which(bingo_check$col_bingo == TRUE)]
    cards_tbl <- cards_tbl %>% dplyr::filter(!(card_no %in% winning_bingo_card_id))
  }
}

cards_tbl <- cards_tbl %>% dplyr::select(-6)
bingo <- FALSE
while(bingo == FALSE){
  current_number <- callout[i]
  cards_tbl[cards_tbl == current_number] <- "X"
  i <- i+1
  
  bingo_check <- cards_tbl %>% dplyr::mutate(
    row_bingo = (col_1 == "X" & col_2 == "X" & col_3 == "X" & col_4 == "X" & col_5 == "X")
  )
  if(sum(bingo_check$row_bingo)==1){
    winning_bingo_card_id <- ceiling(which(bingo_check$row_bingo == TRUE)/5)
    cards_tbl$card_no <- 1:nrow(cards_tbl)
    cards_tbl$card_no <- ceiling(cards_tbl$card_no/5)
    winning_bingo_card <- cards_tbl %>% dplyr::filter(card_no == winning_bingo_card_id) %>%
      dplyr::select(-6)
    winning_bingo_card[winning_bingo_card == "X"] <- NA
    winning_bingo_card <- map_dfc(winning_bingo_card, function(x){as.numeric(x)})
    print(paste0("Answer is ", sum(winning_bingo_card, na.rm = TRUE) * as.numeric(current_number)))
    bingo <- TRUE
    cards_tbl <- cards_tbl %>% dplyr::select(-6)
  } else {
    bingo_check <- cards_tbl == "X"
    bingo_check <- data.frame(bingo_check)
    bingo_check$card_no <- 1:nrow(bingo_check)
    bingo_check$card_no <- ceiling(bingo_check$card_no/5)
    bingo_check <- bingo_check %>% dplyr::group_by(card_no) %>%
      dplyr::summarise(col_1 = sum(col_1),
                       col_2 = sum(col_2),
                       col_3 = sum(col_3),
                       col_4 = sum(col_4),
                       col_5 = sum(col_5))
    bingo_check <- bingo_check %>% dplyr::mutate(
      col_bingo = (col_1 == 5 | col_2 == 5 | col_3 == 5 | col_4 == 5 | col_5 == 5)
    ) 
    if(sum(bingo_check$col_bingo)==1){
      winning_bingo_card_id <- which(bingo_check$col_bingo == TRUE)
      cards_tbl$card_no <- 1:nrow(cards_tbl)
      cards_tbl$card_no <- ceiling(cards_tbl$card_no/5)
      winning_bingo_card <- cards_tbl %>% dplyr::filter(card_no == winning_bingo_card_id) %>%
        dplyr::select(-6)
      winning_bingo_card[winning_bingo_card == "X"] <- NA
      winning_bingo_card <- map_dfc(winning_bingo_card, function(x){as.numeric(x)})
      print(paste0("Answer is ", sum(winning_bingo_card, na.rm = TRUE) * as.numeric(current_number)))
      bingo <- TRUE
      cards_tbl <- cards_tbl %>% dplyr::select(-6)
    }
  }
}
