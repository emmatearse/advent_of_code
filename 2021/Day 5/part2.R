coords <- cbind(start_points, end_points)
coords$x2 <- gsub(">", "", coords$x2)
coords <- map_dfc(coords, function(x){as.integer(x)})
coords <- coords %>% 
  dplyr::filter(!(x1 == x2) & !(y1 == y2))
coords_covered <- pmap(coords %>% dplyr::select(c('x1', 'x2', 'y1', 'y2')), function(x1, x2, y1, y2){
  current_x <- x1
  current_y <- y1
  covered <- paste0("(", current_x, ",", current_y, ")")
  if (current_x > x2){
    while(!(current_x == x2)){
      current_x <- current_x - 1
      if(current_y > y2){current_y <- current_y - 1} else {current_y <- current_y + 1}
      covered <- c(covered, paste0("(", current_x, ",", current_y, ")"))
    }
  } else if (current_x < x2){
    while(!(current_x == x2)){
      current_x <- current_x + 1
      if(current_y > y2){current_y <- current_y - 1} else {current_y <- current_y + 1}
      covered <- c(covered, paste0("(", current_x, ",", current_y, ")"))
    }
  }
  return(covered)
})
coords_covered <- unlist(coords_covered) %>% 
  as.tibble() %>% 
  `colnames<-`("coords")
coverage_summary_diagonal <- coords_covered %>% 
  dplyr::group_by(coords) %>%
  dplyr::summarise(times_covered = n())

coverage_summary <- rbind(coverage_summary_hv, coverage_summary_diagonal) %>%
  dplyr::group_by(coords) %>%
  dplyr::summarise(times_covered = sum(times_covered))
paste0("Answer is ", sum(coverage_summary$times_covered >= 2))
