identify_sequences <- function(data, max_break = 2){
  
  data <- data %>%
    group_by(ID) %>%
    arrange(time, .by_group = TRUE) %>%
    mutate(time_diff = difftime(time, data.table::shift(time)), # had to define package or errored
           break_point = ifelse(time_diff > max_break | time_diff < 0 , 1, 0),
           break_point = replace_na(break_point, 0),
           sequence = cumsum(break_point)) %>%
    select(-break_point, -time_diff) %>%
    ungroup()
  
  return(data)
  
}

identify_events <- function(data, class_col = "true_class"){
  
  setDT(data)
  
  data[
    order(ID, sequence, time),
    event := {
      lag_class <- shift(get(class_col))
      cp <- fifelse(lag_class == get(class_col), 0L, 1L)
      cp[is.na(cp)] <- 0L
      cumsum(cp)
    },
    by = .(ID, sequence)
  ]
  
  return(data)
}
