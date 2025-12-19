# Not sure what I'm going to do here yet but playing ----------------------


# Assigning the bib status ------------------------------------------------
# list all the outputs
pred_files <- list.files("Output/Predictions",pattern = "_unlabelled_predictions.csv", full.names = TRUE)
pred_data <- lapply(pred_files, function(x){ fread(x)})
pred_data <- rbindlist(pred_data)
pred_data$time <- as.POSIXct((pred_data$time - 719529)*86400, origin = "1970-01-01", tz = "UTC")
pred_data$date <- as.Date(pred_data$time)

# find the datetimes when the bib was on and off 
cat_info <- fread("Data/CatInfo.csv")
cat_info <- cat_info %>%
  dplyr::mutate(dplyr::across(
    c(BibOnStartDate, BibOnEndDate, BibOffStartDate, BibOffEndDate),
    \(x) as.POSIXct(x, format = "%d-%b-%y", tz = "UTC")
  )) %>%
  select(ID, Sex, BibOnStartDate, BibOnEndDate, BibOffStartDate, BibOffEndDate)

# label the preds based on bib on or off
setDT(cat_info)
setDT(pred_data)

# Join individual-level bib dates onto predictions
pred_data <- cat_info[pred_data, on = "ID"]

# Assign bib status by the date (until I can figure out the times lmao)
pred_data[, bib_status := {
  on  <- !is.na(BibOnStartDate)  & !is.na(BibOnEndDate)  &
    date >= as.Date(BibOnStartDate) &
    date <= as.Date(BibOnEndDate)
  
  off <- !is.na(BibOffStartDate) & !is.na(BibOffEndDate) &
    date >= as.Date(BibOffStartDate) &
    date <= as.Date(BibOffEndDate)
  
  fifelse(on & off, "transition",
          fifelse(on, "On",
                  fifelse(off, "Off", NA_character_)))
}]

final_data <- pred_data %>% select(-BibOnStartDate, -BibOnEndDate, -BibOffStartDate, -BibOffEndDate,
                                  -activity_level, -Locomotion, -Fast_Locomotion) %>%
  mutate(hour = hour(time))

# Making some graphs ------------------------------------------------------
ggplot(final_data, aes(x = as.factor(hour), y = mean_VDBA)) + 
  geom_boxplot() + 
  xlab("Hour") + ylab("Mean VDBA (g)") + 
  my_theme()

