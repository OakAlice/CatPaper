# Creating the bib data ---------------------------------------------------

# Assigning the bib status ------------------------------------------------
# find the datetimes when the bib was on and off 
cat_info <- fread("Data/CatInfo.csv")
cat_info <- cat_info %>%
  mutate(ChangeDateTime = paste(ChangeDate, ChangeTime)) %>%
  mutate(ChangeDateTime = as.POSIXct(ChangeDateTime, format = "%d-%b-%y %H:%M:%S", tz = "UTC"))

# list all the outputs
pred_files <- list.files("Output/Predictions",pattern = "_unlabelled_predictions.csv", full.names = TRUE)
pred_data <- lapply(pred_files, function(x){ fread(x)})
pred_data <- rbindlist(pred_data)
pred_data$time <- as.POSIXct((pred_data$time - 719529)*86400, origin = "1970-01-01", tz = "UTC")

# combine these predictions to make a final "predictions" column
# select the highest proabbility target class, but only if the prediction probability is greater than threshold
# smooth so that locmotion is only recorded if it lasts more than one second
# this is very basic code at the moment
pred_data <- pred_data %>%
  mutate(
    prediction_raw = if_else(
      Locomotion == "Locomotion" & Locomotion_prob > 0.85,
      "Locomotion",
      activity_level
    )
  ) %>%
  group_by(run = rleid(prediction_raw)) %>%
  mutate(
    prediction = if_else(
      prediction_raw == "Locomotion" & n() < 3,
      activity_level,
      prediction_raw
    )
  ) %>%
  ungroup() %>%
  select(-run)


# label the preds based on bib on or off
setDT(pred_data)
setDT(cat_info)

# join the change info to pred_data
pred_data <- pred_data %>%
  left_join(cat_info[, .(ID, ChangeDateTime, StartCond)], by = "ID") %>%
  mutate(bib_status = ifelse(time < ChangeDateTime, StartCond,
                             ifelse(StartCond == "Off", "On", "Off"))) %>%
  select(-ChangeDateTime, -StartCond)

fwrite(pred_data, "Output/Results/Final_predictions.csv")
