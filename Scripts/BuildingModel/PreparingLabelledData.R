# preparing the labelled data approporiate for inclusion in the model

# Dedicated training data -------------------------------------------------
vid_labelled_files <- list.files("Data/LabelledData/from_vid", pattern = ".csv", full.names = TRUE)
vid_labelled_data <- lapply(vid_labelled_files, function(x){
  dat <- fread(x)
  dat$ID <- str_split(basename(x), "_")[[1]][1]
  dat
})
vid_labelled_data <- rbindlist(vid_labelled_data)
vid_labelled_data <- vid_labelled_data %>%
  mutate(activity = ifelse(activity %in% c("Aggression", "Vigilance", "Rolling", "Jump", "Stalking", "Defensive"), "NaN", activity))

# Inspection of the raw traces --------------------------------------------
vis_labelled_files <- list.files("Data/LabelledData/from_vis", pattern = ".csv", full.names = TRUE)
vis_labelled_data <- lapply(vis_labelled_files, function(x){
  dat <- fread(x)
  dat$ID <- str_split(basename(x), "_")[[1]][1]
  dat
})
vis_labelled_data <- rbindlist(vis_labelled_data)
vis_labelled_data <- vis_labelled_data %>%
  rename(behnum = eco_behaviour) %>%
  select(time, x, y, z, behnum, ID)
# stitch the numbers together to add in the activity labels
activity_key <- fread("Data/LabelledData/Activity_Key.csv")
vis_labelled_data <- merge(vis_labelled_data, activity_key, by = "behnum")
# Preparing labelled data -------------------------------------------------

# Add the two training data sources together ------------------------------
labelled_data <- rbind(vid_labelled_data, vis_labelled_data)
labelled_data <- labelled_data %>% select(!behnum)

# Visualise the labelled data ---------------------------------------------
# plot_dat <- labelled_data %>%
#   na.omit() %>%
#   group_by(ID, activity) %>%
#   slice(1:300) %>%
#   mutate(row = row_number())
# ggplot(plot_dat, aes(x = row, group = 1)) +
#   geom_path(aes(y = x, colour = "coral")) +
#   geom_path(aes(y = y, colour = "lightblue")) +
#   geom_path(aes(y = z, colour = "lightgreen")) +
#   facet_grid(
#     rows = vars(ID),
#     cols = vars(activity),
#     scales = "free",
#     drop = FALSE
#   ) +
#   my_theme()

# Downsample the counts from each category --------------------------------
# experiment with different numbers until chooseing a reasonable one
## TODO: Change this so individuals with more data compensate for those with less
max_count <- 100000
per_ID <- round(max_count / length(unique(labelled_data$ID)),0)

rough_cut <- labelled_data %>%
  group_by(ID, activity) %>%
  slice_head(n = per_ID) %>%
  ungroup() 

# counts <- rough_cut %>% 
#     na.omit() %>%
#     group_by(ID, activity) %>%
#     count()
# ggplot(counts, aes(x = activity, y = n, fill = ID)) +
#     geom_bar(position="stack", stat="identity") +
#     my_theme()

# Save this ---------------------------------------------------------------
# last changes
rough_cut <- rough_cut %>%
  mutate(time = as.POSIXct((time - 719529)*86400, origin = "1970-01-01", tz = "UTC"),
         activity = ifelse(activity == "NaN", "Unknown", activity))
fwrite(rough_cut, "Output/ModelBuilding/cleaned_labelled_data.csv")
 
# rough_cut2 <- rough_cut %>% arrange(ID, time)
# ggplot(rough_cut2, aes(x = seq(1:nrow(rough_cut2)), y = x)) + geom_path()
