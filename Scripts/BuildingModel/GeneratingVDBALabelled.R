# Generating VDBA from the labelled data ----------------------------------

data <- fread("ModelBuilding/cleaned_labelled_data.csv")

# firstly, find all the continuous stretches of data
data <- detect_breaks(data)

IDs <- split(data, data$ID)

vdba_data <- lapply(IDs, function(x){
  # split into the continuous data bits and feed each to the function
  sections <- split(x, x$break_id)
  
  vdba_results <- lapply(
    sections,
    generate_vdba,
    window = 2,
    freq = 50
  )
  vdba_results <- bind_rows(vdba_results)
  
})
vdba_data <- bind_rows(vdba_data)
# vdba_data2 <- na.omit(vdba_data) # don't na omit because it removes too many rows!

# average to one second 
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

seconds <- vdba_data %>%
  group_by(break_id) %>%
  mutate(sample_i = row_number(),
         second = floor((sample_i - 1) / freq)) %>%
  group_by(ID, second) %>%
  summarise(
    mean_VDBA = mean(vedba, na.rm = TRUE),
    sd_vedba = sd(vedba, na.rm = TRUE),
    time = first(time),
    activity = get_mode(activity),
    .groups = "drop"
  ) %>%
  select(-second)

# save this
fwrite(seconds, "ModelBuilding/all_labelled_vdba.csv")
