# Generating VDBA from the labelled data ----------------------------------

data <- fread("ModelBuilding/cleaned_labelled_data.csv")

# firstly, find all the continuous stretches of data
data <- detect_breaks(data)

IDs <- split(data, data$ID)

vdba_data <- lapply(IDs, function(x){
  print(x)
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
vdba_data <- na.omit(vdba_data)

# save this
fwrite(vdba_data, "ModelBuilding/all_labelled_vdba.csv")
