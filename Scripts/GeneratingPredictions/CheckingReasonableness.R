# Checking the reasonableness of the predictions --------------------------
# its all very well and good to go ahead and do all that work, but we need a way to check
# whether it's evern reasonable
# without labelled data, that is hard, but we can try our best through visualisation
# this script is an interactive scroll through for looking and checking how the preds align with the accel

# TODO: Automate this 


cat <- "Freddie"

# load in the raw accel
rawdat <- fread(paste0("Data/RawData/", cat, "_1.csv")) # because thats the size of a chunk
colnames(rawdat) <- c("time", "x", "y", "z")

# load in the predictions
preds <- fread(file.path("Output/Predictions", paste0(cat, "_unlabelled_predictions.csv")))

# combine
setDT(rawdat)
setDT(preds)
setkey(rawdat, time)
setkey(preds, time)
combdat <- preds[rawdat, roll = "nearest"]

# plot them
combdat_plot <- combdat[0:80000,]
ggplot(combdat_plot, aes(x = seq(1:nrow(combdat_plot)), colour = prediction, group = 1)) + 
  geom_path(aes(y = x)) +
  geom_path(aes(y = y)) +
  geom_path(aes(y = z))
