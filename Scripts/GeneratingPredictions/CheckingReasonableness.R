# Checking the reasonableness of the predictions --------------------------
# its all very well and good to go ahead and do all that work, but we need a way to check
# whether it's evern reasonable
# without labelled data, that is hard, but we can try our best through visualisation
# this script is an interactive scroll through for looking and checking how the preds align with the accel
# I iterated this dozens of times until I was happy with the smoothing logic etc.

cat <- "Jaya" # select a random cat

# load raw accelerometer data
rawdat <- fread(paste0("Data/RawData/", cat, "_1.csv"))
setnames(rawdat, c("time", "x", "y", "z"))
# rawdat$time <- as.POSIXct((rawdat$time - 719529)*86400, origin = "1970-01-01", tz = "UTC")

# load predictions
preds <- fread("Output/Results/Final_predictions.csv") %>%
  filter(ID == cat)

setDT(preds)

# join on nearest timestamp
setkey(rawdat, time)
setkey(preds, time)
combdat <- preds[rawdat, roll = "nearest"]

# colours for categorical prediction
prediction_colours <- c(
  "Locomotion" = "#e89fbf",
  "high"       = "#d8907c",
  "medium"     = "#e6c078",
  "low"        = "lightgreen",
  "inactive"   = "#0097b2"
)

# subset for plotting
combdat_plot <- combdat[56000:59000,]

# time axis in seconds
combdat_plot[, t := (seq_len(.N) - 1) / 50] # where 50 is the Hz

good_plot <- ggplot(combdat_plot, aes(x = t, group = 1, colour = prediction)) +
  geom_path(aes(y = x), linewidth = 1) +
  geom_path(aes(y = y), linewidth = 1) +
  geom_path(aes(y = z), linewidth = 1) +
  scale_colour_manual(values = prediction_colours) +
  my_theme() +
  labs(x = "Time (s)", y = "Acceleration (g)", colour = "Prediction")

good_plot

# and when you're happy with it, save as an example and lock in the results :)
ggsave(
  filename = "Manuscript/Figures/example_of_prediction.png",
  plot = good_plot,
  width = 200, height = 100, units = "mm", dpi = 300,
  bg = "transparent"
)

