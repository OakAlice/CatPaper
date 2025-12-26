# Making a figure linking behaviour to acceleration -----------------------

dat <- fread("Output/ModelBuilding/cleaned_labelled_data.csv") %>%
  dplyr::filter(ID == "Juliet")

# extract the sections of data I want to use
inactive <- dat %>% 
  dplyr::filter(activity == "Rest") %>%
  slice(1000:1200)
walking <- dat %>% 
  dplyr::filter(activity == "Locomotion") %>% 
  slice(1000:1200)
running <- dat %>% 
  dplyr::filter(activity %in% c("Locomotion")) %>% 
  slice(1:200)
# check the sections independently
plot_dat <- walking
ggplot(plot_dat, aes(x = seq_len(nrow(plot_dat)), y = x)) + geom_line()

# combine them
subset <- rbind(inactive, walking, running)

# generate the metrics
win <- 1 * 50  # smoothing window over 25hz

# calculate the static accelerations
subset$ax_static <- frollmean(subset$x, n = win, align = "center", fill = NA)
subset$ay_static <- frollmean(subset$y, n = win, align = "center", fill = NA)
subset$az_static <- frollmean(subset$z, n = win, align = "center", fill = NA)

# get the dynamic component 
subset$ax_dynamic <- subset$x - subset$ax_static
subset$ay_dynamic <- subset$y - subset$ay_static
subset$az_dynamic <- subset$z - subset$az_static

subset$vedba <- sqrt(subset$ax_dynamic^2 + subset$ay_dynamic^2 + subset$az_dynamic^2)

# smooth it
subset[, smooth_vdba := frollmean(vedba, n = win, align = "center", fill = NA)]

# plots 
raw <- ggplot(subset, aes(x = seq_len(nrow(subset)))) +
  geom_line(aes(y = x), colour = "#e89fbf", linewidth = 1) +
  geom_line(aes(y = y), colour = "#e6c078", linewidth = 1) +
  geom_line(aes(y = z), colour = "lightgreen", linewidth = 1) +
  my_theme() +
  ylab("Acceleration (g)") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

vdba <- ggplot(subset, aes(x = seq_len(nrow(subset)))) +
  geom_line(aes(y = vedba, colour = "Raw"), linewidth = 1) +
  geom_line(aes(y = smooth_vdba, colour = "Smoothed"), linewidth = 1) +
  scale_colour_manual(values = c("Raw" = "#0097b2",
                                 "Smoothed" = "#505a6d")) +
  my_theme() +
  theme(
    legend.position.inside = c(0.98, 0.98),
    legend.justification = c(1, 1)
  ) +
  labs(x = "Time", y = "VDBA")

ExampleTraceShapes <- raw / vdba

# save to disk
ggsave(
  filename = "Manuscript/Figures/ExampleTraceShapes.png",
  plot = ExampleTraceShapes,
  width = 180, height = 120, units = "mm", dpi = 300,
  bg = "transparent"
)


