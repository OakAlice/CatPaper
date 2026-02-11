dat <- fread(file.path("Output", "Results", "CollatedGPSData.csv"))

# remove the ones that dont have matched conditions
dat <- dat %>%
  filter(!ID %in% c("Webster", "Sooky", "Evee", "Kashmir"))

# Smooth the GPS locations ------------------------------------------------
gps_smooth <- dat %>%
  group_by(ID, BibStatus) %>%
  arrange(DateTime) %>%
  mutate(t = as.numeric(DateTime - first(DateTime), units = "secs")) %>%
  group_modify(~ {
    df <- .x
    if (nrow(df) < 5) return(df)
    
    lat_lm <- lm(Latitude  ~ bs(t, df = min(30, nrow(df)-1), degree = 3), data = df)
    lon_lm <- lm(Longitude  ~ bs(t, df = min(30, nrow(df)-1), degree = 3), data = df)
    
    df %>%
      mutate(
        Lat_smooth = predict(lat_lm),
        Lon_smooth = predict(lon_lm)
      )
  }) %>%
  ungroup()

ggplot(gps_smooth) +
  geom_path(aes(Longitude, Latitude), alpha = 0.3) +
  geom_path(aes(Lon_smooth, Lat_smooth, colour = BibStatus), linewidth = 1) +
  facet_wrap(~ ID, scales = "free") +
  my_theme() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

fwrite(gps_smooth, file.path("Output", "Results", "SmoothedGPSData.csv"))
