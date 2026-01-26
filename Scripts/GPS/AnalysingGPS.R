dat <- fread(file.path("Data", "GPSData", "CollatedGPSData.csv"))


gps_smooth <- dat %>%
  group_by(ID, BibStatus) %>%
  arrange(DateTime) %>%
  mutate(t = as.numeric(DateTime - first(DateTime), units = "secs")) %>%
  group_modify(~ {
    df <- .x
    if (nrow(df) < 5) return(df)
    
    lat_gam <- gam(Latitude  ~ s(t, k = 15), data = df)
    lon_gam <- gam(Longitude ~ s(t, k = 15), data = df)
    
    df %>%
      mutate(
        Lat_smooth = predict(lat_gam),
        Lon_smooth = predict(lon_gam)
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
