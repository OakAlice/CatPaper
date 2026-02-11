# This is the data provided by Galea --------------------------------------
# Assessing which of the datasets are usable
# Explore each of them and then keep the ones I wanted

# Load in the two csv files -----------------------------------------------
files <- c("Data/GPSData/GPS_BibOff.csv", "Data/GPSData/GPS_BibOn.csv")

Galea_sorted <- lapply(files, function(x){
  fread(x) %>%
    mutate(DateTime = as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")) %>%
    select(ID, DateTime, Date, Latitude, Longitude) %>%
    mutate(ID = str_to_title(ID), # one of them was lowercase so didn't match
           BibStatus = str_split(tools::file_path_sans_ext(basename(x)), "_")[[1]][2])
})
Galea_sorted <- rbindlist(Galea_sorted)
Galea_sorted$ID <- ifelse(Galea_sorted$ID == "Freddy", "Freddie", Galea_sorted$ID)
Galea_sorted$ID <- ifelse(Galea_sorted$ID == "Puddie", "Puddy", Galea_sorted$ID)

ggplot(Galea_sorted, aes(x = Longitude, Latitude, colour = as.factor(BibStatus))) + geom_path() +
  facet_wrap(~ID, nrow = 2, scales = "free") +
  my_theme() + theme(axis.text = element_blank(), axis.ticks = element_blank())

# verdict: very usable, but not a lot of data (1 day per condition per individual)

# Extracting information from the GPX files -------------------------------
# converted these to plain text at: https://www.gpsvisualizer.com/

textfiles <- list.files("Data/GPSData/ConvertedGPX", full.names = TRUE)
textGPS <- lapply(textfiles, function(x){
  fread(x, fill = TRUE) %>%
    select(time, latitude, longitude) %>%
    mutate(across(-time, as.numeric)) %>%
    na.omit() %>%
    mutate(ID = str_split(basename(x), " ")[[1]][1],
           BibStatus = str_split(basename(x), " ")[[1]][2],
           DateTime = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S")) %>%
    rename(Longitude = longitude,
           Latitude = latitude)
})
textGPS <- rbindlist(textGPS) %>% select(-time)

# select the ones not already included in the sorted data
textGPS <- textGPS %>% 
  dplyr::filter(!ID %in% unique(Galea_sorted$ID))

ggplot(textGPS, aes(x = Longitude, Latitude, colour = as.factor(BibStatus))) + geom_path() +
  facet_wrap(~ID, nrow = 2, scales = "free") +
  my_theme() + theme(axis.text = element_blank(), axis.ticks = element_blank())

# verdict: not a lot of data, and most of it doesnt have both conditions for every cat... 

# Converting from the original csvs I was given (no IDs) ------------------
# load in the files
csv_files <- list.files("Data/GPSData/UnknownIDs", full.names = TRUE, pattern = ").csv")
csv_data <- lapply(csv_files, function(x){
  dat <- fread(x)
  dat$filedate <- as.Date(
    str_split(tools::file_path_sans_ext(basename(x)), "-")[[1]][1],
    format = "%Y%m%d"
  )
  dat$filename <- tools::file_path_sans_ext(basename(x))
  dat
})
csv_data <- rbindlist(csv_data)

# define the limits of each individual and the files associated with them
# as I figure them out, input here
IDs <- list(
  A = list(
    Longitude = c(153.0, 152.95),
    Latitude  = c(-26.5, -26.6),
    filenames = c(
      "20160924-171757(1)", "20160924-171757(2)", "20160924-171757(4)",
      "20160927-102645(1)", "20160927-102646(1)"
    )
  ),
  
  B = list(
    Longitude = c(152.9, 152.83),
    Latitude  = c(-26.7, -26.75),
    filenames = c(
      "20160918-003551(1)", "20160918-003551(14)", "20160918-003551(15)",
      "20160918-003551(5)", "20160925-134714(1)", "20160925-134715(1)",
      "20160927-143643(1)", "20160927-143643(2)", "20160928-145813(1)",
      "20160928-145813(2)", "20160929-151540(1)", "20160929-151540(2)",
      "20170208-164443(2)", "20170208-165850(1)"
    )
  ),
  
  C = list(
    Longitude = c(152.85, 152.8),
    Latitude  = c(-26.7, -26.75),
    filenames = c(
      "20161108-162225(23)", "20161108-163143(1)", "20161108-163143(2)",
      "20161109-080923(1)", "20161109-080924(1)", "20161109-080924(2)",
      "20161109-080924(3)", "20161109-080924(4)", "20161109-080925(1)"
    )
  ),
  
  D = list(
    Longitude = c(152.846, 152.83),
    Latitude  = c(-26.76, -26.8),
    filenames = c(
      "20161208-005653(1)", "20161216-143720(6)", "20170208-195354(11)",
      "20170208-195354(15)", "20170208-195354(6)", "20170208-195354(9)"
    )
  ),
  
  E = list(
    Longitude = c(152.85, 152.846),
    Latitude  = c(-26.765, -26.8),
    filenames = c(
      "20160922-063705(1)", "20160922-063706(1)", "20160922-065728(2)",
      "20160922-175346(1)", "20160922-175346(2)", "20160922-180134(1)",
      "20160922-180134(2)"
    )
  ),
  
  G = list(
    Longitude = c(152.847, 152.85),
    Latitude  = c(-26.75, -26.765),
    filenames = c(
      "20161209-074209(2)","20161209-074209(3)"
    )
  ),
  
  H = list(
    Longitude = c(152.848, 152.875),
    Latitude  = c(-26.75, -26.759),
    filenames = c(
      "20161212-154415(1)", "20161212-154415(2)", "20161216-143720(1)", 
      "20161216-143720(2)", "20161216-143720(3)", "20161229-194555(1)"
    )
  ),
  
  I = list(
    Longitude = c(152.848, 152.875),
    Latitude  = c(-26.75, -26.759),
    filenames = c(
      "20160919-003458(1)", "20160919-003458(2)", "20160919-003459(1)", "20160919-003459(2)",
      "20160920-122128(1)", "20160920-123028(1)", "20160920-123028(2)", "20160920-123028(3)"
    )
  )
)

id_lookup <- map_dfr(
  names(IDs),
  ~ tibble(
    filename = IDs[[.x]]$filenames,
    ID = .x
  )
)

csv_data <- csv_data %>%
  left_join(id_lookup, by = "filename") %>%
  mutate(DateTime = as.POSIXct(paste(Date, Time), format = "%Y/%m/%d %H:%M:%S")) %>%
  select(ID, DateTime, Latitude, Longitude)

# further split individual E into 2 sections
csv_data <- csv_data %>%
  mutate(ID = case_when(
    ID == "E" & as.Date(DateTime) == as.Date("2016-09-21") ~ "E1",
    ID == "E" & as.Date(DateTime) == as.Date("2016-09-22") ~ "E2",
    TRUE ~ ID
  ))

ggplot(csv_data, aes(x = Longitude, Latitude)) + geom_path() +
  facet_wrap(~ID, nrow = 2, scales = "free") +
  my_theme() + theme(axis.text = element_blank(), axis.ticks = element_blank())




# Combine the 3 data sources ----------------------------------------------
# add them together and then remove redundant individuals 
all_GPS <- rbind(Galea_sorted, textGPS, csv_data, fill = TRUE)
all_GPS <- all_GPS %>%
  mutate(BibStatus = ifelse(BibStatus == "plus", "BibOn", 
                            ifelse(BibStatus == "no", "BibOff", BibStatus)),
         Date = as.Date(DateTime))

ggplot(all_GPS, aes(x = Longitude, y = Latitude, colour = as.factor(Date))) + geom_path() +
  facet_wrap(~ID, scales = "free") +
  my_theme() + theme(axis.text = element_blank(), axis.ticks = element_blank())

# I found a bunch of matches and therefore removed some data
# the unnamed data had more days but the named data has the associated metadata 
# therefore, I was able to retain most data this way
unique_GPS <- all_GPS %>%
  # while they aren't exactly overlapped, going to remove the ones that are clearly replicates
  dplyr::filter(!ID %in% c("A", "C", "I", "E1", "E2", "G", "B")) 

  # alternatively - to keep them, would recode them...
  # mutate(ID = recode(ID,
  #                     "A" = "Annie",
  #                     "C" = "Calico",
  #                     "I" = "Tassie",
  #                     "E1" = "Coco",
  #                     "E2" = "Timmy",
  #                     "G" = "Webster",
  #                     "B" = "Freddie"
  #                    ))

# # remove duplicate time values if there
# unique_GPS <- unique_GPS %>%
#   arrange(is.na(BibStatus)) %>%
#   distinct(across(-c(BibStatus)), .keep_all = TRUE)

# visualise everything
ggplot(unique_GPS, aes(x = Longitude, y = Latitude, colour = as.factor(BibStatus))) + geom_path() +
  facet_wrap(~ID, scales = "free") +
  my_theme() + theme(axis.text = element_blank(), axis.ticks = element_blank())

# verdict: can't really use this



# Write to file -----------------------------------------------------------
all_GPS <- rbind(Galea_sorted, textGPS, fill = TRUE)
all_GPS <- all_GPS %>%
  mutate(BibStatus = ifelse(BibStatus == "plus", "BibOn", 
                            ifelse(BibStatus == "no", "BibOff", BibStatus)),
         Date = as.Date(DateTime))
ggplot(all_GPS, aes(x = Longitude, y = Latitude, colour = as.factor(BibStatus))) + geom_path() +
  facet_wrap(~ID, scales = "free") +
  my_theme() + theme(axis.text = element_blank(), axis.ticks = element_blank())
fwrite(all_GPS, file.path("Output", "Results", "CollatedGPSData.csv"))
