
# Chunking out unlabelled data to label -----------------------------------
# because we didnt have enough labelled data, I'm going to create some more from the unlabelled data
# chunk it out and then label it in MatLab and then add it to the LabelledData folder for model building

setwd("C:/Users/PC/OneDrive - University of the Sunshine Coast/CatPaper")

cat_list <- list.files("Data/RawData", pattern = ".csv", full.names = TRUE)

# define sections to extract
# (relatively random trial and error but writing it this way so its reproducible)
clips <- list(Coco_1 = list(rest = c(400000, 500000),
                            wake = c(0, 100000)),
              Freddie_1 = list(rest = c(25000,70000),
                               wake = c(75000,100000)),
              Jaya_1 = list(rest = c(200000,300000),
                            wake = c(0,25000)),
              Jett_1 = list(rest = c(500000, 800000),
                            wake = c(100000, 400000)),
              Juliet_1 = list(rest = c(500000, 800000),
                              wake = c(2100000, 2300000)),
              Maple_1 = list(rest = c(200000, 300000),
                              wake = c(0, 1)),
              Maple_2 = list(rest = c(0,100000),
                             wake = c(0, 1)),
              Max_1 = list(rest = c(350000, 450000),
                             wake = c(75000, 175000)),
              Obi_1 = list(rest = c(100000, 200000),
                             wake = c(750000, 1000000)),
              Skippy_1 = list(rest = c(150000, 250000),
                              wake = c(0,100000)),
              Timmy_1 = list(rest = c(150000, 200000),
                             wake = c(1750000, 1850000))
              
              )

lapply(cat_list, function(x){
  cat <- tools::file_path_sans_ext(basename(x))
  for (i in 1:2){
    first_row <- clips[[cat]][[i]][1]
    second_row <- clips[[cat]][[i]][2]
    
    dat <- fread(
      x,
      skip  = first_row - 1,
      nrows = second_row - first_row + 1
    )
    
    ggplot(dat, aes(x = seq(1:nrow(dat)))) + 
      geom_path(aes(y = V2, colour = "coral")) +
      geom_path(aes(y = V3, colour = "lightblue")) +
      geom_path(aes(y = V4, colour = "lightgreen"))

    fwrite(dat, file.path("Data/LabelledData/clipped_data", paste0(cat, "_clipped_", i, ".csv")))
  }
})

