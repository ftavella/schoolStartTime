## code to prepare `worldCities` dataset goes here
worldCities <- readr::read_csv("https://raw.githubusercontent.com/datasets/world-cities/master/data/world-cities.csv")

usethis::use_data(worldCities, overwrite = TRUE)

