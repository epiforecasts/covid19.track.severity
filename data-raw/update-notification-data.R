## code to prepare `DATASET` dataset goes here
admissions <- readRDS("data-raw/hospital_admissions.rds")
beds <- readRDS("data-raw/hospital_beds.rds")
ico <- readRDS("data-raw/icu_beds.rds")
deaths <- readRDS("data-raw/linelist_deaths.rds")


usethis::use_data(admissions, overwrite = TRUE)
usethis::use_data(cases, overwrite = TRUE)
usethis::use_data(occupancy, overwrite = TRUE)
usethis::use_data(icu, overwrite = TRUE)