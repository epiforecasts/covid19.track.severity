## code to prepare `DATASET` dataset goes here
cases <- readRDS("data-raw/test_positive_cases.rds")
admissions <- readRDS("data-raw/hospital_admissions.rds")
occupancy <- readRDS("data-raw/hospital_beds.rds")
icu <- readRDS("data-raw/icu_beds.rds")
deaths <- readRDS("data-raw/linelist_deaths.rds")


usethis::use_data(admissions, overwrite = TRUE)
usethis::use_data(cases, overwrite = TRUE)
usethis::use_data(occupancy, overwrite = TRUE)
usethis::use_data(icu, overwrite = TRUE)