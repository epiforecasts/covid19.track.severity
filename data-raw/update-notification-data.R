library(data.table)
library(purrr)

# load data
cases <- setDT(readRDS("data-raw/test_positive_cases.rds"))
admissions <- setDT(readRDS("data-raw/hospital_admissions.rds"))
occupancy <- setDT(readRDS("data-raw/hospital_beds.rds"))
icu <- setDT(readRDS("data-raw/icu_beds.rds"))
deaths <- setDT(readRDS("data-raw/linelist_deaths.rds"))

# clean up naming
setnames(admissions, "cases", "admissions")
setnames(occupancy, "beds", "hospital_occupancy")
setnames(icu, "beds", "icu_occupancy")


uk_notifications <- list(cases, admissions, occupancy, icu, deaths)

uk_notifications <- reduce(uk_notifications, merge, all = TRUE, 
                           by = c("date", "region")
)

usethis::use_data(uk_notifications, overwrite = TRUE)
