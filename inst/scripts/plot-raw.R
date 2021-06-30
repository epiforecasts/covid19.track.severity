library(data.table)
library(ggplot2)
library(scales)
library(cowplot)
library(lubridate)

# load data
cases <- as.data.table(readRDS(here::here("data/test_positive_cases.rds")))
hosp <- setDT(readRDS(here("data/hospital_admissions.rds")))

obs <- rbind(
  cases[, type := "Pillar 2 cases"], 
  hosp[, type := "Hospital admissions"])
obs <- obs[date > as.Date("2020-07-01")]

# make weekly
obs <- obs[, date := floor_date(date, unit = "week", week_start = 1)]
obs <- obs[, .(cases = sum(cases, na.rm = TRUE)),
             by = c("date", "region", "type")]

# make plot
plot_nots <- function(obs) {
  obs <- copy(obs)
  setnames(obs, "type", "Data source")
  plot <- ggplot(obs) +
  aes(x = date, y = cases, col = `Data source`) +
  geom_point(size = 1.2) +
  geom_line(size = 1.1, alpha = 0.8) +
  labs(x = "Date", y = "Notifications") +
  theme_cowplot() +
  scale_x_date(date_breaks = "2 week", date_labels = "%b %d") +
  theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~region, ncol = 3, scales = "free_y")

  return(plot)
}

raw_nots <- plot_nots(obs)
ggsave(
  here("data", "hosp-cases.png"),
  raw_nots,
  dpi = 330, height = 24, width = 24
)

scaled_obs <- copy(obs)
scaled_obs <- scaled_obs[, cases := cases / max(cases),
                           by = c("region", "type")]
scaled_nots <- plot_nots(scaled_obs)
ggsave(
  here("data", "scaled-hosp-cases.png"),
  scaled_nots,
  dpi = 330, height = 24, width = 24
)