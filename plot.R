library(dplyr)
library(ggplot2)
library(lubridate)
theme_set(theme_bw() + theme(text = element_text(size=20)))

tweets <- readRDS(file.path("data", "tweets.rds"))
actions <- readRDS(file.path("data", "actions.rds"))
adventures <- readRDS(file.path("data", "adventures.rds"))

adventures %>%
  ggplot(aes(out, duration)) +
  geom_point(size=1) +
  geom_smooth(method="gam", formula= y ~ s(x, bs = "cs"), method.args= list(family=Gamma())) +
  labs(
    x = "Date",
    y = "Duration of Adventure",
    title = "Durations of @PepitoTheCat's Adventures",
    caption = "\ngam(duration ~ s(datetime, bs=\"cs\"), family=Gamma())"
  )

ggsave(file.path("plots", "cs_duration.png"), width=8, height=6, device="png", type="cairo", dpi=600)

n_days <- interval(date(min(adventures$out)), date(max(adventures$home))) / days(1)
range_days <- seq(date(min(adventures$out)), date(min(adventures$out)) + n_days, by="days")

adventures %>%
  mutate(day = date(out)) %>%
  group_by(day) %>%
  count() %>%
  right_join(tibble(day = range_days), by="day") %>%
  ggplot(aes(day, n)) +
  geom_col() +
  geom_smooth(alpha=0.75, method="gam", formula= y ~ s(x, bs = "cs"), method.args= list(family=poisson())) +
  labs(
    x = "Date",
    y = "Adventures per Day",
    title = "@PepitoTheCat's Number of Daily Adventures",
    caption = "\ngam(n_adventures ~ s(date, bs=\"cs\"), family=poisson())"
  )

ggsave(file.path("plots", "cs_adv_per_day.png"), width=8, height=6, device="png", type="cairo", dpi=600)
