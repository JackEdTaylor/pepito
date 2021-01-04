library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
theme_set(theme_bw() + theme(text = element_text(size=20)))

tweets <- readRDS(file.path("data", "tweets.rds"))
actions <- readRDS(file.path("data", "actions.rds"))
adventures <- readRDS(file.path("data", "adventures.rds"))

adventures %>%
  ggplot(aes(out, duration)) +
  geom_point(size=1) +
  geom_smooth(method="gam", formula= y ~ s(x, bs="cs"), method.args= list(family=Gamma())) +
  scale_y_continuous(breaks = scales::breaks_pretty(10)) +
  labs(
    x = "Date",
    y = "Duration of Adventure (Minutes)",
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
  geom_smooth(alpha=0.8, method="gam", formula= y ~ s(x, bs = "cs"), method.args= list(family=poisson())) +
  labs(
    x = "Date",
    y = "Adventures per Day",
    title = "@PepitoTheCat's Number of Daily Adventures",
    caption = "\ngam(n_adventures ~ s(date, bs=\"cs\"), family=poisson())"
  )

ggsave(file.path("plots", "cs_adv_per_day.png"), width=8, height=6, device="png", type="cairo", dpi=600)

adventures %>%
  mutate(time = as.POSIXct(format(out, "%H:%M:%S"), format="%H:%M:%S")) %>%
  ggplot(aes(time)) +
  geom_histogram(aes(y = stat(count / sum(count))), binwidth = 3600) +
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%I:%M %p") +
  scale_y_continuous(breaks = scales::breaks_pretty(6)) +
  labs(
    x = "Time",
    y = "Proportion",
    title = "When does @PepitoTheCat go Out?"
  ) +
  theme(axis.text.x = element_text(angle=30, hjust=1))

ggsave(file.path("plots", "hist_out_per_time.png"), width=8, height=6, device="png", type="cairo", dpi=600)

adventures %>%
  mutate(time = as.POSIXct(format(home, "%H:%M:%S"), format="%H:%M:%S")) %>%
  ggplot(aes(time)) +
  geom_histogram(aes(y = stat(count / sum(count))), binwidth = 3600) +
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%I:%M %p") +
  scale_y_continuous(breaks = scales::breaks_pretty(6)) +
  labs(
    x = "Time",
    y = "Proportion",
    title = "When does @PepitoTheCat get Home?"
  ) +
  theme(axis.text.x = element_text(angle=30, hjust=1))

ggsave(file.path("plots", "hist_home_per_time.png"), width=8, height=6, device="png", type="cairo", dpi=600)

adventures %>%
  filter(duration >= 350) %>%
  mutate(time = as.POSIXct(format(home, "%H:%M:%S"), format="%H:%M:%S")) %>%
  ggplot(aes(time)) +
  geom_histogram(aes(y = stat(count / sum(count))), binwidth = 3600) +
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%I:%M %p") +
  scale_y_continuous(breaks = scales::breaks_pretty(6)) +
  labs(
    x = "Time",
    y = "Proportion",
    title = "When does he get Home from Adventures?",
    subtitle = "(Gone for >350 minutes)"
  ) +
  theme(axis.text.x = element_text(angle=30, hjust=1))

ggsave(file.path("plots", "hist_home_per_time_adv.png"), width=8, height=6, device="png", type="cairo", dpi=600)

adventures %>%
  mutate(time = as.POSIXct(format(out, "%H:%M:%S"), format="%H:%M:%S")) %>%
  ggplot(aes(time, duration)) +
  geom_point(size=1) +
  geom_smooth(method="gam", formula= y ~ s(x, bs="cs"), method.args= list(family=Gamma())) +
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%I:%M %p") +
  scale_y_continuous(breaks = scales::breaks_pretty(10)) +
  labs(
    x = "Time of Leaving",
    y = "Duration of Outing",
    title = "When does @PepitoTheCat go on Adventures?"
  ) +
  theme(axis.text.x = element_text(angle=30, hjust=1))

ggsave(file.path("plots", "out_by_duration.png"), width=8, height=6, device="png", type="cairo", dpi=600)
