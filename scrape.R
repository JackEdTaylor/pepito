library(rtweet)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

tweets <- get_timeline("PepitoTheCat", n=Inf) %>%
  mutate(created_at = as_datetime(created_at) + hours(1)) %>%  # correct for timezone
  arrange(created_at)

actions <- tweets %>%
  filter(!is_retweet) %>%
  select(created_at, text, favorite_count, retweet_count, quote_count, reply_count) %>%
  mutate(action = case_when(
    grepl("^Pépito is out \\(\\d{2}:\\d{2}:\\d{2}\\) https://t.co/.+", text) ~ "out",
    grepl("^Pépito is back home \\(\\d{2}:\\d{2}:\\d{2}\\) https://t.co/.+", text) ~ "home"
  )) %>%
  filter(!is.na(action)) %>%
  filter(ifelse(action=="out", lag(action=="home"), lag(action=="out"))) %>%
  filter(
    !(row_number()==1 & action=="home"),
    !(row_number()==n() & action=="out")
  ) %>%
  mutate(adventure_nr = cumsum(action=="out"))

adventures <- actions %>%
  pivot_wider(id_cols=adventure_nr, names_from=action, values_from=created_at) %>%
  mutate(duration = interval(out, home) / minutes(1)) %>%
  filter(duration <= 24*60)  # if gone for more than a day likely due to technical issues

saveRDS(tweets, file.path("data", "tweets.rds"))
saveRDS(actions, file.path("data", "actions.rds"))
saveRDS(adventures, file.path("data", "adventures.rds"))
write_csv(adventures, file.path("data", "adventures.csv"))
