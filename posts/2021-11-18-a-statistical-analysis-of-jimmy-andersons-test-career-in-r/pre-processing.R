library(readr)
library(dplyr)
library(stringr)
library(lubridate)


batting <- read_csv("jimmy-anderson/data/batting.csv", col_names = FALSE) %>%
  filter(X1 != "DNB") %>%
  mutate(
    innings_no = 1:n(),
    date = dmy(X13),
    score = as.numeric(str_replace(X1, "[^0-9]", "")),
    dismissed = !str_detect(X1, "\\*"),
    date = dmy(X13)
  ) %>%
  select(innings_no, date, score, dismissed)

saveRDS(batting, "jimmy-anderson/data/batting.rds")