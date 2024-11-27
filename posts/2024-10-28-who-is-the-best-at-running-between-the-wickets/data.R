library(readr)
library(dplyr)
library(stringr)

t20_bbb_links <- list(
  "2015" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/AIrqfgrtrWnzL2y91kArA6I/t20_2015.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2016" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/ACehDaYpVx9tBvfBWmVK7Ds/t20_2016.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2017" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/AJt4x4hRw6lIYUy0JC1Rw4I/t20_2017.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2018" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/AIiMP2xD16cy_gPNDvFTvFA/t20_2018.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2019" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/ANdcqTtYYWQOJ_pcuIYaTlQ/t20_2019.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2020" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/ANc9rKEFNRpuJ73Xt0z59C4/t20_2020.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2021" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/ADCtr7XVpSsVs9D-UYwHSZo/t20_2021.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2022" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/AHnU-4jiZzSWUCndhXUIjag/t20_2022.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2023" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/AH2g3ggZO77cGLWkd-BID88/t20_2023.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1",
  "2024" = "https://www.dropbox.com/scl/fo/7dv7ry9ljzasj3ovxnv10/AGwhkn3pweMcSkwT-H88pkw/t20_2024.csv?rlkey=5kb3q4pjt7laddl1vctt9dx6m&dl=1"
)

major_english_county_grounds <- c(
  "Edgbaston, Birmingham",
  "Trent Bridge, Nottingham",
  "Riverside Ground, Chester-le-Street",
  "Headingley, Leeds",
  "Kennington Oval, London",
  "County Ground, Hove",
  "County Ground, Northampton",
  "The Rose Bowl, Southampton",
  "County Ground, New Road, Worcester",
  "Grace Road, Leicester",
  "County Ground, Chelmsford",
  "St Lawrence Ground, Canterbury",
  "Sophia Gardens, Cardiff",
  "Old Trafford, Manchester",
  "County Ground, Bristol",
  "County Ground, Derby",
  "The Cooper Associates County Ground, Taunton",
  "Lord's, London"
)

t20_bbb_list <- list()
for (i in 1:10) {
  year <- 2014 + i
  t20_bbb_list[[i]] <- tryCatch(
    {
      df <- read_csv(t20_bbb_links[[as.character(year)]])
      if (ncol(df) == 1 && colnames(df) == "<!DOCTYPE html>") {
        stop(paste("Error: The file for year", year, "appears to be an HTML document instead of a CSV file."))
      }
      df %>%
        mutate(ground = case_when(
          ground == "County Ground, Taunton" ~ "The Cooper Associates County Ground, Taunton",
          TRUE ~ ground
        )) %>%
        filter(
          competition == "T20 Blast", # t20 blast only
          ground %in% major_english_county_grounds # major county grounds only
        )
    },
    error = function(e) {
      message(paste("Error reading data for year", year, "from URL:", url))
    }
  )
}
t20_bbb <- bind_rows(t20_bbb_list) %>%
  mutate(wickets_lost = inns_wkts - as.numeric(out))

players_df <- bind_rows(
  t20_bbb %>%
    select(p_bat, bat) %>%
    rename(
      player_id = p_bat,
      player_name = bat
    ),
  t20_bbb %>%
    select(p_bowl, bowl) %>%
    rename(
      player_id = p_bowl,
      player_name = bowl
    )
) %>%
  filter(!duplicated(player_id)) %>%
  arrange(player_id)

num_bats_seen_per_wicket <- t20_bbb %>%
  group_by(p_match, inns, wickets_lost) %>%
  summarise(num_bats_seen = length(unique(c(p_bat, p_out))), .groups = "drop")

first_bat_seen_per_wicket <- t20_bbb %>%
  group_by(p_match, inns, wickets_lost) %>%
  summarise(first_bat_seen = (unique(c(p_bat, p_out)))[1], .groups = "drop")

second_bat_seen_per_wicket <- t20_bbb %>%
  group_by(p_match, inns, wickets_lost) %>%
  summarise(second_bat_seen = (unique(c(p_bat, p_out)))[2], .groups = "drop")

t20_bbb <- t20_bbb %>%
  mutate(
    score_minus_wides_and_noballs = score - (wide + noball),
    balls_rem = inns_balls_rem + as.numeric(!outcome %in% c("no ball", "wide"))
  ) %>%
  left_join(num_bats_seen_per_wicket, by = c("p_match", "inns", "wickets_lost")) %>%
  left_join(first_bat_seen_per_wicket, by = c("p_match", "inns", "wickets_lost")) %>%
  left_join(second_bat_seen_per_wicket, by = c("p_match", "inns", "wickets_lost")) %>%
  mutate(p_nonstriker_bat = if_else(num_bats_seen == 2, if_else(p_bat == first_bat_seen, second_bat_seen, first_bat_seen), NA)) %>%
  filter(
    wagonX != 0 & wagonY != 0, # need wagonwheel data
    score_minus_wides_and_noballs <= 3, # exclude boundaries
    is.na(dismissal) | (!is.na(dismissal) & dismissal == "run out"), # non wicket deliveries (but can include run outs)
    over >= 7, # exclude powerplay
    bowl_kind != "mixture/unknown" #  exclude if we don't know bowler type
  ) %>%
  select(
    p_match, inns, over, ball, p_bat, p_nonstriker_bat, wagonX, wagonY, bat_hand, bowl_kind, balls_rem, ground, score_minus_wides_and_noballs
  )
