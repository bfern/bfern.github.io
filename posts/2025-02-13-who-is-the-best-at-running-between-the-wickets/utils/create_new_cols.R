library(dplyr)

create_new_cols <- function(df) {
  df <- df %>%
    mutate(wickets_lost = inns_wkts - as.numeric(out))
  num_bats_seen_per_wicket <- df %>%
    group_by(p_match, inns, wickets_lost) %>%
    summarise(num_bats_seen = length(unique(c(p_bat, p_out))), .groups = "drop")
  first_bat_seen_per_wicket <- df %>%
    group_by(p_match, inns, wickets_lost) %>%
    summarise(first_bat_seen = (unique(c(p_bat, p_out)))[1], .groups = "drop")
  second_bat_seen_per_wicket <- df %>%
    group_by(p_match, inns, wickets_lost) %>%
    summarise(second_bat_seen = (unique(c(p_bat, p_out)))[2], .groups = "drop")
  df %>%
    mutate(
      score_minus_wides_and_noballs = score - (wide + noball),
      balls_rem = inns_balls_rem + as.numeric(!outcome %in% c("no ball", "wide"))
    ) %>%
    left_join(num_bats_seen_per_wicket, by = c("p_match", "inns", "wickets_lost")) %>%
    left_join(first_bat_seen_per_wicket, by = c("p_match", "inns", "wickets_lost")) %>%
    left_join(second_bat_seen_per_wicket, by = c("p_match", "inns", "wickets_lost")) %>%
    mutate(p_nonstriker_bat = if_else(num_bats_seen == 2, if_else(p_bat == first_bat_seen, second_bat_seen, first_bat_seen), NA))
}