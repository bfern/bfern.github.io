library(dplyr)

get_players_df <- function(df) {
  bind_rows(
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
}