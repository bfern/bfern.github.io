library(dplyr)
source("utils/read_in_blast_data.R")
source("utils/get_players_df.R")
source("utils/create_new_cols.R")
source("utils/remove_dodgy_data.R")
source("utils/filter_data.R")

players_file <- "data/players_df.rds"
t20_bbb_for_model_file <- "data/t20_bbb_for_model.rds"

if (!file.exists(t20_bbb_for_model_file) || !file.exists(players_file)) {

  t20_bbb <- read_in_blast_data()

  players_df <- get_players_df(t20_bbb)
  saveRDS(players_df, players_file)
    
  t20_bbb_for_model <- t20_bbb %>%
    create_new_cols() %>%
    remove_dodgy_data() %>%
    filter_data() %>%
    arrange(p_match, inns, over, ball) %>%
    select(
      p_match, inns, over, ball, p_bat, p_nonstriker_bat,
      ground, bat_hand, bowl_kind, balls_rem, wagonX, wagonY,
      score_minus_wides_and_noballs
    )
  saveRDS(t20_bbb_for_model, t20_bbb_for_model_file)

} else {

  players_df <- readRDS(players_file)
  t20_bbb_for_model <- readRDS(t20_bbb_for_model_file)

}
