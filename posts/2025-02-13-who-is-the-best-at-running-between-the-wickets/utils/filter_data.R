library(dplyr)

filter_data <- function(df) {
  df %>%
    filter(
      score_minus_wides_and_noballs <= 3, # exclude boundaries
      is.na(dismissal) | (!is.na(dismissal) & dismissal == "run out"), # non wicket deliveries (but can include run outs)
      over >= 7, # exclude powerplay
      bowl_kind != "mixture/unknown" #  exclude if we don't know bowler type
    )
}