library(dplyr)
library(class)

remove_dodgy_data <- function(df) {
  df_filtered <- df %>%
    filter(
      wagonZone != 0, # remove when no wagonwheel data available
      (wagonX != 180 | wagonY != 125), # used when ball goes straight to keeper
      (wagonX != 180 | wagonY != 140), # used when ball goes straight to keeper
      (wagonX != 180 | wagonY != 165), # used for batter misses and ball either hits batter or stumps
      (wagonX != 180 | wagonY != 204) # used for ball goes straight to bowler
    )
  build_boundary_model_from_wagonwheel_coords(
    as.factor(df_filtered$score_minus_wides_and_noballs >= 4),
    df_filtered %>% select(wagonX, wagonY)
  )
  boundary_model <- readRDS("models/boundary_model_from_wagonwheel_coords.rds")
  df_filtered <- df_filtered %>%
    remove_likely_boundaries(boundary_model)
  df_filtered
}

build_boundary_model_from_wagonwheel_coords <- function(boundaries, wagonwheel_coords, rebuild = FALSE) {
  boundary_model_file <- "models/boundary_model_from_wagonwheel_coords.rds"
  if (rebuild || !file.exists(boundary_model_file)) {
    set.seed(42)
    boundary_model <- knn(
      train = wagonwheel_coords %>%
        mutate(
          wagonX = wagonX + rnorm(n(), sd = 0.001),
          wagonY = wagonY + rnorm(n(), sd = 0.001)
        ) %>%
        as.matrix(),
      test = as.matrix(wagonwheel_coords),
      cl = boundaries,
      k = 20,
      prob = TRUE
    )
    saveRDS(boundary_model, boundary_model_file)
  }
}

remove_likely_boundaries <- function(df, knn_model, threshold=0.5) {
  boundary_probs <- ifelse(
    knn_model == TRUE,
    attr(knn_model, "prob"),
    1 - attr(knn_model, "prob")
  )
  df %>% slice(which(boundary_probs <= threshold))
}