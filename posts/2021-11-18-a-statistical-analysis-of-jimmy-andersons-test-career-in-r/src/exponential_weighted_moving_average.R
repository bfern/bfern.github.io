get_neg_log_lik <- function(param, score_vec, not_out_vec) {
  if (length(score_vec) != length(not_out_vec) ||
      !length(param) %in% c(1, length(score_vec))) {
    return(NULL)
  }
  -sum(if_else(
    not_out_vec,
    pgeom(score_vec-1, param, lower.tail = FALSE, log.p = TRUE),
    dgeom(score_vec, param, log = TRUE)
  ))
}


get_weighted_means <- function(weight, warm_up_cut_off, timestep_vec, score_vec, not_out_vec) {
  if (length(timestep_vec) != length(score_vec) ||
      length(score_vec) != length(not_out_vec)) {
    return(NULL)
  }
  weighted_means <- numeric(max(timestep_vec))
  for (i in (warm_up_cut_off + 1):max(timestep_vec)) {
    indexes_to_include <- timestep_vec < i
    weights <- exp(weight * (timestep_vec[indexes_to_include] - i))
    scores <- score_vec[indexes_to_include]
    outs <- !not_out_vec[indexes_to_include]
    weighted_means[i] <- sum(scores * weights) / sum(outs * weights)
  }
  weighted_means
}


get_params_by_timestep <- function(weight, warm_up_cut_off, timestep_vec, score_vec, not_out_vec) {
  weighted_means <- get_weighted_means(weight, warm_up_cut_off, timestep_vec, score_vec, not_out_vec)
  1 / (1 + weighted_means)
}


func_to_optimize <- function(weight, warm_up_cut_off, timestep_vec, score_vec, not_out_vec) {
  if (length(timestep_vec) != length(score_vec) ||
      length(score_vec) != length(not_out_vec)) {
    return(NULL)
  }
  params_by_timestep <- get_params_by_timestep(weight, warm_up_cut_off, timestep_vec, score_vec, not_out_vec)
  timestep_vec_non_warmup <- timestep_vec[timestep_vec > warm_up_cut_off]
  params_vec_non_warmup <- params_by_timestep[timestep_vec_non_warmup]
  score_vec_non_warmup <- score_vec[timestep_vec > warm_up_cut_off]
  not_out_vec_non_warmup <- not_out_vec[timestep_vec > warm_up_cut_off]
  get_neg_log_lik(params_vec_non_warmup, score_vec_non_warmup, not_out_vec_non_warmup)
}


get_optimal_weight <- function(warm_up_cut_off, timestep_vec, score_vec, not_out_vec) {
  if (length(timestep_vec) != length(score_vec) ||
      length(score_vec) != length(not_out_vec)) {
    return(NULL)
  }
  optimize(
    func_to_optimize,
    c(0, 0.5),
    warm_up_cut_off = warm_up_cut_off,
    timestep_vec = timestep_vec,
    score_vec = score_vec,
    not_out_vec = not_out_vec
  )$minimum}




