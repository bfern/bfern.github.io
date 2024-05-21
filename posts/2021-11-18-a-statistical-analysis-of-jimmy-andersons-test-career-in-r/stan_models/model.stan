functions {
  real geometric_lpmf(int y, real theta) {
    return log(theta) + y * log(1 - theta);
  }
  real geometric_lccdf(int y, real theta) {
    return (y + 1) * log(1 - theta);
  }
}

data {
  int<lower=1> num_outs;
  int<lower=1> num_not_outs;
  int<lower=1> ntimesteps;
  int<lower=1, upper=ntimesteps> timestep_out[num_outs];
  int<lower=1, upper=ntimesteps> timestep_not_out[num_not_outs];
  int<lower=0> score_out[num_outs];
  int<lower=0> score_not_out[num_not_outs];
}

parameters {
  real initial_param;
  real unscaled_param[ntimesteps-1];
  real<lower=0> sigma;
}

transformed parameters {
  real param[ntimesteps];
  real transformed_param[ntimesteps];
  param[1] = initial_param;
  for (k in 2:ntimesteps) {
    param[k] = param[k-1] + unscaled_param[k-1] * sigma;
  }
  transformed_param = inv_logit(param);
}

model {
  for (i in 1:num_outs) {
    target += geometric_lpmf(score_out[i] | transformed_param[timestep_out[i]]);
  }
  for (i in 1:num_not_outs) {
    target += geometric_lccdf( (score_not_out[i] - 1) | transformed_param[timestep_not_out[i]]);
  }
  initial_param ~ normal(-2.25, 0.5);
  unscaled_param ~ std_normal();
  sigma ~ std_normal();
}
