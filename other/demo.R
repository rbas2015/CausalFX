
source("synthetize.R")
source("wpp.R")
source("iv.R")

# Create synthetic problem

epsilons <- c(0.2, 0.2, 0.2, 0.2, 0.9, 1.1)
p <- 4
q <- 4
par_max <- 3
sample_size <- 1000
num_monte_carlo <- 1000
hardness_factor <- 0.10
N <- 1000; i <- 1

while (TRUE) {
  problem <- simulateWitnessModel(p, q, par_max, sample_size)
  effects <- synthetizeCausalEffect(problem)
  out <- effects[[1]] - effects[[2]]
  if (abs(out) > hardness_factor) break()
  printPercentage(i, N)
  i <- i + 1
}

# Test covariate search only

sol_pop <- covsearch(problem, pop_solve = TRUE)
effect_pop <- synthetizeCausalEffect(problem)
cat(sprintf("ACE (true) = %1.2f\nACE (adjusting for all) = %1.2f\nACE (adjusting for nothing) = %1.2f\n", 
            effect_pop$effect_real, effect_pop$effect_naive, effect_pop$effect_naive2))

covariate_hat <- covsearch(problem, cred_calc = TRUE, M = 1000)
summary(covariate_hat)

# Test IV analysis using result from covariate search

sol_iv <- iv(problem, covariate_hat$hw,  covariate_hat$hZ, prior_table = 10, M = 1000)
summary(sol_iv)
  
# Test WPP: first, use analytical bounds

wpp_sol <- wpp(problem, epsilons, M = 1000, prior_ind = 0.5, prior_table = 10, cred_calc = TRUE, analytical_bounds = TRUE, verbose = FALSE)
summary(wpp_sol)

# Test WPP: use numerical bounds (very slow)

wpp_sol2 <- wpp(problem, epsilons, M = 100, prior_ind = 0.5, prior_table = 10, cred_calc = TRUE, analytical_bounds = FALSE, verbose = TRUE)
summary(wpp_sol2)
