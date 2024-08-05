load(file.path("./_sim_data", "sim_params.rda"))
source("fcmc_functions_v2.R")
source("fcmc_power_functions.R")

mult = array(0, dim = c(42, 32, 300))
mult[seq(21-4, 21+4), seq(15-4, 15+4), 200:300] = 1
sum(mult)
multmat = matrix(c(mult), nrow = nrow(sim_means), ncol = 300)

# generate 100 null data sets with 10 models each

set.seed(1)

generate_null_data(
  nrep = 100,
  m = sim_means,
  s = sim_sd)

# generate data for power testing with 10% increase
# in mean based on sd

set.seed(5)

generate_power_data(
  nrep = 100,
  m = sim_means,
  s = sim_sd,
  pc = 10)

set.seed(6)

generate_power_data(
  nrep = 100,
  m = sim_means, 
  s = sim_sd,
  pc = 15)

set.seed(1006)

generate_power_data(
  nrep = 100,
  m = sim_means, 
  s = sim_sd,
  pc = 20)

set.seed(7)

generate_power_data(
  nrep = 100,
  m = sim_means,
  s = sim_sd,
  pc = 25)

set.seed(1009)

generate_power_data(
  nrep = 100,
  m = sim_means, 
  s = sim_sd,
  pc = 30)

set.seed(1010)

generate_power_data(
  nrep = 100,
  m = sim_means, 
  s = sim_sd,
  pc = 35)

set.seed(1011)

generate_power_data(
  nrep = 100,
  m = sim_means, 
  s = sim_sd,
  pc = 40)

set.seed(1012)

generate_power_data(
  nrep = 100,
  m = sim_means, 
  s = sim_sd,
  pc = 45)

set.seed(8)

generate_power_data(
  nrep = 100,
  m = sim_means,
  s = sim_sd,
  pc = 50)

set.seed(9)

generate_power_data(
  nrep = 100,
  m = sim_means,
  s = sim_sd,
  pc = 100)

set.seed(23)

generate_fdr_data(
  nrep = 100,
  m = sim_means,
  s = sim_sd,
  pc = 100,
  mult = mult)

set.seed(15)

generate_fdr_data(
  nrep = 100,
  m = sim_means,
  s = sim_sd,
  pc = 125,
  mult = mult)

set.seed(16)

generate_fdr_data(
  nrep = 100,
  m = sim_means,
  s = sim_sd,
  pc = 150,
  mult = mult)

set.seed(17)

generate_fdr_data(
  nrep = 100,
  m = sim_means,
  s = sim_sd,
  pc = 200,
  mult = mult)