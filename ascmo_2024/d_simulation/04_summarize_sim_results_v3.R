# directory where sim results are saved
results_dir = "./_sim_results"

source("fcmc_power_functions.R")

sig_results_null = 
  combine_results(results_dir, "null_sim")
sig_results_power_10 = 
  combine_results(results_dir, "power_sim_10")
sig_results_power_15 = 
  combine_results(results_dir, "power_sim_15")
sig_results_power_20 = 
  combine_results(results_dir, "power_sim_20")
sig_results_power_25 = 
  combine_results(results_dir, "power_sim_25")
sig_results_power_30 = 
  combine_results(results_dir, "power_sim_30")
sig_results_power_35 = 
  combine_results(results_dir, "power_sim_35")
sig_results_power_40 = 
  combine_results(results_dir, "power_sim_40")
sig_results_power_45 = 
  combine_results(results_dir, "power_sim_45")
sig_results_power_50 = 
  combine_results(results_dir, "power_sim_50")
sig_results_power_100 = 
  combine_results(results_dir, "power_sim_100")
sig_results_power_100_partial = 
  combine_results(results_dir, "fdr_sim_100")
sig_results_power_125_partial = 
  combine_results(results_dir, "fdr_sim_125")
sig_results_power_150_partial = 
  combine_results(results_dir, "fdr_sim_150")
sig_results_power_200_partial = 
  combine_results(results_dir, "fdr_sim_200")

df = rbind(sig_results_null,
           sig_results_power_10,
           sig_results_power_15,
           sig_results_power_20,
           sig_results_power_25,
           sig_results_power_30,
           sig_results_power_35,
           sig_results_power_40,
           sig_results_power_45,
           sig_results_power_50,
           sig_results_power_100,
           sig_results_power_100_partial,
           sig_results_power_125_partial,
           sig_results_power_150_partial,
           sig_results_power_200_partial
           )
saveRDS(df,
        file = file.path("./_plot_data", "simulation_results.rds"),
        compress = TRUE)

set.seed(33)
er_results_df = er_results(results_dir, "null_sim", size = 20)
saveRDS(er_results_df,
        file = file.path("./_plot_data", "null_er_results.rds"),
        compress = TRUE)

