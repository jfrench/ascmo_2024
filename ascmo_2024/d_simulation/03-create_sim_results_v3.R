# change this to where your processed data are placed
simdata_dir = "./_sim_data"

if (!dir.exists("./_sim_results")) {
  dir.create("./_sim_results")
}

# directory to save results
results_dir = "./_sim_results"

nrep = 100
B = 999

# determine if running on cluster
os <- Sys.info()["sysname"]
attr(os, "names") <- NULL
mc_cores <- ifelse(os == "Windows", 1L, 20L)

# read functions file
source('fcmc_functions_v2.R')
source('fcmc_power_functions.R')

# generate exact permutations for this test
perm_idxs = prepare_perm_idxs(10, 1)

save_sim_results(nrep = nrep, seed = 100, B = B,
                 perm_idxs = perm_idxs, cl = mc_cores,
                 simdata_dir = simdata_dir,
                 sim_name = "null_sim",
                 results_dir = results_dir)

save_sim_results(nrep = nrep, seed = 200, B = B,
                 perm_idxs = perm_idxs, cl = mc_cores,
                 simdata_dir = simdata_dir,
                 sim_name = "power_sim_10",
                 results_dir = results_dir)

save_sim_results(nrep = nrep, seed = 220, B = B,
                 perm_idxs = perm_idxs, cl = mc_cores,
                 simdata_dir = simdata_dir,
                 sim_name = "power_sim_15",
                 results_dir = results_dir)

save_sim_results(nrep = nrep, seed = 240, B = B,
                 perm_idxs = perm_idxs, cl = mc_cores,
                 simdata_dir = simdata_dir,
                 sim_name = "power_sim_20",
                 results_dir = results_dir)

save_sim_results(nrep = nrep, seed = 300, B = B,
                 perm_idxs = perm_idxs, cl = mc_cores,
                 simdata_dir = simdata_dir,
                 sim_name = "power_sim_25",
                 results_dir = results_dir)

save_sim_results(nrep = nrep, seed = 310, B = B,
                 perm_idxs = perm_idxs, cl = mc_cores,
                 simdata_dir = simdata_dir,
                 sim_name = "power_sim_30",
                 results_dir = results_dir)

save_sim_results(nrep = nrep, seed = 320, B = B,
                 perm_idxs = perm_idxs, cl = mc_cores,
                 simdata_dir = simdata_dir,
                 sim_name = "power_sim_35",
                 results_dir = results_dir)

save_sim_results(nrep = nrep, seed = 330, B = B,
                 perm_idxs = perm_idxs, cl = mc_cores,
                 simdata_dir = simdata_dir,
                 sim_name = "power_sim_40",
                 results_dir = results_dir)

save_sim_results(nrep = nrep, seed = 340, B = B,
                 perm_idxs = perm_idxs, cl = mc_cores,
                 simdata_dir = simdata_dir,
                 sim_name = "power_sim_45",
                 results_dir = results_dir)

save_sim_results(nrep = nrep, seed = 400, B = B,
                 perm_idxs = perm_idxs, cl = mc_cores,
                 simdata_dir = simdata_dir,
                 sim_name = "power_sim_50",
                 results_dir = results_dir)

save_sim_results(nrep = nrep, seed = 500, B = B,
                 perm_idxs = perm_idxs, cl = mc_cores,
                 simdata_dir = simdata_dir,
                 sim_name = "power_sim_100",
                 results_dir = results_dir)

save_sim_results(nrep = nrep, seed = 540, B = B,
                 perm_idxs = perm_idxs, cl = mc_cores,
                 simdata_dir = simdata_dir,
                 sim_name = "fdr_sim_100",
                 results_dir = results_dir)

save_sim_results(nrep = nrep, seed = 800, B = B,
                 perm_idxs = perm_idxs, cl = mc_cores,
                 simdata_dir = simdata_dir,
                 sim_name = "fdr_sim_125",
                 results_dir = results_dir)

save_sim_results(nrep = nrep, seed = 900, B = B,
                 perm_idxs = perm_idxs, cl = mc_cores,
                 simdata_dir = simdata_dir,
                 sim_name = "fdr_sim_150",
                 results_dir = results_dir)

save_sim_results(nrep = nrep, seed = 1000, B = B,
                 perm_idxs = perm_idxs, cl = mc_cores,
                 simdata_dir = simdata_dir,
                 sim_name = "fdr_sim_200",
                 results_dir = results_dir)