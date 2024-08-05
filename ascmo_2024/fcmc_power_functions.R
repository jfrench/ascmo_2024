# spatial average of bordering pixels
# takes space-time array and averages
# interior pixels with bordering pixels
# (edges not corners)
sma = function(x) {
  nr = dim(x)[1]
  nc = dim(x)[2]
  (x[2:(nr - 1), 2:(nc - 1),] + 
      x[1:(nr - 2), 2:(nc - 1),] + 
      x[3:(nr), 2:(nc - 1),] + 
      x[2:(nr - 1), 1:(nc - 2),] + 
      x[2:(nr - 1), 3:(nc),])/5
}

# run sim test for each data set
save_sim_results = function(nrep, seed, 
                            B, perm_idxs, cl,
                            simdata_dir, sim_name,
                            results_dir) {
  for (i in seq_len(nrep)) {
    x = readRDS(file.path(simdata_dir,
                          paste0(sim_name, "_", i, "_sta.rds")))
    
    set.seed(seed + i)
    
    fout = file.path(results_dir,
                     paste0("std_perm_tests_de_", sim_name, "_", i, "_sta.rds"))
    
    if (!file.exists(fout)) {
      std_perm_de_sim = perm_test_de_sta(x,
                                         perm_idxs = perm_idxs,
                                         cl = mc_cores)
      saveRDS(std_perm_de_sim,
              file = fout, compress = "xz")
    }
    
    set.seed(seed + 2 * i)
    
    fout = file.path(results_dir,
                     paste0("sp_test_de_", sim_name, "_", i, "_sta.rds"))
    
    if(!file.exists(fout)) {
      sp_test_de_sim = strat_perm_test_de_sta(x,
                                              nsim = B,
                                              cl = mc_cores)
      sp_test_de_sim$tstats = sp_test_de_sim$tstats[B + 1,]
      saveRDS(sp_test_de_sim,
              file = fout,
              compress = "xz")
    }
    
    set.seed(seed + 3 * i)
    
    fout = file.path(results_dir,
                     paste0("std_perm_tests_mean_", sim_name, "_", i, "_sta.rds"))
    
    if(!file.exists(fout)) {
      std_perm_test_mean_sim = perm_test_sta(x,
                                             stat_func = mean,
                                             alternative = "two.sided",
                                             perm_idxs = perm_idxs,
                                             cl = mc_cores)
      saveRDS(std_perm_test_mean_sim,
              file = fout,
              compress = "xz")
    }
    
    set.seed(seed + 4 * i)
    
    fout = file.path(results_dir,
                     paste0("sp_test_mean_", sim_name, "_", i, "_sta.rds"))
    
    if(!file.exists(fout)) {
      sp_test_mean_sim = strat_perm_test_sta(x, 
                                             stat_func = mean,
                                             alternative = "two.sided",
                                             nsim = B,
                                             cl = mc_cores)
      sp_test_mean_sim$tstats = sp_test_mean_sim$tstats[B + 1,]
      saveRDS(sp_test_mean_sim,
              file = fout,
              compress = "xz")
    }
  }
}

# simulate ar with changing mean and variance
# average spatially
sim_stdata = function(nm = 10, m, s, nr, nc) {
  sim1 = lapply(seq_len(nm), function(i) {
    t(sapply(seq_len(nrow(m)), function(j) {
      arima.sim(n = length(m[j,]),
                list(ar = 0.1),
                mean = m[j,], sd = s[j,])
    }))
  })
  # convert each matrix to a 3d array
  sim2 = lapply(sim1, array, dim = c(nr, nc, dim(m)[2]))
  sim3 = lapply(sim2, sma)
  prepare_str_starray(sim3, 
                      grp = factor(rep(c("m", "r"), times = c(9, 1))),
                      strata = factor(rep(1:25, each = 12)))
}

# last data generated will have a mean increase
# by pc * s
sim_stdata_power = function(nm = 10, m, s, nr, nc, pc) {
  sim1 = lapply(seq_len(nm), function(i) {
    if (i != nm) {
      t(sapply(seq_len(nrow(m)), function(j) {
        arima.sim(n = length(m[j,]),
                  list(ar = 0.1),
                  mean = m[j,], sd = s[j,])
      }))
    } else {
      t(sapply(seq_len(nrow(m)), function(j) {
        arima.sim(n = length(m[j,]),
                  list(ar = 0.1),
                  mean = m[j,] + pc/100 * s[j,], sd = s[j,])
      }))
    }
  })
  # convert each matrix to a 3d array
  sim2 = lapply(sim1, array, dim = c(nr, nc, dim(m)[2]))
  sim3 = lapply(sim2, sma)
  prepare_str_starray(sim3, 
                      grp = factor(rep(c("m", "r"), times = c(9, 1))),
                      strata = factor(rep(1:25, each = 12)))
}

# last data generated will have a mean increase
# by pc * s
sim_stdata_power2 = function(nm = 10, m, s, nr, nc, pc, mult) {
  sim1 = lapply(seq_len(nm), function(i) {
    if (i != nm) {
      t(sapply(seq_len(nrow(m)), function(j) {
        arima.sim(n = length(m[j,]),
                  list(ar = 0.1),
                  mean = m[j,], sd = s[j,])
      }))
    } else {
      t(sapply(seq_len(nrow(m)), function(j) {
        arima.sim(n = length(m[j,]),
                  list(ar = 0.1),
                  mean = m[j,] + (pc * multmat[j,])/100 * s[j,], sd = s[j,])
      }))
    }
  })
  # convert each matrix to a 3d array
  sim2 = lapply(sim1, array, dim = c(nr, nc, dim(m)[2]))
  sim3 = lapply(sim2, sma)
  prepare_str_starray(sim3, 
                      grp = factor(rep(c("m", "r"), times = c(9, 1))),
                      strata = factor(rep(1:25, each = 12)))
}

# generate all null data
generate_null_data = function(nrep = 100,
                              nm = 10,
                              m = 0,
                              s = 1,
                              nr = 32,
                              nc = 42,
                              fpath = "./_sim_data") {
  lapply(seq_len(nrep), function(r) {
    x = sim_stdata(nm, m, s, nr, nc)
    saveRDS(x,
            file = file.path(fpath,
                             paste0("null_sim_",
                                    r,
                                    "_sta.rds")),
            compress = "xz")
  })
}

# generate power data with shift for all time steps
generate_power_data = function(nrep = 100,
                               nm = 10,
                               m = 0,
                               s = 1,
                               nr = 32,
                               nc = 42,
                               pc = 10,
                               fpath = "./_sim_data") {
  lapply(seq_len(nrep), function(r) {
    x = sim_stdata_power(nm, m, s, nr, nc, pc)
    saveRDS(x,
            file = file.path(fpath,
                             paste0("power_sim_",
                                    pc,
                                    "_",
                                    r,
                                    "_sta.rds")),
            compress = "xz")
  })
}

# generate data for power test where mean
# is shifted after multipling m by multmat
# so that only some times are shifted
generate_fdr_data = function(nrep = 100,
                             nm = 10,
                             m = 0,
                             s = 1,
                             nr = 32,
                             nc = 42,
                             pc = 10,
                             multmat = matrix(0, 32 * 42, 660),
                             fpath = "./_sim_data") {
  lapply(seq_len(nrep), function(r) {
    x = sim_stdata_power2(nm, m, s, nr, nc, pc, multmat)
    saveRDS(x,
            file = file.path(fpath,
                             paste0("fdr_sim_",
                                    pc,
                                    "_",
                                    r,
                                    "_sta.rds")),
            compress = "xz")
  })
}

# read sim results for specific simulation
read_sim_results = function(results_dir, 
                            test_name,
                            sim_name,
                            nrep = 100) {
  x = vector("list", nrep)
  for (i in seq_len(nrep)) {
    fpath1 = file.path(results_dir,
                       paste0(test_name,
                              sim_name, "_",
                              i, "_sta.rds"))
    x[[i]] = readRDS(fpath1)
  }
  smerc::sgetElement(x, "pvalues")
}

# compute empirical error rate
er = function(prob, pvals) {
  mean(pvals <= prob)
}

# compute empirical error rate
count_er_sample = function(prob, pvals, size = 20) {
  sum(sample(pvals, size = size) <= prob)
}

# combine results for specific simulation
combine_results = function(results_dir, sim_name,
                           p1 = c(0.1, 0.2, 0.3, 0.4, 0.5),
                           p2 = seq(0.05, 0.5, by = 0.05)) {
  std_de_pvals = read_sim_results(results_dir, 
                                  "std_perm_tests_de_",
                                  sim_name)
  sp_de_pvals = read_sim_results(results_dir, 
                                 "sp_test_de_",
                                 sim_name)
  std_mean_pvals = read_sim_results(results_dir, 
                                    "std_perm_tests_mean_",
                                    sim_name)
  sp_mean_pvals = read_sim_results(results_dir, 
                                   "sp_test_mean_",
                                   sim_name)
  sig_std_de = sapply(p1, er, pvals = std_de_pvals)
  sig_sp_de = sapply(p2, er, pvals = sp_de_pvals)
  sig_std_mean = sapply(p1, er, pvals = std_mean_pvals)
  sig_sp_mean = sapply(p2, er, pvals = sp_mean_pvals)
  
  std_de_pvals_by = apply(std_de_pvals, 2, 
                          p.adjust, method = "BY")
  sp_de_pvals_by = apply(sp_de_pvals, 2, 
                         p.adjust, method = "BY")
  std_mean_pvals_by = apply(std_de_pvals, 2, 
                            p.adjust, method = "BY")
  sp_mean_pvals_by = apply(sp_de_pvals, 2, 
                           p.adjust, method = "BY")
  sig_std_de_by = sapply(p1, er, pvals = std_de_pvals_by)
  sig_sp_de_by = sapply(p2, er, pvals = sp_de_pvals_by)
  sig_std_mean_by = sapply(p1, er, pvals = std_mean_pvals_by)
  sig_sp_mean_by = sapply(p2, er, pvals = sp_mean_pvals_by)
  df_std_de = data.frame(prob = p1,
                         significant = sig_std_de,
                         permutation = "standard",
                         statistic = "dist. equality",
                         adjustment = "none")
  df_sp_de = data.frame(prob = p2,
                        significant = sig_sp_de,
                        permutation = "stratified",
                        statistic = "dist. equality",
                        adjustment = "none")
  df_std_mean = data.frame(prob = p1,
                           significant = sig_std_mean,
                           permutation = "standard",
                           statistic = "mean",
                           adjustment = "none")
  df_sp_mean = data.frame(prob = p2,
                          significant = sig_sp_mean,
                          permutation = "stratified",
                          statistic = "mean",
                          adjustment = "none")
  df_std_de_by = data.frame(prob = p1,
                            significant = sig_std_de_by,
                            permutation = "standard",
                            statistic = "de",
                            adjustment = "BY")
  df_sp_de_by = data.frame(prob = p2,
                           significant = sig_sp_de_by,
                           permutation = "stratified",
                           statistic = "de",
                           adjustment = "BY")
  df_std_mean_by = data.frame(prob = p1, 
                              significant = sig_std_mean_by,
                              permutation = "standard",
                              statistic = "mean",
                              adjustment = "BY")
  df_sp_mean_by = data.frame(prob = p2,
                             significant = sig_sp_mean_by,
                             permutation = "stratified",
                             statistic = "mean",
                             adjustment = "BY")
  out = 
    rbind(df_std_de,
          df_sp_de,
          df_std_mean,
          df_sp_mean,
          df_std_de_by,
          df_sp_de_by,
          df_std_mean_by,
          df_sp_mean_by)
  cbind(out, sim = sim_name)
}

# combine results for specific simulation
er_results = function(results_dir, sim_name,
                      p1 = c(0.1, 0.2, 0.3, 0.4, 0.5),
                      p2 = seq(0.05, 0.5, by = 0.05),
                      size = 20) {
  std_de_pvals = read_sim_results(results_dir, 
                                  "std_perm_tests_de_",
                                  sim_name)
  sp_de_pvals = read_sim_results(results_dir, 
                                 "sp_test_de_",
                                 sim_name)
  std_mean_pvals = read_sim_results(results_dir, 
                                    "std_perm_tests_mean_",
                                    sim_name)
  sp_mean_pvals = read_sim_results(results_dir, 
                                   "sp_test_mean_",
                                   sim_name)
  std_de_pvals_sample = apply(std_de_pvals, 2, 
                             sample, size = size)
  sp_de_pvals_sample = apply(sp_de_pvals, 2, 
                             sample, size = size)
  std_mean_pvals_sample = apply(std_mean_pvals, 2, 
                             sample, size = size)
  sp_mean_pvals_sample = apply(sp_mean_pvals, 2, 
                             sample, size = size)

  sig_std_de = sapply(p1, er, pvals = std_de_pvals_sample)
  sig_sp_de = sapply(p2, er, pvals = sp_de_pvals_sample)
  sig_std_mean = sapply(p1, er, pvals = std_mean_pvals_sample)
  sig_sp_mean = sapply(p2, er, pvals = sp_mean_pvals_sample)
  
  df_std_de = data.frame(prob = p1,
                         significant = sig_std_de,
                         permutation = "standard",
                         statistic = "dist. equality",
                         adjustment = "none")
  df_sp_de = data.frame(prob = p2,
                        significant = sig_sp_de,
                        permutation = "stratified",
                        statistic = "dist. equality",
                        adjustment = "none")
  df_std_mean = data.frame(prob = p1,
                           significant = sig_std_mean,
                           permutation = "standard",
                           statistic = "mean",
                           adjustment = "none")
  df_sp_mean = data.frame(prob = p2,
                          significant = sig_sp_mean,
                          permutation = "stratified",
                          statistic = "mean",
                          adjustment = "none")
  out = 
    rbind(df_std_de,
          df_sp_de,
          df_std_mean,
          df_sp_mean)
  cbind(out, sim = sim_name)
}