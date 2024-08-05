library(ggplot2)

# set file location
plot_data_dir = "./_plot_data"
plot_dir = "./_plots"
fpath = file.path(plot_data_dir, "simulation_results.rds")
# read data
simdf = readRDS(fpath)

npath = file.path(plot_data_dir, "null_er_results.rds")

null_data = readRDS(npath)
  # simdf |> subset(subset =
  #                   (sim == "null_sim") &
  #                   (adjustment == "none"))

# probabilities
p1 = seq(0.1, 0.5, by = 0.1)
p2 = seq(0.05, 0.5, by = 0.05)
# margin of errors
cil = qbinom(0.025, size = 2000, prob = p2)/2000
ciu = qbinom(0.975, size = 2000, prob = p2)/2000

cidf = data.frame(prob = p2,
                  cil = cil,
                  ciu = ciu)

# plot null results faceted by statistic
null_sim_results = 
ggplot(data = null_data) +
  geom_linerange(data = cidf, aes(x = prob,
                                  ymin = cil, 
                                  ymax = ciu)) +
  geom_point(aes(x = prob, y = significant,
                shape = permutation,
                col = permutation)) + 
  scale_color_manual(values = c("orange", "blue")) +
  facet_grid(~statistic) +
  xlab("significance level") + 
  ylab("empirical type I error rate")
print(null_sim_results)
ggsave(null_sim_results,
       file = file.path(plot_dir, "null_sim_results.pdf"),
       height = 2.5, width = 6, units = "in")

std_dist_sig <- null_data |> subset(permutation == "standard" &
                                    statistic == "dist. equality")
std_mean_sig <- null_data |> subset(permutation == "standard" &
                                    statistic == "mean")

sp_dist_sig <- null_data |> subset(permutation == "stratified" &
                                   statistic == "dist. equality")
sp_mean_sig <- null_data |> subset(permutation == "stratified" &
                                   statistic == "mean")

cidf_std <- cidf |> subset(prob == 0.1 |  
                              prob == 0.2 |
                              prob == 0.3 | 
                              prob == 0.4 |
                              prob == 0.5)

# number of estimates not in tolerance envelopes
sum(cidf_std$cil > std_dist_sig$significant | cidf_std$ciu < std_dist_sig$significant)
sum(cidf_std$cil > std_mean_sig$significant | cidf_std$ciu < std_mean_sig$significant)
sum(cidf$cil > sp_dist_sig$significant | cidf$ciu < sp_dist_sig$significant)
sum(cidf$cil > sp_mean_sig$significant | cidf$ciu < sp_mean_sig$significant)