library(ggplot2)
library(patchwork)

# set file location
plot_data_dir = "./_plot_data"
plot_dir = "./_plots"
fpath = file.path(plot_data_dir, "simulation_results.rds")
# read data
simdf = readRDS(fpath)

# power results by adjusted
power_data_by <- simdf |>
  subset(subset  = (permutation == "stratified")) |>
  subset(subset = (adjustment == "BY")) |>
  subset(subset =
                  (sim == "power_sim_15") |
                  (sim == "power_sim_20") |
                  (sim == "power_sim_25") |
                  (sim == "power_sim_30") |
                  (sim == "power_sim_35"))

power_data_by$c <- factor(power_data_by$sim)
levels(power_data_by$c) = c("0.15", "0.20", "0.25", "0.30", "0.35")
power_data_by$statistic[power_data_by$statistic == "de"] = "(a) dist. equality"
power_data_by$statistic[power_data_by$statistic == "mean"] = "(b) mean"

# plot fill results faceted by statistic
power_sim_by_results = 
  ggplot(data = power_data_by) +
  geom_point(aes(x = prob, y = significant,
                 col = c,
                 shape = c)) + 
  facet_grid(~statistic) +
  xlab("significance level") +
  ylab("BY-adjusted empirical power")  +
  ggtitle("Non-null scenario 1") + 
  scale_color_brewer(type = "qual", palette = "Dark2")
print(power_sim_by_results)
# ggsave(power_sim_by_results,
#        file = file.path(plot_dir, "power_sim_by_results.pdf"),
#        height = 2.5, width = 6, units = "in")

##### results for partial mean change

# fdr results by adjusted
fdr_data_by = 
  simdf |> subset(subset =
                    ((sim == "fdr_sim_100") |
                       (sim == "fdr_sim_125") |
                       (sim == "fdr_sim_150") |
                       (sim == "fdr_sim_200")) &
                    (adjustment == "BY") &
                    (permutation == "stratified")
  )
fdr_data_by$c = factor(fdr_data_by$sim,
                         levels = c("fdr_sim_100",
                                    "fdr_sim_125",
                                    "fdr_sim_150",
                                    "fdr_sim_200"))
levels(fdr_data_by$c) = c("1.0", "1.25", "1.5", "2")
fdr_data_by$statistic[fdr_data_by$statistic == "de"] = "(c) dist. equality"
fdr_data_by$statistic[fdr_data_by$statistic == "mean"] = "(d) mean"

# plot fill results faceted by statistic
fdr_sim_by_results = 
  ggplot(data = fdr_data_by) +
  geom_point(aes(x = prob, y = significant,
                 col = c,
                 shape = c)) + 
  facet_grid(~statistic) +
  xlab("significance level") + 
  ylab("BY-adjusted empirical power") +
  ggtitle("Non-null scenario 2") + 
  scale_color_brewer(type = "qual", palette = "Dark2")
print(fdr_sim_by_results)
# ggsave(fdr_sim_by_results,
#       file = file.path(plot_dir, "fdr_sim_by_results.pdf"),
#       height = 2.5, width = 6, units = "in")

print(power_sim_by_results/fdr_sim_by_results)
combined_plot <- power_sim_by_results/fdr_sim_by_results
ggsave(combined_plot,
       file = file.path(plot_dir, "power_results.pdf"),
       height = 5, width = 6, units = "in")

