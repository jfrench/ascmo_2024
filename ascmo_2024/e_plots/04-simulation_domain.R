library(autoimage)

# change this to where your processed data are placed
sim_data_dir = "./_sim_data"
# place to store data for plots
plot_data_dir = "./_plot_data"
# plot to store plots
plot_dir = "./_plots"

load(file.path(sim_data_dir, "sim_params.rda"))

if (!dir.exists(plot_data_dir)) {
  dir.create(plot_data_dir)
}

lat_idx_sub = seq(21-4, 21+4)
lon_idx_sub = seq(15-4, 15+4)
means_mat = matrix(sim_means[,1], nrow = 42, ncol = 32)

subpoly = rbind(
  cbind(lon_sim[11], lat_sim[lat_idx_sub]),
  cbind(lon_sim[lon_idx_sub], lat_sim[17]),
  cbind(lon_sim[19], rev(lat_sim[lat_idx_sub])),
  cbind(rev(lon_sim[lon_idx_sub]), lat_sim[25])
)
poly <- data.frame(x = subpoly[,1], y = subpoly[,2])

png(file.path(plot_dir, "sim_domain.png"),
    height = 6, width = 7, units = "in", res = 300)
pimage(lon_sim, lat_sim, means_mat,
       map = "state", 
       xlim = c(-125, -67),
       ylim = c(24, 50),
       xlab = "longitude",
       ylab = "latitude")
title("Average temperature (C) in January 1980")
plines(poly, proj = "none", col = "grey", lwd = 3)
dev.off()