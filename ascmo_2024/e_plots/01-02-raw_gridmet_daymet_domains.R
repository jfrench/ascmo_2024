library(autoimage)

# change this to where your processed data are placed
processed_data_dir = "./_processed_data"
# place to store data for plots
plot_data_dir = "./_plot_data"
# plot to store plots
plot_dir = "./_plots"

# number of desired time steps
desired_length = 660

# # list of all nacordex 44i files
# m_files = c(
#   "tmax.hist.CanESM2.RCA4.mon.NAM-44i.mbcn-gridMET.rds",
#   "tmax.hist.EC-EARTH.RCA4.mon.NAM-44i.mbcn-gridMET.rds",
#   "tmax.hist.CanESM2.CRCM5-UQAM.mon.NAM-44i.mbcn-gridMET.rds",
#   "tmax.hist.GEMatm-Can.CRCM5-UQAM.mon.NAM-44i.mbcn-gridMET.rds",
#   "tmax.hist.GEMatm-MPI.CRCM5-UQAM.mon.NAM-44i.mbcn-gridMET.rds",
#   "tmax.hist.MPI-ESM-LR.CRCM5-UQAM.mon.NAM-44i.mbcn-gridMET.rds",
#   "tmax.hist.MPI-ESM-MR.CRCM5-UQAM.mon.NAM-44i.mbcn-gridMET.rds",
#   "tmax.hist.CanESM2.CanRCM4.mon.NAM-44i.mbcn-gridMET.rds",
#   "tmax.hist.EC-EARTH.HIRHAM5.mon.NAM-44i.mbcn-gridMET.rds",
#   "tmax.hist.GFDL-ESM2M.RegCM4.mon.NAM-44i.mbcn-gridMET.rds",
#   "tmax.hist.HadGEM2-ES.RegCM4.mon.NAM-44i.mbcn-gridMET.rds",
#   "tmax.hist.GFDL-ESM2M.WRF.mon.NAM-44i.mbcn-gridMET.rds",
#   "tmax.hist.HadGEM2-ES.WRF.mon.NAM-44i.mbcn-gridMET.rds",
#   "tmax.hist.MPI-ESM-LR.WRF.mon.NAM-44i.mbcn-gridMET.rds",
#   "tmax.hist.MPI-ESM-LR.RegCM4.mon.NAM-44i.mbcn-gridMET.rds"
# )
# 
# # create list to store objects
# x = vector("list", length(m_files))
# 
# for (i in seq_along(m_files)) {
#   x[[i]] = readRDS(file.path(processed_data_dir, m_files[i]))
#   dim3 = dim(x[[i]])[3]
#   if (dim3 != desired_length) {
#     desired_start = dim3 - desired_length + 1
#     x[[i]] = x[[i]][,,seq(desired_start, dim3, by = 1)]
#   }
# }
# 
# gridMET_array = do.call(abind::abind, list(x, along = 3))
# gridMET_mean = apply(gridMET_array, 1:2, mean)
# 
# # list of all nacordex 44i files
# m_files = c(
#   "tmax.hist.CanESM2.RCA4.mon.NAM-44i.mbcn-Daymet.rds",
#   "tmax.hist.EC-EARTH.RCA4.mon.NAM-44i.mbcn-Daymet.rds",
#   "tmax.hist.CanESM2.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.rds",
#   "tmax.hist.GEMatm-Can.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.rds",
#   "tmax.hist.GEMatm-MPI.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.rds",
#   "tmax.hist.MPI-ESM-LR.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.rds",
#   "tmax.hist.MPI-ESM-MR.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.rds",
#   "tmax.hist.CanESM2.CanRCM4.mon.NAM-44i.mbcn-Daymet.rds",
#   "tmax.hist.EC-EARTH.HIRHAM5.mon.NAM-44i.mbcn-Daymet.rds",
#   "tmax.hist.GFDL-ESM2M.RegCM4.mon.NAM-44i.mbcn-Daymet.rds",
#   "tmax.hist.HadGEM2-ES.RegCM4.mon.NAM-44i.mbcn-Daymet.rds",
#   "tmax.hist.GFDL-ESM2M.WRF.mon.NAM-44i.mbcn-Daymet.rds",
#   "tmax.hist.HadGEM2-ES.WRF.mon.NAM-44i.mbcn-Daymet.rds",
#   "tmax.hist.MPI-ESM-LR.WRF.mon.NAM-44i.mbcn-Daymet.rds",
#   "tmax.hist.MPI-ESM-LR.RegCM4.mon.NAM-44i.mbcn-Daymet.rds"
# )
# 
# # create list to store objects
# x = vector("list", length(m_files))
# 
# for (i in seq_along(m_files)) {
#   x[[i]] = readRDS(file.path(processed_data_dir, m_files[i]))
#   dim3 = dim(x[[i]])[3]
#   if (dim3 != desired_length) {
#     desired_start = dim3 - desired_length + 1
#     x[[i]] = x[[i]][,,seq(desired_start, dim3, by = 1)]
#   }
# }
# 
# Daymet_array = do.call(abind::abind, list(x, along = 3))
# Daymet_mean = apply(Daymet_array, 1:2, mean)
# 
# # read lon and lat coordinates for gridMET data
# lon = readRDS(file = file.path(processed_data_dir,
#                                "lon_nacordex_44i_gridMET.rds"))
# lat = readRDS(file = file.path(processed_data_dir,
#                                "lat_nacordex_44i_gridMET.rds"))
# 
# if (!dir.exists(plot_data_dir)) {
#   dir.create(plot_data_dir)
# }
# 
# obs_means = abind::abind(gridMET_mean, Daymet_mean, along = 3)
# save(lon, lat, obs_means,
#      file = file.path(plot_data_dir, "gridmet_daymet_domain_data.rda"),
#      compress = "xz")

load(file = file.path(plot_data_dir, "tmax_mean_raw.rda"))
load(file = file.path(plot_data_dir, "gridmet_daymet_domain_data.rda"))

if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
}



obs_means[,,1] = obs_means[,,1] - tmax_mean
obs_means[,,2] = obs_means[,,2] - tmax_mean
obs_means = abind::abind(tmax_mean, obs_means, along = 3)

png(file.path(plot_dir, "raw_gridmet_daymet_domains.png"),
    height = 4, width = 12, units = "in", res = 300)
autoimage(lon, lat, obs_means,
          xlab = "longitude",
          ylab = "latitude",
          main = c("(a) Raw", "(b) gridMET adjustment", "(c) Daymet adjustment"),
          common.legend = FALSE,
          legend = "horizontal",
          size = c(1, 3),
          map = "world")
dev.off()

