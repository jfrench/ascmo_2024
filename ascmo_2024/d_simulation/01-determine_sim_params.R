set.seed(55)

# change this to where your processed data are placed
processed_data_dir = "./_processed_data"

# source directory with fcmc.functions.R
source_dir = "./"

# directory to save results
results_dir = "./_results_gridMET"

# number of desired time steps
desired_length = 660

# determine if running on cluster
os <- Sys.info()["sysname"]
attr(os, "names") <- NULL
mc_cores <- ifelse(os == "Windows", 1L, 20L)

# list of all nacordex 44i files
m_files = c(
  "tmax.hist.CanESM2.RCA4.mon.NAM-44i.mbcn-gridMET.rds",
  "tmax.hist.EC-EARTH.RCA4.mon.NAM-44i.mbcn-gridMET.rds",
  "tmax.hist.CanESM2.CRCM5-UQAM.mon.NAM-44i.mbcn-gridMET.rds",
  "tmax.hist.GEMatm-Can.CRCM5-UQAM.mon.NAM-44i.mbcn-gridMET.rds",
  "tmax.hist.GEMatm-MPI.CRCM5-UQAM.mon.NAM-44i.mbcn-gridMET.rds",
  "tmax.hist.MPI-ESM-LR.CRCM5-UQAM.mon.NAM-44i.mbcn-gridMET.rds",
  "tmax.hist.MPI-ESM-MR.CRCM5-UQAM.mon.NAM-44i.mbcn-gridMET.rds",
  "tmax.hist.CanESM2.CanRCM4.mon.NAM-44i.mbcn-gridMET.rds",
  "tmax.hist.EC-EARTH.HIRHAM5.mon.NAM-44i.mbcn-gridMET.rds",
  "tmax.hist.GFDL-ESM2M.RegCM4.mon.NAM-44i.mbcn-gridMET.rds",
  "tmax.hist.HadGEM2-ES.RegCM4.mon.NAM-44i.mbcn-gridMET.rds",
  "tmax.hist.GFDL-ESM2M.WRF.mon.NAM-44i.mbcn-gridMET.rds",
  "tmax.hist.HadGEM2-ES.WRF.mon.NAM-44i.mbcn-gridMET.rds",
  "tmax.hist.MPI-ESM-LR.WRF.mon.NAM-44i.mbcn-gridMET.rds",
  "tmax.hist.MPI-ESM-LR.RegCM4.mon.NAM-44i.mbcn-gridMET.rds"
)

r_files = c(
  "era5_monthly_tmax_nacordex_44i_Daymet.rds"
)

# create list to store objects
x = vector("list", length(m_files) + length(r_files))
# create grouping variable
grp = factor(rep(c("m", "r"), times = c(length(m_files), length(r_files))))
# create stratifying variable
strata = factor(rep(1950:2004, each = 12))

for (i in seq_along(m_files)) {
  x[[i]] = readRDS(file.path(processed_data_dir, m_files[i]))
  dim3 = dim(x[[i]])[3]
  if (dim3 != desired_length) {
    desired_start = dim3 - desired_length + 1
    x[[i]] = x[[i]][,,seq(desired_start, dim3, by = 1)]
  }
}

# read reanalysis data
# adjust from Kelvin to Celsius
for (i in seq_along(r_files)) {
  x[[i + length(m_files)]] = readRDS(file.path(processed_data_dir,
                                               r_files[i]))
  # adjust if temperature in Kelvin not Celsius
  if (max(x[[i + length(m_files)]], na.rm = TRUE) > 150) {
    x[[i + length(m_files)]] = x[[i + length(m_files)]] - 273.15
  }
  
  dim3 = dim(x[[i + length(m_files)]])[3]
  
  if (dim3 != desired_length) {
    desired_start = dim3 - desired_length + 1
    x[[i + length(m_files)]] = x[[i + length(m_files)]][,,seq(desired_start, dim3, by = 1)]
  }
}

# remove reanalysis data
x[[16]] <- NULL

# read lon and lat coordinates for Daymet data
lon = readRDS(file = file.path(processed_data_dir,
                               "lon_nacordex_44i_gridMET.rds"))
lat = readRDS(file = file.path(processed_data_dir,
                               "lat_nacordex_44i_gridMET.rds"))

# read na information
namat = readRDS(file = file.path(processed_data_dir,
                                 "mask_nacordex_44i_gridMET.rds"))
idx = which(!is.na(namat), arr.ind = TRUE)

x_array <- array(dim = c(nrow(idx),
                         dim(x[[1]])[3],
                         length(x)))

# convert to array
for (i in seq_along(x)) {
  x_array[,,i] = t(apply(idx, MARGIN = 1, FUN = function(temp_idx) {
    idx1 = temp_idx[1]
    idx2 = temp_idx[2]
    x[[i]][idx1, idx2, , drop = TRUE]
  }))
}

# choose lon and lat positions to sim
lon_choose = seq((135-20),(135 + 21))
lat_choose = seq((54-12),(54 + 19))
lon_sim = lon[lon_choose]
lat_sim = lat[lat_choose]

# determine associated rows of idx
grid_sim = expand.grid(lon_choose, lat_choose)

idx_which = apply(grid_sim, 1, function(r) which((idx[,1] == r[1]) &
                                                 (idx[,2] == r[2])))
idx_which = unlist(idx_which)
# make sure they overlap domain (TRUE = yes)
length(idx_which) == nrow(grid_sim)

# image(lon, lat, x[[1]][,,1])
# points(expand.grid(lon_sim, lat_sim))
time_idx = (dim(x_array)[2] - 300 + 1):(dim(x_array)[2])
# subset to relevant subset
x_array_sim = x_array[idx_which, time_idx,]

# means of space/time locations by month
sim_means = apply(x_array_sim, 1:2, mean)
# sd of space/time locations by month
sim_sd = apply(x_array_sim, 1:2, sd)

if (!dir.exists("./_sim_data")) {
  dir.create("./_sim_data")
}

save(lon_sim, lat_sim, sim_means, sim_sd,
     file = file.path("./_sim_data", "sim_params.rda"),
     compress = TRUE)