set.seed(55)

# change this to where your processed data are placed
processed_data_dir = "./_processed_data"

# number of desired time steps
desired_length = 660

# list of all nacordex 44i files
m_files = c(
  "tmax.hist.CanESM2.RCA4.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.EC-EARTH.RCA4.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.CanESM2.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.GEMatm-Can.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.GEMatm-MPI.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.MPI-ESM-LR.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.MPI-ESM-MR.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.CanESM2.CanRCM4.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.EC-EARTH.HIRHAM5.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.GFDL-ESM2M.RegCM4.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.HadGEM2-ES.RegCM4.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.GFDL-ESM2M.WRF.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.HadGEM2-ES.WRF.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.MPI-ESM-LR.WRF.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.MPI-ESM-LR.RegCM4.mon.NAM-44i.mbcn-Daymet.rds"
)

r_files = c(
  "era5_monthly_tmax_nacordex_44i_Daymet.rds"
)

source("fcmc_functions.R")
source("fcmc_functions_v2.R")

# create list of spatio-temporal arrays from file
x = prepare_x(m_files, r_files, desired_length, processed_data_dir)
# create grouping variable
grp = factor(rep(c("m", "r"), times = c(length(m_files), length(r_files))))
# create stratifying variable
strata = factor(rep(1950:2004, each = 12))
# read lon and lat coordinates for Daymet data
lon = readRDS(file = file.path(processed_data_dir,
                               "lon_nacordex_44i_Daymet.rds"))
lat = readRDS(file = file.path(processed_data_dir,
                               "lat_nacordex_44i_Daymet.rds"))
# read na information
namat = readRDS(file = file.path(processed_data_dir,
                                 "mask_nacordex_44i_Daymet.rds"))
# create idx information
idx = which(!is.na(namat), arr.ind = TRUE)
# create sta object from x
xsta = prepare_str_starray(x, grp, strata, idx)
# create perm_idxs
perm_idxs = prepare_perm_idxs(length(x), length(r_files))

save(grp, strata, lon, lat, idx, xsta, perm_idxs,
     file = file.path(processed_data_dir, "Daymet_data.rda"),
     compress = "xz")
