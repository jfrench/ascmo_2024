library(ncdf4)

if (!dir.exists("./_processed_data")) {
  dir.create("./_processed_data")
}

# change this to where your nc files are located ()
nacordex44i_nc_data_dir = "./_nacordex_monthly_tmax_nc"
# change this to where you want your processed data to be placed
processed_data_dir = "./_processed_data"

# list of all Daymet nacordex 44i files
all_files = c(
  "tmax.hist.CanESM2.CanRCM4.mon.NAM-44i.mbcn-Daymet.nc",
  "tmax.hist.CanESM2.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.nc",
  "tmax.hist.CanESM2.RCA4.mon.NAM-44i.mbcn-Daymet.nc",
  "tmax.hist.EC-EARTH.HIRHAM5.mon.NAM-44i.mbcn-Daymet.nc",
  "tmax.hist.EC-EARTH.RCA4.mon.NAM-44i.mbcn-Daymet.nc",
  "tmax.hist.GEMatm-Can.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.nc",
  "tmax.hist.GEMatm-MPI.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.nc",
  "tmax.hist.GFDL-ESM2M.RegCM4.mon.NAM-44i.mbcn-Daymet.nc",
  "tmax.hist.GFDL-ESM2M.WRF.mon.NAM-44i.mbcn-Daymet.nc",
  "tmax.hist.HadGEM2-ES.RegCM4.mon.NAM-44i.mbcn-Daymet.nc",
  "tmax.hist.HadGEM2-ES.WRF.mon.NAM-44i.mbcn-Daymet.nc",
  "tmax.hist.MPI-ESM-LR.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.nc",
  "tmax.hist.MPI-ESM-LR.RegCM4.mon.NAM-44i.mbcn-Daymet.nc",
  "tmax.hist.MPI-ESM-LR.WRF.mon.NAM-44i.mbcn-Daymet.nc",
  "tmax.hist.MPI-ESM-MR.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.nc"
)

# create indices for each year
idxs = as.data.frame(matrix(seq_len(length(1950:2004) * 12), nrow = 12))
colnames(idxs) = 1950:2004

years = 1950:2004
years_text = as.character(years)

# function to crease a mask (na or 1, depending on whether data are available)
create_mask = function(x) {
  mx = max(x)
  if (!is.na(mx)) {
    mx = 1
  }
  return(mx)
}

Daymet_mask = NULL

# for each data file
for (i in seq_along(all_files)) {
  # open the file
  nco = nc_open(file.path(nacordex44i_nc_data_dir, all_files[i]))
  # print progress
  message(paste(i, "/", length(all_files), ": ", all_files[i], sep = ""))
  # read in longitude, latitude, and tmax
  lon = ncvar_get(nco, varid = "lon")
  lat = ncvar_get(nco, varid = "lat")
  tmax = ncvar_get(nco, varid = "tmax")
  # create file name for saving
  file_short = substr(all_files[i], start = 1, stop = nchar(all_files[i]) - 3)
  file_name = paste0(file_short, ".rds")
  # set the processed data directory
  # only save the relevant columns of tmax into a new file
  saveRDS(tmax[,,unlist(idxs[,years_text[seq_along(years)]]), drop = FALSE],
          file = file.path(processed_data_dir, file_name),
          compress = "xz")

  # compute mask over all_files
  # only non-NA if all data sets have that data
  if (is.null(Daymet_mask)) {
    Daymet_mask = apply(tmax[,,unlist(idxs[,years_text[seq_along(years)]]), drop = FALSE], 1:2, create_mask)
  } else {
    Daymet_mask = Daymet_mask * apply(tmax[,,unlist(idxs[,years_text[seq_along(years)]]), drop = FALSE], 1:2, create_mask)
  }
  nc_close(nco)
}

saveRDS(lon, file = file.path(processed_data_dir, "lon_nacordex_44i_Daymet.rds"))
saveRDS(lat, file = file.path(processed_data_dir, "lat_nacordex_44i_Daymet.rds"))
saveRDS(Daymet_mask, file.path(processed_data_dir, file = "mask_nacordex_44i_Daymet.rds"))