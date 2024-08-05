library(ecmwfr)
library(ncdf4)

# create download directory if needed
if (!dir.exists("./_era5_monthly_tmax")) {
  dir.create("./_era5_monthly_tmax")
}

# path for file downloads
# change path to download to a different location
path = "./_era5_monthly_tmax"

# get access token
u = "39020"
mykey = "c26cc063-461c-4710-a400-9eb92bbc1a0b"
options(keyring_backend ="file")
wf_set_key(user = u,
           key = mykey,
           service = "cds")

request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = "2m_temperature",
  year = "1950",
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  dataset_short_name = "reanalysis-era5-single-levels",
  area = "12.25/-171.75/76.25/-22.25"
)

years = 1950:2004
years_text = as.character(years)
months = 1:12
months_text = formatC(months, width = 2, flag = "0")

tmax_celsius = function(x) {
  ndays = length(x)/24
  f = rep(seq_len(ndays), each = 24)
  mean(tapply(x, INDEX = f, FUN = max)) - 273.15
}

for (i in seq_along(years_text)) {
  request$year = years_text[i]
  for (j in seq_along(months_text)) {
    request$month = months_text[j]
    # save ERA5 daily t2m nc
    target = paste0("era5_t2m_", years_text[i], "_", months_text[j], ".nc")
    # save ERA5 average monthly tmax
    target_tmax = paste0("era5_tmax_", years_text[i], "_", months_text[j], ".rds")
    request$target = target
    if (!file.exists(file.path(path, target_tmax))) {
      message("request: ", target)
      invisible(capture.output(
        wf_request(user = u, request = request,
                   transfer = TRUE, path = path,
                   verbose = TRUE)))

      nco = nc_open(file.path(path, target))
      if (!file.exists(file.path(path, "lon_ERA5_tmax.rds"))) {
        lon = ncvar_get(nco, varid = "longitude")
        saveRDS(lon, file = file.path(path, "lon_ERA5_tmax.rds"))
      }
      if (!file.exists(file.path(path, "lat_ERA5_tmax.rds"))) {
        lat = ncvar_get(nco, varid = "latitude")
        saveRDS(lat, file = file.path(path, "lat_ERA5_tmax.rds"))
      }
      # get 2-meter temperature
      t2m = ncvar_get(nco, varid = "t2m")
      # determine daily maximum
      tmax = apply(t2m, 1:2, tmax_celsius)
      nc_close(nco)
      # save average daily maximum
      saveRDS(tmax,
              file = file.path(path, target_tmax),
              compress = "xz")
      # remove hourly file
      file.remove(file.path(path, target))
      message("completed: year ", years_text[i], ", month ", months_text[j])
    }
  }
}
