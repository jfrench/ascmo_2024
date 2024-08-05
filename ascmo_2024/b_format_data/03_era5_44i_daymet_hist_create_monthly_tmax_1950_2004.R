# change this to where your era5 monthly avg tmax are placed
era5_monthly_dir = "./_era5_monthly_tmax"

# change this to where your processed data are placed
processed_data_dir = "./_processed_data"

# get Daymet lon/lat
Daymet_lon = readRDS(file.path(processed_data_dir, 
                               "lon_nacordex_44i_Daymet.rds"))
Daymet_lat = readRDS(file.path(processed_data_dir,
                               "lat_nacordex_44i_Daymet.rds"))
Daymet_mask= readRDS(file.path(processed_data_dir,
                               "mask_nacordex_44i_Daymet.rds"))

# get era5 lon/lat
era5_lon = readRDS(file.path(era5_monthly_dir, "lon_ERA5_tmax.rds"))
era5_lat = readRDS(file.path(era5_monthly_dir, "lat_ERA5_tmax.rds"))

# determine era5 lon and lat indices that match
# the Daymet lon and lat
lon_idx = sapply(Daymet_lon, function(xpos) which(era5_lon == xpos))
lat_idx = sapply(Daymet_lat, function(ypos) which(era5_lat == ypos))

# verify accuracy
all.equal(era5_lon[lon_idx], Daymet_lon)
all.equal(era5_lat[lat_idx], Daymet_lat)

# relevant years
years = 1950:2004
years_text = as.character(years)
# relevant months
months = 1:12
months_text = formatC(months, width = 2, flag = "0")

# array to store data
x = array(dim = c(length(Daymet_lon), length(Daymet_lat), length(years) * length(months)))
counter = 1
for (i in seq_along(years_text)) {
  for (j in seq_along(months_text)) {
    # determine target to read
    target = paste0("era5_tmax_", years_text[i], "_", months_text[j], ".rds")
    # read data
    temp_x = readRDS(file = file.path(era5_monthly_dir, target))
    # apply mask
    temp_x = temp_x[lon_idx, lat_idx] * Daymet_mask
    # save masked data
    x[,, counter] = temp_x
    # increment counter
    counter = counter + 1
    # print progress
    message("completed: year ", years_text[i], ", month ", months_text[j])
  }
}

# save masked data
saveRDS(x,
        file = file.path(processed_data_dir,
                         "era5_monthly_tmax_nacordex_44i_Daymet.rds"),
        compress = "xz")
