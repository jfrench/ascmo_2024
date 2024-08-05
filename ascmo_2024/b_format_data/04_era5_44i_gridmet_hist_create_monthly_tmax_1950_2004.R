# change this to where your era5 monthly avg tmax are placed
era5_monthly_dir = "./_era5_monthly_tmax"

# change this to where your processed data are placed
processed_data_dir = "./_processed_data"

# get gridMET lon/lat
gridMET_lon = readRDS(file.path(processed_data_dir,
                                "lon_nacordex_44i_gridMET.rds"))
gridMET_lat = readRDS(file.path(processed_data_dir,
                                "lat_nacordex_44i_gridMET.rds"))
gridMET_mask = readRDS(file.path(processed_data_dir,
                                 "mask_nacordex_44i_gridMET.rds"))

# get era5 lon/lat
era5_lon = readRDS(file.path(era5_monthly_dir, "lon_ERA5_tmax.rds"))
era5_lat = readRDS(file.path(era5_monthly_dir, "lat_ERA5_tmax.rds"))

# determine era5 lon and lat indices that match
# the gridMET lon and lat
lon_idx = sapply(gridMET_lon, function(xpos) which(era5_lon == xpos))
lat_idx = sapply(gridMET_lat, function(ypos) which(era5_lat == ypos))

# verify accuracy
all.equal(era5_lon[lon_idx], gridMET_lon)
all.equal(era5_lat[lat_idx], gridMET_lat)

# # function to crease a mask (na or 1, depending on whether data are available)
# create_mask = function(x) {
#   mx = max(x)
#   if (!is.na(mx)) {
#     mx = 1
#   }
#   return(mx)
# }
# gridMET_mask = apply(gridMET_tmax, 1:2, create_mask)

# # check locations
# image(gridMET_mask)

# relevant years
years = 1950:2004
years_text = as.character(years)
# relevant months
months = 1:12
months_text = formatC(months, width = 2, flag = "0")

# array to store data
x = array(dim = c(length(gridMET_lon), length(gridMET_lat), length(years) * length(months)))
counter = 1
for (i in seq_along(years_text)) {
  for (j in seq_along(months_text)) {
    # determine target to read
    target = paste0("era5_tmax_", years_text[i], "_", months_text[j], ".rds")
    # read data
    temp_x = readRDS(file.path(era5_monthly_dir, target))
    # apply mask
    temp_x = temp_x[lon_idx, lat_idx] * gridMET_mask
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
                         "era5_monthly_tmax_nacordex_44i_gridMET.rds"),
        compress = "xz")
