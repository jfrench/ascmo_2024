era5_hourly_t2m_to_monthly_tmax.R will download daily t2m data from
Copernicus and then convert it to the monthly average maximum t2m (tmax)
and save it in the directory path ./_era5_monthly_tmax.
Note that users will have to provide their own access to the Copernicus 
Climate Store 
(https://cds.climate.copernicus.eu/#!/search?text=ERA5&type=dataset)
to download files through the *ecmwfr* R package.

run bash curl-download-cordex.sh to download the bias corrected monthly
average maximum t2m (tmax). These files must be placed in the directory path
./_nacordex_monthly_tmax_nc





