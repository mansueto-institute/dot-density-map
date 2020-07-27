library(sf)
library(dplyr)
library(purrr)
library(tigris)
library(tidycensus)
library(stringr)

# Working directory
wd_dev <- '/project2/bettencourt/dot-map'
# Obtain Census API Key here: https://api.census.gov/data/key_signup.html
# census_api_key('API KEY', install = TRUE) 
readRenviron("~/.Renviron")

state_xwalk <- as.data.frame(fips_codes) %>%
  rename(state_fips = state_code,
         state_codes = state,
         county_name = county) %>%
  mutate(county_fips = paste0(state_fips,county_code))
state_fips <- unique(state_xwalk$state_fips)[1:51]
state_codes <- unique(state_xwalk$state_codes)[1:51]

# Prepare geographic shapefiles and crosswalks -------------------------------------------

# Unzip and download Census Block shapefiles for each state
filedir <- paste0(tempdir(), '/blocks/')
unlink(filedir, recursive = TRUE)
dir.create(filedir)
for (s in state_fips) {
  state_shp <- paste0('https://www2.census.gov/geo/tiger/TIGER2019/BG/tl_2019_',s,'_bg.zip')
  download.file(url = state_shp, destfile = paste0(filedir, basename(state_shp)))
  unzip(paste0(filedir,basename(state_shp)), exdir= filedir)
}
list.files(path = filedir)
us_blocks <- st_read(fs::dir_ls(filedir, regexp = "\\.shp$")[1])
for (f in fs::dir_ls(filedir, regexp = "\\.shp$")[-1] ) {
  state_sf <- st_read(f)
  us_blocks <- rbind(us_blocks, state_sf)
}
st_write(us_blocks, paste0(wd_dev,'/','us_blocks.geojson'))
