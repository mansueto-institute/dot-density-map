library(sf)
library(dplyr)
library(ggplot2)
library(purrr)
library(tigris)
library(tidycensus)
library(stringr)
library(readxl)
library(viridis)
library(scales)

# Working directory
wd_dev <- '/Users/nm/Desktop/projects/work/mansueto/census'
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
if (!file.exists(paste0(wd_dev,'/us_blocks.geojson'))) {
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
} else {us_blocks <- st_read(paste0(wd_dev,'/','us_blocks.geojson'))}

## Clean up columns, select relevant features, convert FIPS to character, pad FIPS with leading zeroes, filter to states+DC
us_blocks <- us_blocks %>% 
  rename_all(tolower) %>%
  select(geoid, statefp, countyfp, aland, awater) %>%
  mutate_at(vars(geoid, statefp),list(as.character)) %>%
  mutate(geoid = str_pad(geoid, width=12, side="left", pad="0"),
         statefp = str_pad(statefp, width=2, side="left", pad="0"),
         awater_pct = awater/(aland+awater)) %>%
  filter(statefp %in% state_fips) %>% 
  st_as_sf()

# Download ACS data from Census ---------------------------------------

# Read in Census Planning Database at the Tract Level with ACS 5-year estimates for 2018
data_url <- 'https://www2.census.gov/adrm/PDB/2020/pdb2020bgv2_us.zip'
download.file(url = paste0(data_url), destfile = paste(tempdir(), basename(data_url), sep = "/")) 
pdb <- read.csv(unzip(zipfile = paste(tempdir(), basename(data_url), sep = "/")))

# Select Race features for Chicago
names(pdb)
pdb <- pdb %>% 
  select(GIDBG,State,State_name,County,County_name,Tract,Block_group,
         NH_White_alone_ACS_13_17,
         NH_Blk_alone_ACS_13_17,
         NH_AIAN_alone_ACS_13_17,
         NH_Asian_alone_ACS_13_17,
         Hispanic_ACS_13_17) %>%
  filter(State == 17 & County == 031) %>%
  mutate_at(vars(GIDBG),funs(as.character))

# Join Census data and Shapefile
pdb_il <- left_join(pdb, us_blocks, by = c('GIDBG'='geoid') )

# Generate dots according to number of people in each race category for each block
pdb_il2 <- pdb_il[1:5, ] # limit to only 5 records
p1 = st_sample(x=pdb_il2$geometry, size = pdb_il2$NH_White_alone_ACS_13_17, type= 'random' )
p2 = st_sample(x=pdb_il2$geometry, size = pdb_il2$NH_Blk_alone_ACS_13_17, type= 'random' )
p3 = st_sample(x=pdb_il2$geometry, size = pdb_il2$Hispanic_ACS_13_17, type= 'random' )

# Visualize
ggplot(data = pdb_il2 %>% st_as_sf()) +
  geom_sf(fill='white') +
  geom_sf(data = p1, size = .01, color = 'red', alpha = .01) +
  geom_sf(data = p2, size = .01, color = 'blue', alpha = .01) +
  geom_sf(data = p2, size = .01, color = 'green', alpha = .01) + 
  theme_bw()
