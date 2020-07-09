
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

# Prepare geographic shapefiles and crosswalks -------------------------------------------

# Load state codes and FIPS from tidycensus
state_xwalk <- as.data.frame(fips_codes) %>%
  rename(state_fips = state_code,
         state_codes = state,
         county_name = county) %>%
  mutate(county_fips = paste0(state_fips,county_code))
state_fips <- unique(state_xwalk$state_fips)[1:51]
state_codes <- unique(state_xwalk$state_codes)[1:51]

# Unzip and download county-metro crosswalk from excel file on Census website, clean up columns
xwalk_url <- 'https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list1_2020.xls'
tmp_filepath <- paste0(tempdir(), '/', basename(xwalk_url))
download.file(url = paste0(xwalk_url), destfile = tmp_filepath)
unzip(tmp_filepath, exdir=tempdir())
cbsa_xwalk <- read_excel(tmp_filepath, sheet = 1, range = cell_rows(3:1919))
cbsa_xwalk <- cbsa_xwalk %>% 
  select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>%
  rename_all(list(tolower)) %>%
  mutate(county_fips = paste0(fips_state_code,fips_county_code)) %>%
  rename(cbsa_fips = cbsa_code,
         area_type = metropolitan_micropolitan_statistical_area) %>%
  select(county_fips,cbsa_fips,cbsa_title,area_type,central_outlying_county) 
  
# Use tidycensus to download county shapefiles and population estimates for ACS 5-year 2018
us_county <- get_acs(year = 2018, geography = "county", variables = "B01003_001", geometry = TRUE, keep_geo_vars = TRUE, shift_geo = TRUE)
us_county <- us_county %>%
  rename_all(list(tolower)) %>%
  rename(county_fips = geoid,
         county_population = estimate) %>%
  select(county_fips,name,variable,county_population)
# Visually inspect the data
ggplot(us_county, aes(fill = log(county_population), color = log(county_population))) +
  geom_sf() + scale_fill_viridis() + scale_color_viridis() 

# Join state xwalk and CBSA xwalk to county population shapefile
full_xwalk <- us_county %>% 
  left_join(., cbsa_xwalk, by = c('county_fips'='county_fips') ) %>%
  left_join(., state_xwalk, by = c('county_fips'='county_fips') ) %>%
  mutate(area_type = case_when(is.na(area_type) ~ 'Rural',
                               area_type == 'Metropolitan Statistical Area' ~ 'Metro',
                               area_type == 'Micropolitan Statistical Area' ~ 'Micro'),
         central_outlying_county = ifelse(is.na(central_outlying_county), 'Rural', central_outlying_county))
                             
# Unzip and download CBSA shapefile (metros and micros)
cbsa_url <- 'https://www2.census.gov/geo/tiger/TIGER2019/CBSA/tl_2019_us_cbsa.zip'
tmp_filepath <- paste0(tempdir(), '/', basename(cbsa_url))
download.file(url = paste0(cbsa_url), destfile = tmp_filepath)
unzip(tmp_filepath, exdir=tempdir())
us_cbsa <- sf::st_read(gsub(".zip", ".shp", tmp_filepath))

# Unzip and download Census Tract shapefiles for each state
if (!file.exists(paste0(wd_dev,'/us_tracts.geojson'))) {
  filedir <- paste0(tempdir(), '/tracts/')
  unlink(filedir, recursive = TRUE)
  dir.create(filedir)
  for (s in state_fips) {
    state_shp <- paste0('https://www2.census.gov/geo/tiger/TIGER2019/TRACT/tl_2019_',s,'_tract.zip')
    download.file(url = state_shp, destfile = paste0(filedir, basename(state_shp)))
    unzip(paste0(filedir,basename(state_shp)), exdir= filedir)
  }
  list.files(path = filedir)
  us_tracts <- st_read(fs::dir_ls(filedir, regexp = "\\.shp$")[1])
  for (f in fs::dir_ls(filedir, regexp = "\\.shp$")[-1] ) {
    state_sf <- st_read(f)
    us_tracts <- rbind(us_tracts, state_sf)
  }
  st_write(us_tracts, paste0(wd_dev,'/','us_tracts.geojson'))
} else {us_tracts <- st_read(paste0(wd_dev,'/','us_tracts.geojson'))}

## Clean up columns, select relevant features, convert FIPS to character, pad FIPS with leading zeroes, filter to states+DC
us_tracts <- us_tracts %>% 
  rename_all(tolower) %>%
  select(geoid, statefp, countyfp, aland, awater) %>%
  mutate_at(vars(geoid, statefp),list(as.character)) %>%
  mutate(geoid = str_pad(geoid, width=11, side="left", pad="0"),
         statefp = str_pad(statefp, width=2, side="left", pad="0"),
         awater_pct = awater/(aland+awater)) %>%
  filter(statefp %in% state_fips) %>% 
  st_as_sf()

# Unzip and download Census Place shapefiles for each state
if (!file.exists(paste0(wd_dev,'/us_places.geojson'))) {
  filedir <- paste0(tempdir(), '/tracts/')
  unlink(filedir, recursive = TRUE)
  dir.create(filedir)
  for (s in state_fips) {
    state_shp <- paste0('https://www2.census.gov/geo/tiger/TIGER2019/PLACE/tl_2019_',s,'_place.zip')
    download.file(url = state_shp, destfile = paste0(filedir, basename(state_shp)))
    unzip(paste0(filedir,basename(state_shp)), exdir= filedir)
  }
  list.files(path = filedir)
  us_places <- st_read(fs::dir_ls(filedir, regexp = "\\.shp$")[1])
  for (f in fs::dir_ls(filedir, regexp = "\\.shp$")[-1] ) {
    state_sf <- st_read(f)
    us_tracts <- rbind(us_tracts, state_sf)
  }
  st_write(us_places, paste0(wd_dev,'/','us_places.geojson'))
} else {us_places <- st_read(paste0(wd_dev,'/','us_places.geojson'))}

## Clean up columns, select relevant features, convert FIPS to character, pad FIPS with leading zeroes, filter to states+DC
us_places <- us_places %>% 
  rename_all(tolower) %>%
  select(geoid, statefp, placefp, placens, name, namelsad, aland, awater) %>%
  mutate_at(vars(geoid, statefp, placefp, placens),list(as.character)) %>%
  mutate(geoid = str_pad(geoid, width=7, side="left", pad="0"),
         statefp = str_pad(statefp, width=2, side="left", pad="0")) %>%
  filter(statefp %in% state_fips) %>%
  st_as_sf()

# Download ACS data from Census ---------------------------------------

# Read in Census Planning Database at the Tract Level with ACS 5-year estimates for 2018
data_url <- 'https://www2.census.gov/adrm/PDB/2020/pdb2020trv2_us.zip'
download.file(url = paste0(data_url), destfile = paste(tempdir(), basename(data_url), sep = "/")) 
df_map <- read.csv(unzip(zipfile = paste(tempdir(), basename(data_url), sep = "/")))

## Clean up columns, select relevant features, convert FIPS to character, pad FIPS with leading zeroes, filter to states+DC
df_map <- df_map %>% 
  rename_all(tolower) %>%
  select(gidtr, state, state_name, county, county_name, tract, tot_population_acs_14_18, hispanic_acs_14_18, nh_white_alone_acs_14_18, nh_blk_alone_acs_14_18, nh_aian_alone_acs_14_18, nh_asian_alone_acs_14_18, nh_nhopi_alone_acs_14_18, nh_sor_alone_acs_14_18, pct_hispanic_acs_14_18, pct_nh_white_alone_acs_14_18, pct_nh_blk_alone_acs_14_18, pct_nh_aian_alone_acs_14_18, pct_nh_asian_alone_acs_14_18, pct_nh_nhopi_alone_acs_14_18, pct_nh_sor_alone_acs_14_18, pct_not_hs_grad_acs_14_18, pct_college_acs_14_18, pct_prs_blw_pov_lev_acs_14_18, pct_no_health_ins_acs_14_18, pct_pub_asst_inc_acs_14_18, pct_pop_nocompdevic_acs_14_18, pct_vacant_units_acs_14_18) %>%
  mutate_at(vars(gidtr, state),list(as.character)) %>%
  mutate(gidtr = str_pad(gidtr, width=11, side="left", pad="0"),
         state = str_pad(state, width=2, side="left", pad="0"),
         county = str_pad(county, width=3, side="left", pad="0"),
         county_fips = str_sub(gidtr, 1, 5)) %>%
  filter(state %in% state_fips)

## Check for duplicates
df_map %>% 
  group_by(gidtr, state, state_name, county, county_name, tract) %>% 
  mutate(dupes = n()) %>%
  filter(dupes >= 2)
## Fix duplicate issue (there are duplicates where population is NA)
df_map <- df_map %>% 
  filter(!is.na(tot_population_acs_14_18))

# Download Census data for all tracts using tidycensus API

acs5_vars <- load_variables(year = 2018, dataset = 'acs5', cache = FALSE)
acs5_vars_subject <- load_variables(year = 2018, dataset = 'acs5/subject', cache = FALSE)
acs5_vars_selected <- c('S0501','S0701','S0802','S0804','S0902','S1101','S1301','S1401','S1501','S1601','S1602','S1603','S1701','S1702','S1703','S1810','S1811','S1901','S1902','S1903','S2001','S2002','S2201','S2301','S2302','S2303','S2405','S2406','S2407','S2411','S2412','S2413','S2414','S2418','S2419','S2501','S2502','S2503','S2504','S2506','S2507','S2601A','S2701','S2702','S2703','S2704','S2801','S2802','S2902')

## Total Population by Tract
tract_population <- map_df(state_codes, function(x) {
  get_acs(year = 2018, geography = "tract", survey = 'acs5', variables = "B01003_001", state = x)
})

# Join together tract geometries and tract dataframes ---------------------

df_map_geo <- inner_join(df_map, us_tracts, by = c('gidtr'='geoid') ) %>% st_as_sf()

# Check for missing
df_map_geo %>%
  group_by(state_name, statefp) %>%
  select(state_name,statefp) %>% 
  #select(everything()) %>%  
  summarise_all(funs(sum(is.na(.)))) %>% 
  as.data.frame()
#options(tibble.print_max = Inf)

# Visualize metro-tract choropleth ---------------------------------------------

# City boundary outline
places_outline <- us_places %>% filter(geoid =='1714000') %>% 
  st_simplify(dTolerance = .005, preserveTopology = FALSE) %>%
  st_transform(crs = st_crs(4326)) %>% 
  st_as_sf()

# County boundary outline
county_outline <- full_xwalk %>% filter(cbsa_fips == '16980') %>% 
  st_transform(crs = st_crs(4326)) %>% st_as_sf()

# Metro boundary outline
metro_outline <- st_union(county_outline)

# Tract boundaries and data
chi_fips <- full_xwalk %>% filter(cbsa_fips == '16980') %>% pull(county_fips)
df_viz <- df_map_geo %>% filter(county_fips %in% chi_fips,
                                awater_pct < 1,
                                tot_population_acs_14_18 > 0) %>%
  st_transform(crs = st_crs(4326)) %>% 
  st_as_sf() %>%
  st_intersection(. , y = metro_outline)

# Create choropleth
(plot_tract_metro <- ggplot(df_viz) +
    geom_sf( aes(fill = pct_college_acs_14_18/100, color =  pct_college_acs_14_18/100), size = .1) +
    geom_sf(data = county_outline, fill = NA, color = "#f5f5f2", size= .5) +
    geom_sf(data = places_outline, fill = NA, color = "#f5f5f2", size = .5) +
    scale_fill_viridis("", option = "magma", labels = percent, expand = c(0, 0), breaks = c(0, .25, .5, .75, 1)) +
    scale_color_viridis("", option = "magma", labels = percent, expand = c(0, 0)) +
    labs(title = "Chicago Metropolitan Area",
         subtitle = "Share of persons 25+ years and over with bachelor's degree or higher",
         caption = 'Data Source: American Community Survey, 2018 (5-year estimate)') +
    coord_sf(clip = "on") +
    theme_minimal() + 
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.spacing.x=unit(0, "lines"),
          panel.spacing.y=unit(0, "lines"),
          panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#f5f5f2", color = NA),
          plot.title = element_text(face = 'bold'),
          plot.subtitle=element_text(size=9),
          plot.caption=element_text(size=9, hjust = 0, face = 'italic'),
          panel.background = element_rect(fill = "#f5f5f2", color = NA), 
          legend.background = element_rect(fill = "#f5f5f2", color = NA),
          panel.border = element_blank()))

# Save plot as PNG
ggsave(plot = last_plot(), 
       filename = paste0(wd_dev,'/msa_tract.png'), 
       dpi = 400,  device='png', height = 6, width = 5.1)


# Visualize national-county choropleth ------------------------------------

# Household income by county
county_income <- get_acs(year = 2018, geography = "county", survey = 'acs5', variables = "B19013_001", geometry = TRUE, keep_geo_vars = TRUE, shift_geo = TRUE) 

# Replace missing with smallest estimate
county_income <- county_income %>%
  mutate(estimate = tidyr::replace_na(estimate, min(estimate, na.rm = TRUE)))

ggplot(county_income) +
  geom_sf(aes(fill = estimate), color = 'white', size=.01) + 
  scale_fill_viridis("Dollars ($)", labels = comma, direction = -1) +
  labs(title = "Income in the United States",
       subtitle = "Household income in the past 12 months (in 2018 inflation-adjusted dollars)",
       caption = 'Data Source: American Community Survey, 2018 (5-year estimate)') +
  coord_sf(clip = "on") +
  theme_minimal() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.spacing.x=unit(0, "lines"),
        panel.spacing.y=unit(0, "lines"),
        panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        plot.title = element_text(face = 'bold'),
        plot.subtitle=element_text(size=9),
        plot.caption=element_text(size=9, hjust = 0, face = 'italic'),
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border = element_blank())

# Save plot as PNG
ggsave(plot = last_plot(), 
       filename = paste0(wd_dev,'/national_county.png'), 
       dpi = 400,  device='png')



