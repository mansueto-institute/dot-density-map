library(sf)
library(dplyr)
library(ggplot2)

# Race, poverty, education, health insurance
# NH_White_alone_ACS_13_17 White alone, not Hispanic or Latino population in the ACS
# NH_Blk_alone_ACS_13_17 Black or African American alone, not Hispanic or Latino population in the ACS
# NH_AIAN_alone_ACS_13_17 American Indian and Alaska Native alone, not Hispanic or Latino population in the ACS
# NH_Asian_alone_ACS_13_17 Asian alone, not Hispanic or Latino population in the ACS
# Hispanic_ACS_13_17 Persons of Hispanic Origin in the ACS 

# College_ACS_13_17 Persons 25+ years and over with bachelors degree or higher in the ACS
# Not_HS_Grad_ACS_13_17 Number of people 25 years old and over who are not high school graduates (did receive diploma or equivalent) in the ACS

# Prs_Blw_Pov_Lev_ACS_13_17 Number of people classified as below the poverty level in the ACS
# One_Health_Ins_ACS_13_17 Number of people with one type of health insurance coverage in the ACS
# No_Health_Ins_ACS_13_17 Number of people with no health insurance coverage in the ACS

# https://www.census.gov/topics/research/guidance/planning-databases/2019.html

# Read in Census Data
pdb <- read.csv('/Users/nm/Desktop/pdb2019bgv6_us.csv')

#https://www2.census.gov/geo/tiger/TIGER2019/BG/

# Read in Block Group Shapefiles
il_shp <- read_sf('/Users/nm/Desktop/tl_2019_17_bg/tl_2019_17_bg.shp')

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
pdb_il <- left_join(pdb, il_shp, by = c('GIDBG'='GEOID') )

# Generate dots according to number of people in each race category for each block
pdb_il2 <- pdb_il[1:5, ] 
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
