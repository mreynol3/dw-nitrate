# first intro code
# doing some data analysis and stuff ya know

library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
library(readr)
#install.packages('tidygeocoder')
library(tidygeocoder)
install.packages("cdlTools")
library(cdlTools)

setwd("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/dw-nitrate")

# RET data and wells ===========================================================

# wells <- read_delim("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/dw-nitrate/well_logs.txt")
# 
# welp <- wells |>
#   drop_na(latitude, longitude) |>
#   drop_na(use_domestic)
# 
# welp <- welp |>
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4269) |>
#   separate(col = complete_date,
#            into = c("Month","Date","Year"),
#            sep = "/",
#            remove = TRUE)
# #  st_transform(crs = 5072)
# 
# welp$Year <- as.numeric(welp$Year)
# 
# welp_2020 <- welp |>
#   filter(Year =='2020')

# welp <- welp %>%
#   mutate(era = factor(case_when(
#     Year < 1900 ~ 'old',
#     Year >= 1900 & Year < 1910 ~ '1900s',
#     Year >= 1910 & Year < 1920 ~ '1910s',
#     Year >= 1920 & Year < 1930 ~ '1920s',
#     Year >= 1930 & Year < 1940 ~ '1930s',
#     Year >= 1940 & Year < 1950 ~ '1940s',
#     Year >= 1950 & Year < 1960 ~ '1950s',
#     Year >= 1960 & Year < 1970 ~ '1960s',
#     Year >= 1970 & Year < 1980 ~ '1970s',
#     Year >= 1980 & Year < 1990 ~ '1980s',
#     Year >= 1990 & Year < 2000 ~ '1990s',
#     Year >= 2000 & Year < 2010 ~ '2000s',
#     Year >= 2010 & Year < 2020 ~ '2010s',
#     Year >= 2020 ~ '2020s',
#   )))

# ggplot(welp, aes(color = era)) +
#   geom_sf(size = 0.9)
# 
# hist(welp$Year, main = paste("Well Data Per Year"),
#      xlab = "Year")

# RET data

RET <- read_csv("RET-excel.csv")

RET <- RET |>
  drop_na(WellLongitude, WellLatitude) |>
  filter((WellLongitude < (-100))  & (WellLatitude > 41.9)) |>
  st_as_sf(coords = c("WellLongitude", "WellLatitude"), crs = 4269) 

# RET_summary <- as.data.frame(table(RET$Year))
# 
# RET_summary <- RET_summary |>
#   rename(year = Var1,
#          RET_total = Freq)

RET_county <- RET |>
  group_by(WellCounty, Year) |>
  summarize(count = n())

RET_county <- RET_county |>
  st_drop_geometry() |>
  pivot_wider(id_cols = WellCounty,
              names_from = Year,
              values_from = count,
              values_fill = 0)

RET_county <- RET_county |>
  rename_with(~ paste0("RET_", .), -WellCounty)
  

# ggplot(RET, aes(color = WellCounty)) +
#   geom_sf(size = 1)

# pending_all <- read.csv("PENDLISOR.csv") # data : https://fred.stlouisfed.org/series/PENLISCOUOR
# 
# pending_yr <- pending_all |>
#   separate(col = observation_date,
#            into = c("year","month","date"),
#            sep = "-",
#            remove = TRUE) |>
#   rename(pending = PENLISCOUOR)
# 
# pend_summary <- pending_yr |>
#   group_by(year) |>
#   summarize(pending_total = sum(pending))
# 
# comp_all <- RET_summary |>
#   left_join(pend_summary, by = 'year') |>
#   mutate(estim_sold = pending_total * 0.95, # source : https://www.homelight.com/blog/how-often-do-pending-offers-fall-through/#:~:text=According%20to%20data%20compiled%20by,sales%20fell%20through%20in%202023.
#          perc = round(RET_total / estim_sold, digits = 4))

# county level pending data, should be 36 counties in Oregon

rdc_county <- read_csv('RDC_Inventory_Core_Metrics_County_History.csv')

rdc_county <- rdc_county |>
  separate(col = county_name,
           into = c("county", "state"),
           sep = ", ",
           remove = TRUE) |>
  filter(state == 'or')

# oregon county fips to names 

fips_to_name <- read_csv('fips_to_name.csv')

fips_to_name <- fips_to_name |>
  filter(`FIPS State and County Codes` > 40000) |>
  separate(col = `Geographic area name`,
           into = c("county", "name"),
           sep = " County",
           remove = TRUE) |>
  rename(county_fips = `FIPS State and County Codes`) |>
  select(c(county_fips, county))

# oregon domestic well data, Andrew Murray

domes_well <- read.csv('Block_Groups.csv')

# select county identifier from GEOID / block group
domes_well <- domes_well |>
  mutate(county_fips = str_sub(as.character(GEOID), 1 , 5),
         wells_2020_num = as.numeric(Wells.2020),
         wells_1990_num = as.numeric(X1990.Wells)) |>
  relocate(county_fips, wells_2020_num, wells_1990_num) |>
  drop_na(wells_2020_num, wells_1990_num) 
  

# summarize to determine number of wells in county
well_county <- domes_well |>
  group_by(county_fips) |>
  summarise(AM_wells.2020 = sum(wells_2020_num),
            AM_wells.1990 = sum(wells_1990_num)) 

rdc_mini <- rdc_county |>
  select(month_date_yyyymm, county_fips, county, 
         active_listing_count, pending_listing_count, total_listing_count,
         quality_flag) |>
  mutate(year = str_sub(as.character(month_date_yyyymm), 1 , 4))|>
  relocate(year) |>
  select(-month_date_yyyymm)

rdc_mini <- rdc_mini |>
  group_by(county_fips, year) |>
  summarise(active_listing = sum(active_listing_count),
            pending_listing = sum(pending_listing_count),
            total_listing = sum(total_listing_count)) |>
  pivot_wider(id_cols = county_fips,
              names_from = year,
              values_from = c(active_listing, pending_listing, total_listing),
              values_fill = 0)

rdc_mini <- rdc_mini |>
  left_join(well_county, by = 'county_fips')

# join to fips to name

rdc_mini <- fips_to_name |>
  mutate(county_fips = as.character(county_fips)) |>
  left_join(rdc_mini, by = 'county_fips')

# add ret data

RET_county <- RET_county |>
  rename(county = WellCounty)

county_all <- RET_county |>
  left_join(rdc_mini, by = 'county') |>
  relocate(county_fips)

county_all <- county_all |>
  drop_na(county)

# cyanotoxin transport =========================================================

# pws_inv <- read.csv('O:/Public/Pennino/HABs/PWS_inventory_gwsw_2023Q4.csv')
# pws_gw <- pws_inv |>
#   filter(str_detect(Primary.Source, regex("ground", ignore_case = TRUE)))

ucmr4 <- read.csv('O:/Public/Pennino/HABs/ucmr_4_main.csv')
ucmr_GU <- ucmr4 |>
  filter(facility_water_type == 'GU') |>
  #filter(contaminant == 'total microcystin' | contaminant == 'anatoxin-a' | contaminant == 'cylindrospermopsin') |>
  rename(PWSID = pws_id)
# ucrm_gw data frame represents GWUDI systems that have cyanotoxin presence detected
# must combined with some sort of geographic information 

# geographic information
# PWS_FAC_to_HUC12.csv: has duplicates due to facilities for a single PWS being in multiple HUC12s
pws_huc <- read_csv("PWS_FAC_Locations_to_HUC12_correct.csv", col_types = cols(HUC12 = col_character()))

# check how many overlap 
length(intersect(pws_huc$PWSID, ucmr_cyano$PWSID))

# join
geo_cyanotox <- inner_join(pws_huc, ucmr_cyano, by = 'PWSID', relationship = 'many-to-many')
# add geospatial
nhd <- st_read("O:/LAB/COR/Geospatial_Library_Resource/Physical/HYDROLOGY/WBD/WBD_National_GDB.gdb", layer = 'WBDHU12')

#check overlap 
length(intersect(nhd$huc12, geo_cyanotox$HUC12))

nhd <- nhd |>
  rename(HUC12 = huc12)

# geo_all == ucmr data filtered to cyanotoxins and GWUDI then joined with HUC12 geospatial data using Micheal's
# conversion methods and NHD huc info. Represents 94 PWS systems 
geo_all <- nhd |>
  inner_join(geo_cyanotox, by = 'HUC12', relationship = 'many-to-many')

states <- tigris::states(cb = TRUE, progress_bar = FALSE)  %>%
  filter(!STUSPS %in% c('HI', 'PR', 'AK', 'MP', 'GU', 'AS', 'VI'))  %>%
  st_transform(crs = 5072)

geo_all <- geo_all |>
  st_transform(crs = 5072)

geo_all <- geo_all |>
  st_point_on_surface()

ggplot() +
  geom_sf(data = geo_all, fill = NA) + 
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
  geom_sf(data = geo_combo)
  
ggplot(geo_combo, aes(color = pred_cyano_fit)) +
  geom_sf()

geo_combo <- st_filter(PredData, geo_all, .predicate = st_within)
is_within <- as.vector(geo_combo)

length(PredData[is_within, ])

# idaho deq data
 
idaho <- read_csv('EDMS_2025.csv')
sw_deq <- st_read('IDEQ_SWA_DELINEATIONS')

# wisconsin shallow gw database

wisc <- st_read("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/dw-nitrate/WISC_gwdb/Private Well - PFAS Shallow Groundwater Study Results.gdb", layer = 'PW_PFAS_Study_Sample_Data')

# # cyanotoxin contam
# 
# source <- read.csv('gwudi_sw.csv')
# 
# source_geo <- source_all |>
#   geocode(address, method = 'osm', lat = latitude, long = longitude)
# 
# source_geo <- source_geo |>
#   drop_na(latitude, longitude) |>
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4269) 
#   
# ggplot(source_geo, aes(color = Primary.Source)) +
#   geom_sf(size = 1)
# 
# 
# welp_fil <- welp |>
#   drop_na(street, city, zip, state)
# 
# welp_geo <- welp_fil |>
#   unite(col = 'address', street, city, state, zip, sep = ', ')
# 
# welp_geo <- welp_geo |>
#   geocode(address, method = 'osm', lat = latitude, long = longitude)

# Its happening ================================================================

# If I want to compile all these PWS types I might have to pull from 
# individual states that have the information :( 

# Loading and cleaning data

water_watch <- read_csv('state_data/waterwatch-gwudi.csv')
water_watch <- water_watch |>
  select(-starts_with("...")) |>
  rename(PWSID = `Water System No.`,
         County = `Principal County Served`,
         Source = `Primary Source Water Type`,
         PWS_name = `Water System Name`)

wv <- readxl::read_excel('state_data/WV-gwudi.xlsx')
wv <- wv |>
  rename(PWSID = `Water System ID`,
         County = `Principal County/Parish`,
         Source = `Primary Water Source Type`,
         Type = `Water System Type`,
         PWS_name = `Water System Name`) |>
  select(PWSID, PWS_name, Type, Status, County, Source)

or <- read_csv('state_data/OR-gwudi.csv')
or <- or |>
  rename(PWSID = `PWS ID`,
         PWS_name = `PWS Name\n\t\t\t\t\n`,
         Type = `System Type`,
         County = `County Served`,
         Source = `Primary Source`) |>
  select(PWSID, PWS_name, Type, Status, County, Source) |>
  mutate(PWSID = paste0("OR", PWSID))

va <- readxl::read_excel('state_data/VA-gwudi.xlsx')
va <- va |>
  rename(PWSID = `Water System ID`,
         PWS_name = `Water System Name`,
         Type = `Water System Type`,
         County = `Principal County/Parish`,
         Source = `Primary Water Source Type`) |>
  select(PWSID, PWS_name, Type, Status, County, Source)
  
oh <- readxl::read_excel('state_data/OH-gwudi.xlsx')
oh <- oh |>
  rename(PWSID = `Water System ID`,
         PWS_name = `Water System Name`,
         Type = `Water System Type`,
         County = `Principal County/Parish`,
         Source = `Primary Water Source Type`) |>
  select(PWSID, PWS_name, Type, Status, County, Source)
  
ms <- readxl::read_excel('state_data/MS-gwudi.xlsx')
ms <- ms |>
  rename(PWSID = `Water System ID`,
         PWS_name = `Water System Name`,
         Type = `Water System Type`,
         County = `Principal County/Parish`,
         Source = `Primary Water Source Type`) |>
  select(PWSID, PWS_name, Type, Status, County, Source)


state_in <- readxl::read_excel('state_data/IN-gwudi.xlsx')
state_in <- state_in |>
  rename(PWSID = `Water System ID`,
         PWS_name = `Water System Name`,
         Type = `Water System Type`,
         County = `Principal County/Parish`,
         Source = `Primary Water Source Type`) |>
  select(PWSID, PWS_name, Type, Status, County, Source)

CO <- read_csv('state_data/CO-gwudi.csv')
CO <- CO |>
  rename(PWSID = `PWS ID (Links to Records)`,
         PWS_name = Name,
         Type = `Federal Type`,
         Source = `State Source Type`) |>
  select(PWSID, PWS_name, Type, Status, County, Source)

CA <- read_csv('state_data/CA-gwudi.csv')
CA <- CA |>
  rename(PWSID = `Water System No.`,
         PWS_name = `Water System Name`,
         Source = `Primary Source Water Type`,
         County = `Principal County Served`) |>
  select(PWSID, PWS_name, Type, County, Source)

gwudi_all <- water_watch |>
  bind_rows(wv, or, va, oh, ms, state_in, CO, CA)

umcr_GU_sample <- ucmr_GU |>
  rename(PWS_name = pws_name,
         Source = facility_water_type) |>
  select(PWSID, PWS_name, Source) |>
  distinct()

gwudi_all <- gwudi_all |>
  bind_rows(umcr_GU_sample)

# check how many overlap 
length(intersect(pws_huc$PWSID, gwudi_all$PWSID))

# join
geo_gwudi <- inner_join(pws_huc, gwudi_all, by = 'PWSID', relationship = "many-to-many")

# geo_all == ucmr data filtered to cyanotoxins and GWUDI then joined with HUC12 geospatial data using Micheal's
# conversion methods and NHD huc info. Represents 94 PWS systems 
geo_gwudi <- nhd |>
  inner_join(geo_gwudi, by = 'HUC12', relationship = 'many-to-many')

ggplot() +
  geom_sf(data = geo_gwudi, fill = NA) + 
  geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) 



