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

# RET data and wells ===========================================================

wells <- read_delim("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/dw-nitrate/well_logs.txt")

welp <- wells |>
  drop_na(latitude, longitude) |>
  drop_na(use_domestic)

welp <- welp |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269) |>
  separate(col = complete_date,
           into = c("Month","Date","Year"),
           sep = "/",
           remove = TRUE)
#  st_transform(crs = 5072)

welp$Year <- as.numeric(welp$Year)

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

ggplot(welp, aes(color = era)) +
  geom_sf(size = 0.9)

hist(welp$Year, main = paste("Well Data Per Year"),
     xlab = "Year")

# RET data

RET <- read_csv("RET-excel.csv")

RET <- RET |>
  drop_na(WellLongitude, WellLatitude) |>
  #filter(WellLongitude != 0  & WellLatitude != 0) |>
  filter((WellLongitude < (-100))  & (WellLatitude > 41.9)) |>
  st_as_sf(coords = c("WellLongitude", "WellLatitude"), crs = 4269) 


ggplot(RET, aes(color = WellCounty)) +
  geom_sf(size = 1)

joined <- RET |>
  st_drop_geometry() |>
  rename(wl_nbr = WellID) |>
  mutate(wl_nbr = as.numeric(wl_nbr)) |>
  left_join(welp, by = 'wl_nbr')

joined <- joined |>
  relocate(WellID, wl_nbr)

fred_county <- read.csv('RDC_County_History.csv')

fred_or <- fred_county |>
  separate(col = county_name,
           into = c("county","state"),
           sep = ", ",
           remove = TRUE) |>
  filter(state == 'or')



# cyanotoxin contam

source <- read.csv('gwudi_sw.csv')

source_geo <- source_all |>
  geocode(address, method = 'osm', lat = latitude, long = longitude)

source_geo <- source_geo |>
  drop_na(latitude, longitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269) 
  
ggplot(source_geo, aes(color = Primary.Source)) +
  geom_sf(size = 1)


welp_fil <- welp |>
  drop_na(street, city, zip, state)

welp_geo <- welp_fil |>
  unite(col = 'address', street, city, state, zip, sep = ', ')

welp_geo <- welp_geo |>
  geocode(address, method = 'osm', lat = latitude, long = longitude)


# # Iowa?
# 
# iowa <- read.csv('iowa-data/SITE_INFO.csv')
# 
# iowa <- iowa |>
#   st_as_sf(coords = c("DecLongVa", "DecLatVa"), crs = 4269) 
# 
# ggplot(iowa, aes(color = NatAqfrDesc)) +
#   geom_sf(size = 1)

# cyanotoxin transport =========================================================

umcr4 <- read.csv('O:/Public/Pennino/HABs/ucmr_4_main.csv')
pws_inv <- read.csv('O:/Public/Pennino/HABs/PWS_inventory_gwsw_2023Q4.csv')

GU <- umcr4 |>
  filter(facility_water_type == 'GU')

GU_pws <- pws_inv |>
  filter(Primary.Source == 'Groundwater under influence of surface water' | Primary.Source == 'Purchased ground water under influence of surface water source')

micx <- umcr4 |>
  filter(contaminant == 'total microcystin')

micx_gw <- micx |>
  filter(facility_water_type == "GU")

# GU <- GU |>
#   mutate(state_abbr = fips(state, to = "Abbreviation")) |>
#   rename(state_fips = state)
# 
pws_huc12 <- read.csv('Facilities_95correct_1HUC12.csv')

overlap_values <- intersect(pws_huc12$PWSID, micx_gw$pws_id)
print(overlap_values) #There's only 54???? rats!!

micx_gw <- micx_gw |>
  rename(PWSID = pws_id)

overlap <- inner_join(pws_huc12, micx_gw, by = 'PWSID', relationship = 'many-to-many')
nhd <- st_read("O:/LAB/COR/Geospatial_Library_Resource/Physical/HYDROLOGY/WBD/WBD_National_GDB.gdb", layer = 'WBDHU12')

overlap_values <- intersect(nhd$huc12, overlap$HUC12) # only 12????? RATS! how did that happen folks
# There's 54 unique HUC12s in overlap df
# There's 100,000+ unique HUC12s in nhd df

ov_huc <- unique(overlap$HUC12)
print(length(intersect(nhd$huc12, ov_huc)))


