# first intro code
# doing some data analysis and stuff ya know

library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
library(readr)
install.packages('tidygeocoder')
library(tidygeocoder)

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




