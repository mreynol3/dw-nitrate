# first intro code
# doing some data analysis and stuff ya know

library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)

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

welp <- welp %>%
  mutate(era = factor(case_when(
    Year < 1900 ~ 'old',
    Year >= 1900 & Year < 1910 ~ '1900s',
    Year >= 1910 & Year < 1920 ~ '1910s',
    Year >= 1920 & Year < 1930 ~ '1920s',
    Year >= 1930 & Year < 1940 ~ '1930s',
    Year >= 1940 & Year < 1950 ~ '1940s',
    Year >= 1950 & Year < 1960 ~ '1950s',
    Year >= 1960 & Year < 1970 ~ '1960s',
    Year >= 1970 & Year < 1980 ~ '1970s',
    Year >= 1980 & Year < 1990 ~ '1980s',
    Year >= 1990 & Year < 2000 ~ '1990s',
    Year >= 2000 & Year < 2010 ~ '2000s',
    Year >= 2010 & Year < 2020 ~ '2010s',
    Year >= 2020 ~ '2020s',
  )))

ggplot(welp, aes(color = era)) +
  geom_sf(size = 0.9)

