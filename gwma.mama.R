# LUB GWMA

library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
library(readr)
#install.packages('tidygeocoder')
library(tidygeocoder)
#install.packages("cdlTools")
library(cdlTools)
library(janitor)
library(here)
library(tidycensus)
library(readxl)

# CRITICAL
options(scipen = 50)

setwd("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/dw-nitrate")

# shapefile ====================================================================

gwma.shp <- st_read("lub-gwma-poly/Lower_Umatilla_GWMA.shp")

gwma.shp <- gwma.shp |>
  st_transform(crs = 2992)

# Nitrate Isotopes =============================================================

gwma <- read_excel('LUB-GWMA-ISO.xlsx', col_names = TRUE, skip =1) 
n.iso <- read_excel('lubgwma-nitrate-iso.xlsx', col_names = TRUE, skip =1) 

gwma <- gwma |>
  rename(WELLID = `DEQ WELL ID`,
         lat = `DECIMAL LAT`,
         lon = `DECIMAL LONG`)

n.iso <- n.iso |>
  rename(WELLID = `DEQ WELL ID`)

# gwma_fill <- gwma |>
#   group_by(WELLID) |>
#   fill(lat, lon, .direction = "downup") |>
#   ungroup()
# 
# gwma_missing <- gwma_fill |>
#   filter(is.na(lon))
# 
# geo.gwma <- gwma_fill |>
#   drop_na(lon)

locs <- gwma |>
  select(WELLID, lat, lon) |>
  drop_na(lon)

n.geo <- n.iso |>
  left_join(locs, by = 'WELLID', relationship = "many-to-many") |>
  distinct()

gwma_missing <- n.geo |>
  filter(is.na(lon))

n.geo <- n.geo |>
  drop_na(lat, lon) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4269) |>
  st_transform(crs = 2992)

n.geo <- n.geo |>
  rename(`Nitrate-N (mg/L)` = `Nitrate-N (mg/L)...5`,
         `d-excess` = `d-excess...22`)

n.geo <- n.geo |>
  mutate(nitrate = as.double(`Nitrate-N (mg/L)`))

n.geo |>
  ggplot() +
  geom_sf(aes(color = `δ15N vs Air`)) +
  geom_sf(data = gwma.shp, fill = NA, color = "black", lwd = 0.1)

# print gwma
#
# lasar_ids <- paste(gwma_missing$`DEQ LASAR ID`, collapse = ",")
# cat(lasar_ids)






























