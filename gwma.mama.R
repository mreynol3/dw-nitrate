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
  st_transform(crs = 4326)

h2o_iso <- read_excel("LUB-GWMA-Iso-Master-260413.xlsx", sheet = "LUB-GWMA-H2O-isotopes", skip = 1)
nit_iso <- read_excel("LUB-GWMA-Iso-Master-260413.xlsx", sheet = "LUB-GWMA-Nitrate-Isotopes", skip = 1)

# Nitrate Isotopes =============================================================

h2o_iso <- h2o_iso |>
  rename(WELLID = `DEQ WELL ID`,
         lat = `DECIMAL LAT`,
         lon = `DECIMAL LONG`)

nit_iso <- nit_iso |>
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

locs <- h2o_iso |>
  select(WELLID, lat, lon) |>
  drop_na(lon)

n_geo <- nit_iso |>
  left_join(locs, by = 'WELLID', relationship = "many-to-many") |>
  distinct()

gwma_missing <- n_geo |>
  filter(is.na(lon))

n_geo <- n_geo |>
  drop_na(lat, lon) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4269) |>
  st_transform(crs = 4326)

n_geo <- n_geo |>
  rename(`Nitrate-N (mg/L)` = `Nitrate-N (mg/L)...5`,
         `d-excess` = `d-excess...22`)

n_geo <- n_geo |>
  mutate(nitrate = as.double(`Nitrate-N (mg/L)`))

n_geo <- n_geo |>
  mutate(disc_nitrate = factor(case_when(nitrate < 7 ~ '< 7 mg/L', # under 25k
                                      nitrate >= 7 & nitrate < 10 ~ '7-10 mg/L', # 25k - 50k
                                      nitrate >= 10 & nitrate < 20 ~ '10-20 mg/L', # 50k - 100k
                                      nitrate > 20 ~ '> 20 mg/L', # 500k
                                      TRUE ~ NA),
                            levels = c('< 7 mg/L', '7-10 mg/L', '10-20 mg/L', '> 20 mg/L'))) |>
  arrange(disc_nitrate)

# 7, 10, above 20

danger <- rev(RColorBrewer::brewer.pal(4, "Spectral"))

n_geo |>
  drop_na(nitrate) |>
  ggplot() +
  geom_sf(aes(color = disc_nitrate), size = 2.5) +
  scale_color_manual(values = danger,
                     name = 'Measured Nitrate \nConcentration') +
  geom_sf(data = gwma.shp, fill = NA, color = "black", lwd = 0.1)

# print gwma
#
# lasar_ids <- paste(gwma_missing$`DEQ LASAR ID`, collapse = ",")
# cat(lasar_ids)

# largest source ===============================================================

lub_shp <- st_read("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/dw-nitrate/UmatillaIsotope/UmatillaIsotope/lubcomid.shp")

# test <- get_nhdplus(AOI = lub_shp)
# # Get COMIDs using nhdplusTools package
# lub_shp$COMID<- NA
# for (i in 1:nrow(lub_shp)){
#   print (i)
#   lub_shp[i,'comid'] <- nhdplusTools::discover_nhdplus_id(lub_shp[i,c('geometry')])
# }

lub_shp <- lub_shp |>
  rename(COMID = FEATUREID)

get_mets <- function(coms){
  StreamCatTools::sc_get_data(metric = 'n_ff_2017,n_lw_2017,n_hw_2017,n_uf_2017,n_cf_2017,n_dep_2017,n_leg_2017,n_ags_2017',
                              aoi='ws',
                              comid = coms,
                              showAreaSqKm = TRUE)
}

chunks <- split(lub_shp$COMID, ceiling(seq_along(lub_shp$COMID) / 50))

metrics <- as.data.frame(do.call(rbind, lapply(chunks, get_mets)))

# make ha 
# divide by ha

all_year <- all_year |>
  rowwise() |>
  mutate(n_ff_Ws = n_ff_Ws / WSAREAHA,
         n_lw_Ws = n_lw_Ws / WSAREAHA,
         n_hw_Ws = n_hw_Ws / WSAREAHA,
         n_uf_Ws = n_uf_Ws / WSAREAHA,
         n_cf_Ws = n_cf_Ws / WSAREAHA,
         n_dep_Ws = n_dep_Ws / WSAREAHA,
         n_leg_Ws = n_legWs / WSAREAHA,
         n_ags_Ws = n_ags_Ws / WSAREAHA) 




























