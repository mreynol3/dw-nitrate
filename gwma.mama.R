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

n_geo_2 <- n_geo |>
  drop_na(nitrate)


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

metrics <- metrics |>
  #rename(COMID = comid) |>
  rowwise() |>
  mutate(WSAREAHA = wsareasqkm * 100)

metrics <- metrics |>
  rowwise() |>
  mutate(n_ff_ha = n_ff_2017ws / WSAREAHA,
         n_lw_ha = n_lw_2017ws / WSAREAHA,
         n_hw_ha = n_hw_2017ws / WSAREAHA,
         n_uf_ha = n_uf_2017ws / WSAREAHA,
         n_cf_ha = n_cf_2017ws / WSAREAHA,
         n_dep_ha = n_dep_2017ws / WSAREAHA,
         n_leg_ha = n_leg_2017ws / WSAREAHA,
         n_ags_ha = n_ags_2017ws / WSAREAHA) 

metmini <- metrics |>
  dplyr::select(COMID, WSAREAHA:n_ags_ha)

lub_all <- lub_shp |>
  left_join(metmini, by = 'COMID')

n_cols = c('n_ff_ha', 'n_lw_ha', 'n_uf_ha', 'n_hw_ha', 'n_dep_ha', 'n_cf_ha', 'n_leg_ha', 'n_ags_ha')
n_mod = c('n_ff_ha', 'n_lw_ha', 'n_uf_ha', 'n_hw_ha', 'n_dep_ha', 'n_cf_ha')

lub_all <- lub_all |> 
  rowwise() |>
  mutate(largest_source = paste0(n_cols[c_across(all_of(n_cols)) == max(c_across(all_of(n_cols)), na.rm=TRUE)], collapse = '_'),
         modern_larsource = paste0(n_mod[c_across(all_of(n_mod)) == max(c_across(all_of(n_mod)), na.rm=TRUE)], collapse = '_'))

lub_all <- lub_all |>
  mutate(modern_larsource = factor(case_when(
    modern_larsource == 'n_ff_ha' ~ 'Farm Fertilizer',
    modern_larsource == 'n_cf_ha' ~ 'Crop N-Fixation',
    modern_larsource == 'n_uf_ha' ~ 'Urban Fertilizer',
    modern_larsource == 'n_hw_ha' ~ 'Human Waste',
    modern_larsource == 'n_lw_ha' ~ 'Livestock Manure',
    modern_larsource == 'n_dep_ha' ~ 'Total Deposition',
    TRUE ~ 'None'
  ), levels = c('Farm Fertilizer','Livestock Manure','Crop N-Fixation','Urban Fertilizer','Total Deposition','Human Waste','None')))

lub_all <- lub_all |>
  rowwise() |>
  mutate(TN = rowSums(across(c(n_ff_ha:n_ags_ha))))
  
  
colorsN <- c('Farm Fertilizer' = '#A3CC51',
             'Crop N-Fixation'='#FFD700',
             'Livestock Manure'='#B26F2C',
             'Total Deposition'='#6db6ff',
             'Human Waste'='#E51932',
             'Urban Fertilizer'='black')

danger <- rev(RColorBrewer::brewer.pal(4, "Spectral"))

# largest source map
ggplot() +
  geom_sf(data = lub_all, aes(fill = modern_larsource)) +
  scale_fill_manual(values = colorsN,
                    name = "Largest Source") +
  geom_sf(data = n_geo_2, aes(color = disc_nitrate), size = 2.5) +
  scale_color_manual(values = danger,
                     name = 'Measured Well \nNitrate Concentration') +
  theme_void()

# TN map 
ggplot() +
  geom_sf(data = lub_all, aes(fill = TN)) +
  scale_fill_continuous(palette = c('#84C767', '#E51932'),
                        name = "Total N Inputs \n(kg/ha/yr)") +
  geom_sf(data = n_geo_2, aes(color = disc_nitrate), size = 2.5) +
  scale_color_manual(values = danger,
                     name = 'Measured Well \nNitrate Concentration') +
  theme_void()

# map function for indiv sources
map <- function(var, col_pal, name){
ggplot() +
  geom_sf(data = lub_all, aes(fill = var)) +
  scale_fill_continuous(palette = col_pal,
                        name = paste(name)) +
  geom_sf(data = n_geo_2, aes(color = disc_nitrate), size = 2.5) +
  scale_color_manual(values = danger,
                     name = 'Measured Well \nNitrate Concentration') +
  theme_void()
}

dep_map <- map(lub_all$n_dep_ha, c("#E6F2FF", '#6db6ff', "#0069D1"), 'Total Deposition\n(kg/ha/yr)')
fert_map <- map(lub_all$n_ff_ha, c("#E2EFC8", "#A3CC51", "#617E25"), 'Farm Fertilizer\n(kg/ha/yr)')
lw_map <- map(lub_all$n_lw_ha, c("#F1DBC6", "#B26F2C", "#5E3B17"), 'Livestock Waste\n(kg/ha/yr)')
cf_map <- map(lub_all$n_cf_ha, c("#FFF4B8", "#FFD700", "#A38B00"), 'Crop Fixation\n(kg/ha/yr)')
hw_map <- map(lub_all$n_hw_ha, c("#FCE8EA", "#E51932", "#931020"), 'Human Waste\n(kg/ha/yr)')

ggsave("n_LARGEST_SOURCE_map.jpeg", width = 12, height = 7, device = 'jpeg', units = 'in', dpi = 600)












