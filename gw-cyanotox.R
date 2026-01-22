# cyanotoxin transport =========================================================

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
library(janitor)
library(StreamCatTools)

setwd("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/dw-nitrate/gw-cyanotox")

# compilation ------------------------------------------------------------------

# pws_inv <- read.csv('O:/Public/Pennino/HABs/PWS_inventory_gwsw_2023Q4.csv')
# pws_gw <- pws_inv |>
#   filter(str_detect(Primary.Source, regex("ground", ignore_case = TRUE)))

# community water system data? 



ucmr4 <- read.csv('O:/Public/Pennino/HABs/ucmr_4_main.csv')
ucmr_cyano <- ucmr4 |>
  filter(facility_water_type == 'GU') |>
  filter(contaminant == 'total microcystin' | contaminant == 'anatoxin-a' | contaminant == 'cylindrospermopsin') |>
  rename(PWSID = pws_id)

ucmr_cyano <- ucmr_cyano |>
  mutate(cyano = factor(case_when(
    contaminant == 'total microcystin' ~ 'micx',
    contaminant == 'anatoxin-a' ~ 'ana',
    contaminant == 'cylindrospermopsin' ~ 'cyl'
  )),
  present = 1) |>
  select(PWSID, pws_name, state, cyano, present) |>
  rename(PWS_name = pws_name) |>
  distinct()

ucmr_GU <- ucmr4 |>
  filter(facility_water_type == 'GU') |>
  rename(PWSID = pws_id)

# ucrm_gw data frame represents GWUDI systems that have cyanotoxin presence detected
# must combined with some sort of geographic information 

# geographic information
# PWS_FAC_to_HUC12.csv: has duplicates due to facilities for a single PWS being in multiple HUC12s
pws_huc <- read_csv("pws_to_huc_no_note.csv", col_types = cols(HUC12 = col_character()))

pws_huc <- pws_huc |>
  mutate(HUC12 = format(HUC12, scientific = FALSE))

# check how many overlap 
#length(intersect(pws_huc$PWSID, ucmr_cyano$PWSID))

# join
#geo_cyanotox <- inner_join(pws_huc, ucmr_cyano, by = 'PWSID', relationship = 'many-to-many')
# add geospatial
nhd <- st_layers("O:/LAB/COR/Geospatial_Library_Resource/Physical/HYDROLOGY/WBD/WBD_National_GDB.gdb", layer = 'WBDHU12')

#check overlap 
# length(intersect(nhd$huc12, geo_cyanotox$HUC12))

nhd <- nhd |>
  rename(HUC12 = huc12)

# geo_all == ucmr data filtered to cyanotoxins and GWUDI then joined with HUC12 geospatial data using Micheal's
# conversion methods and NHD huc info. Represents 94 PWS systems 
# geo_all <- nhd |>
#   inner_join(geo_cyanotox, by = 'HUC12', relationship = 'many-to-many')

states <- tigris::states(cb = TRUE, progress_bar = FALSE)  %>%
  filter(!STUSPS %in% c('HI', 'PR', 'AK', 'MP', 'GU', 'AS', 'VI'))  %>%
  st_transform(crs = 5072)

# geo_all <- geo_all |>
#   st_transform(crs = 5072)
# 
# geo_all <- geo_all |>
#   st_point_on_surface()
# 
# ggplot() +
#   geom_sf(data = geo_all, fill = NA) + 
#   geom_sf(data = states, fill = NA, color = "black", lwd = 0.1) +
#   geom_sf(data = geo_combo)
#   
# ggplot(geo_combo, aes(color = pred_cyano_fit)) +
#   geom_sf()
# 
# geo_combo <- st_filter(PredData, geo_all, .predicate = st_within)
# is_within <- as.vector(geo_combo)
# 
# length(PredData[is_within, ])

# idaho deq data

# idaho <- read_csv('EDMS_2025.csv')
# sw_deq <- st_read('IDEQ_SWA_DELINEATIONS')
# 
# # wisconsin shallow gw database
# 
# wisc <- st_read("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/dw-nitrate/WISC_gwdb/Private Well - PFAS Shallow Groundwater Study Results.gdb", layer = 'PW_PFAS_Study_Sample_Data')
# 
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

# glimpse the echo data

echo_co <- read_csv('ECHO_CO.csv') |>
  rename(PWSID = SDWAIDs)

co_all <- echo_co |>
  left_join(CO, by = 'PWSID') |>
  st_as_sf(coords = c("FacLong", "FacLat"), crs = 4326) |>
  st_transform(crs = 5072)
# 
# ggplot(co_all, aes(color = SDWAComplianceStatus)) +
#   geom_sf() +
#   geom_sf(data = states, fill = NA, color = 'black')


# state compilation ============================================================

# If I want to compile all these PWS types I might have to pull from 
# SDWIS / individual states 

# Loading and cleaning data

nhd_clean <- nhd |>
  select(HUC12, tohuc, shape_Area, shape, states) 

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

overlap_df <- intersect(gwudi_all$PWSID, umcr_GU_sample$PWSID)
unique_ucmr <- umcr_GU_sample[!(umcr_GU_sample$PWSID %in% overlap_df), ]

gwudi_all <- gwudi_all |>
  bind_rows(unique_ucmr) |>
  mutate(state = substr(PWSID, 1, 2)) |>
  distinct()

pws_huc <- pws_huc |>
  select(-PWSID_FACILTIYID) |>
  mutate(state = substr(PWSID, 1, 2))

test <- gwudi_all |>
  left_join(pws_huc, by = 'PWSID', relationship = "one-to-many") |>
  distinct()

test <- test |>
  left_join(nhd_clean, by = 'HUC12') |>
  drop_na(PWSID) |>
  st_as_sf() |>
  filter(!state.x == "AK" & !state.x == 'AS')

test <- test |>
  st_transform(crs = 5072)

all <- test |>
  left_join(ucmr_cyano, by = 'PWSID', relationship = 'many-to-many') |>
  mutate(present = as.factor(replace_na(present, 0)),
         cyano = as.character(cyano),
         cyano = replace_na(cyano, "non-detect"),
         cyano = factor(cyano, levels = c())
  )


ggplot(all, aes(color = present)) +
  geom_sf() +
  scale_color_manual(values = c("blue", "orange")) +
  geom_sf(data = states, fill = NA, color = 'black')

# color by cyano
ggplot(all, aes(color = cyano)) +
  geom_sf() +
  scale_color_manual(values = c("blue", "orange")) +
  geom_sf(data = states, fill = NA, color = 'black')


# SDWIS_GU <- readxl::read_excel('state_data/SDWIS_GU.xlsx')
# GDWIS_GUP <- readxl::read_excel('state_data/SDWIS_GUP.xlsx')
# 
# SDWIS <- bind_rows(SDWIS_GU, GDWIS_GUP) |>
#   mutate(state = substr(PWSID, 1, 2))
# 
# SDWISmini <- SDWIS |>
#   rename(Type = `PWS Type`,
#          County = `Counties Served`) |>
#   select(PWSID, PWS_name, Type, County)
# 
# df <- bind_rows(gwudi_all, SDWISmini) |>
#   distinct()
# 
# df <- df |>
#   inner_join(pws_huc, by = 'PWSID',relationship = "many-to-many") |>
#   drop_na(PWSID)
# 
# df <- df |>
#   inner_join(nhd, by = 'HUC12', relationship = 'many-to-many')
# 
# 
# length(unique(SDWIS$PWSID))
# # 1124
# length(unique(gwudi_all$PWSID))
# # 1142
# length(intersect(SDWIS$PWSID, gwudi_all$PWSID))
# # 650
# length(setdiff(SDWIS$PWSID, gwudi_all$PWSID))
# # 474
# length(setdiff(gwudi_all$PWSID, SDWIS$PWSID))
# # 492
# # there's 1616 unique PWSID that are GWUDI 
# 
# length(symdiff(gwudi_all$PWSID, SDWIS$PWSID))
# 
# list_pws <- symdiff(gwudi_all$PWSID, SDWIS$PWSID)
# list_pws2 <- intersect(SDWIS$PWSID, gwudi_all$PWSID)

# census block data for PWS ====================================================

census_blocks <- read_csv('CWS_Boundaries_Latest/Census_Tables/Blocks_V_2_1.csv')

or_blocks <- census_blocks |>
  mutate(state = substr(PWSID, 1, 2)) |>
  filter(state == 'OR')

# watershed level data 

crosswalk <- foreign::read.dbf('O:/LAB/COR/Geospatial_Library_Resource/Physical/HYDROLOGY/NHDPlusV21/NHDPlusNationalData/NHDPlusV1Network_V2Network_Crosswalk.dbf')

get_nlcd <- function(coms){
  sc_get_data(metric = 'PctWdWet2016, PctUrbMd2016, PctUrbLo2016, PctUrbHi2016,
                          PctMxFst2016, PctCrop2016, PctHay2016, PctDecid2016,
                          PctConif2016, PctUrbOp2016, PctHbWet2016',
              aoi='watershed',
              comid = coms,
              showAreaSqKm = TRUE)
}

chunks <- split(PredData$COMID, ceiling(seq_along(PredData$COMID) / 10000))

ncldMas <- do.call(rbind, lapply(chunks, get_nlcd))

# ncldMas <- ncldMas |>
#   dplyr::select(-WSAREASQKM)

ncldMas <- ncldMas |>
  mutate(agr_ws = PCTCROP2016WS + PCTHAY2016WS,
         dev_ws = PCTURBLO2016WS + PCTURBMD2016WS + PCTURBOP2016WS + PCTURBHI2016WS,
         fst_ws = PCTMXFST2016WS + PCTDECID2016WS + PCTCONIF2016WS,
         wet_ws = PCTWDWET2016WS + PCTHBWET2016WS,
         COMID = COMID,
         .keep = 'unused')

# Oregon cyanotoxin data =======================================================

or_tox <- read_csv('cyanotoxin-sample-results-all.csv')

or_tox <- or_tox |>
  rename(PWSID = `PWS ID`,
         PWS_name = `PWS Name\n\t\t\t\t\n`,
         County = `County Served`,
         micx = `Total Microcystins(ug/L)`,
         cylin = `Cylindrospermopsin(ug/L)`)  |>
  mutate(PWSID = paste0("OR", PWSID),
         micx = as.numeric(micx),
         micx = replace_na(micx, 0),
         cylin = as.numeric(cylin),
         cylin = replace_na(cylin, 0))

intersect(or_tox$PWSID, or$PWSID)

or_tox <- or_tox |>
  filter(PWSID == "OR00839" |PWSID == "OR00835" | PWSID == "OR00724" | PWSID == "OR94929" | PWSID == "OR90476")



# lets try colorado ============================================================

hab_co <- read_csv('state_data/ToxicAlgaeDataHistoricalResults-co.csv')

hab_co <- hab_co |>
  filter(`Tested, toxic levels of algae found` == 'Toxic levels of algae found')

hab_co <- hab_co |>
  mutate(lake_coords = factor(case_when(
    `Water body`  ~ 'HNHM',
    (n_dev_inputs < 10 | p_farm_inputs < 4) & pred_micx_fit >= 0.50 ~ 'LNHM',
    (n_dev_inputs >= 10 | p_farm_inputs >= 4) & pred_micx_fit < 0.50 ~ 'HNLM',
    (n_dev_inputs < 10 | p_farm_inputs < 4) & pred_micx_fit < 0.50 ~ 'LNLM',
    TRUE ~ 'OTHER')))


loc <- "O:/LAB/COR/Geospatial_Library_Resource/Physical/HYDROLOGY/NHDPlusV21/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"

wbd <- sf::st_read(dsn = loc, layer = 'NHDWaterbody') |>
  st_transform(5072)

wbd_copy <- wbd |>
  subset(COMID %in% PredData$COMID) |>
  dplyr::select(COMID, MaxDepth, Shape)

for (Shape in 1:length(wbd_copy)) {
  shapes <- wbd_copy$Shape
  wbd_copy$lake_area <- st_area(shapes)
}

wbd_copy <- wbd_copy |>
  mutate(lake_area = drop_units(lake_area)) |>
  st_point_on_surface()

    



