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
                              aoi='cat',
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
  mutate(n_ff_ha = n_ff_2017cat / WSAREAHA,
         n_lw_ha = n_lw_2017cat / WSAREAHA,
         n_hw_ha = n_hw_2017cat / WSAREAHA,
         n_uf_ha = n_uf_2017cat / WSAREAHA,
         n_cf_ha = n_cf_2017cat / WSAREAHA,
         n_dep_ha = n_dep_2017cat / WSAREAHA,
         n_leg_ha = n_leg_2017cat / WSAREAHA,
         n_ags_ha = n_ags_2017cat / WSAREAHA) 

metmini <- metrics |>
  rename(COMID = comid) |>
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
  mutate(TN = (n_ff_ha + n_lw_ha + n_uf_ha + n_hw_ha + n_dep_ha + n_cf_ha))
  
# maps / plots =================================================================

colorsN <- c('Farm Fertilizer' = '#A3CC51',
             'Crop N-Fixation'='#FFD700',
             'Livestock Manure'='#B26F2C',
             'Total Deposition'='#6db6ff',
             'Human Waste'='#E51932',
             'Urban Fertilizer'='black')

danger <- rev(RColorBrewer::brewer.pal(4, "Spectral"))

# sc get nni ------------------------------------
lub_com <- paste(lub_all$COMID, collapse = ",")
all_years <- paste(seq(from = 1987, to = 2017), collapse = ',')

data <- StreamCatTools::sc_get_nni(year = all_years, aoi= 'Ws', comid = lub_com)

plot <- StreamCatTools::sc_plotnni(comid = '24233060')

# largest source map ----------------------------
ggplot() +
  geom_sf(data = lub_all, aes(fill = modern_larsource)) +
  scale_fill_manual(values = colorsN,
                    name = "Largest Source") +
  geom_sf(data = n_geo_2, aes(color = disc_nitrate), size = 2.5) +
  scale_color_manual(values = danger,
                     name = 'Measured Well \nNitrate Concentration') +
  theme_void()

# iso data w/ source ---------------------------

n_geo <- n_geo |>
  mutate(disc_iso = factor(case_when(`δ15N vs Air` < 5 ~ '< 5', 
                                         `δ15N vs Air` >= 5 & `δ15N vs Air` < 10 ~ '5-10',
                                         `δ15N vs Air` >= 10 ~ '> 10', 
                                         TRUE ~ NA),
                               levels = c('< 5', '5-10', '> 10'))) |>
  arrange(disc_iso)

small_danger <- rev(RColorBrewer::brewer.pal(3, "PuOr"))


ggplot() +
  geom_sf(data = lub_all, aes(fill = modern_larsource)) +
  scale_fill_manual(values = colorsN,
                    name = "Largest Source") +
  geom_sf(data = drop_na(n_geo, `δ15N vs Air`), aes(color = disc_iso), size = 2.5) +
  scale_color_manual(values = c('#FF7978','#85B2DC','#FFCF7A'),
                     name = 'N Isotopic Signature') +
  theme_void()

# TN map ---------------------------------------
ggplot() +
  geom_sf(data = lub_all, aes(fill = TN)) +
  scale_fill_continuous(palette = c('#F2D9FC', '#6A0994'),
                        name = "Total N Inputs \n(kg/ha/yr)") +
  geom_sf(data = drop_na(n_geo, `δ15N vs Air`), aes(color = disc_iso), size = 2.5) +
  scale_color_manual(values = c('#FF7978','#85B2DC','#FFCF7A'),
                     name = 'δ15N Value') +
  theme_void()

# '#89374FFF', '#C46F3EFF', '#B79E4BFF'
# '#89973DFF','#E8B92FFF', '#A45E41FF'

# map function for indiv sources --------------
map <- function(var, col_pal, name){
ggplot() +
  geom_sf(data = lub_all, aes(fill = var)) +
  scale_fill_continuous(palette = col_pal,
                        name = paste(name)) +
  geom_sf(data = drop_na(n_geo, `δ15N vs Air`), aes(color = disc_iso), size = 2.5) +
  scale_color_manual(values = c('#FF7978','#85B2DC','#FFCF7A'),
                     name = 'N Isotopic Signature') +
  theme_void()
}

expression(paste("TN Input Rate (kg N ", ha^{-1}, yr^{-1},")", sep=""))

dep_map <- map(lub_all$n_dep_ha, c("#E6F2FF", '#6db6ff', "#0069D1"), 'Total Deposition\n(kg/ha/yr)')
fert_map <- map(lub_all$n_ff_ha, c("#E2EFC8", "#A3CC51", "#617E25"), 'Farm Fertilizer\n(kg/ha/yr)')
lw_map <- map(lub_all$n_lw_ha, c("#F1DBC6", "#B26F2C", "#5E3B17"), 'Livestock Waste\n(kg/ha/yr)')
cf_map <- map(lub_all$n_cf_ha, c("#FFF4B8", "#FFD700", "#A38B00"), 'Crop Fixation\n(kg/ha/yr)')
hw_map <- map(lub_all$n_hw_ha, c("#FCE8EA", "#E51932", "#931020"), 'Human Waste\n(kg/ha/yr)')

ggsave("total_n_map.jpeg", width = 12, height = 7, device = 'jpeg', units = 'in', dpi = 600)


# LUB inputs trends ============================================================

comid = as.character(lub_shp$COMID)

# Get StreamCat data
test <- sc_get_data(metric = 'n_dep_1990,n_ff_1990,n_uf_1990,n_lw_1990,n_hw_1990,n_ags_1990,n_cf_1990,n_cr_1990,p_cr_1990,p_lw_1990,p_hw_1990,p_uf_1990,p_ff_1990,p_ags_1990,n_dep_1991,n_ff_1991,n_uf_1991,n_lw_1991,n_hw_1991,n_ags_1991,n_cf_1991,n_cr_1991,p_cr_1991,p_lw_1991,p_hw_1991,p_uf_1991,p_ff_1991,p_ags_1991,n_dep_1992,n_ff_1992,n_uf_1992,n_lw_1992,n_hw_1992,n_ags_1992,n_cf_1992,n_cr_1992,p_cr_1992,p_lw_1992,p_hw_1992,p_uf_1992,p_ff_1992,p_ags_1992,n_dep_1993,n_ff_1993,n_uf_1993,n_lw_1993,n_hw_1993,n_ags_1993,n_cf_1993,n_cr_1993,p_cr_1993,p_lw_1993,p_hw_1993,p_uf_1993,p_ff_1993,p_ags_1993,n_dep_1994,n_ff_1994,n_uf_1994,n_lw_1994,n_hw_1994,n_ags_1994,n_cf_1994,n_cr_1994,p_cr_1994,p_lw_1994,p_hw_1994,p_uf_1994,p_ff_1994,p_ags_1994,n_dep_1995,n_ff_1995,n_uf_1995,n_lw_1995,n_hw_1995,n_ags_1995,n_cf_1995,n_cr_1995,p_cr_1995,p_lw_1995,p_hw_1995,p_uf_1995,p_ff_1995,p_ags_1995,n_dep_1996,n_ff_1996,n_uf_1996,n_lw_1996,n_hw_1996,n_ags_1996,n_cf_1996,n_cr_1996,p_cr_1996,p_lw_1996,p_hw_1996,p_uf_1996,p_ff_1996,p_ags_1996,n_dep_1997,n_ff_1997,n_uf_1997,n_lw_1997,n_hw_1997,n_ags_1997,n_cf_1997,n_cr_1997,p_cr_1997,p_lw_1997,p_hw_1997,p_uf_1997,p_ff_1997,p_ags_1997,n_dep_1998,n_ff_1998,n_uf_1998,n_lw_1998,n_hw_1998,n_ags_1998,n_cf_1998,n_cr_1998,p_cr_1998,p_lw_1998,p_hw_1998,p_uf_1998,p_ff_1998,p_ags_1998,n_dep_1999,n_ff_1999,n_uf_1999,n_lw_1999,n_hw_1999,n_ags_1999,n_cf_1999,n_cr_1999,p_cr_1999,p_lw_1999,p_hw_1999,p_uf_1999,p_ff_1999,p_ags_1999,n_dep_2000,n_ff_2000,n_uf_2000,n_lw_2000,n_hw_2000,n_ags_2000,n_cf_2000,n_cr_2000,p_cr_2000,p_lw_2000,p_hw_2000,p_uf_2000,p_ff_2000,p_ags_2000,n_dep_2001,n_ff_2001,n_uf_2001,n_lw_2001,n_hw_2001,n_ags_2001,n_cf_2001,n_cr_2001,p_cr_2001,p_lw_2001,p_hw_2001,p_uf_2001,p_ff_2001,p_ags_2001,n_dep_2002,n_ff_2002,n_uf_2002,n_lw_2002,n_hw_2002,n_ags_2002,n_cf_2002,n_cr_2002,p_cr_2002,p_lw_2002,p_hw_2002,p_uf_2002,p_ff_2002,p_ags_2002,n_dep_2003,n_ff_2003,n_uf_2003,n_lw_2003,n_hw_2003,n_ags_2003,n_cf_2003,n_cr_2003,p_cr_2003,p_lw_2003,p_hw_2003,p_uf_2003,p_ff_2003,p_ags_2003,n_dep_2004,n_ff_2004,n_uf_2004,n_lw_2004,n_hw_2004,n_ags_2004,n_cf_2004,n_cr_2004,p_cr_2004,p_lw_2004,p_hw_2004,p_uf_2004,p_ff_2004,p_ags_2004,n_dep_2005,n_ff_2005,n_uf_2005,n_lw_2005,n_hw_2005,n_ags_2005,n_cf_2005,n_cr_2005,p_cr_2005,p_lw_2005,p_hw_2005,p_uf_2005,p_ff_2005,p_ags_2005,n_dep_2006,n_ff_2006,n_uf_2006,n_lw_2006,n_hw_2006,n_ags_2006,n_cf_2006,n_cr_2006,p_cr_2006,p_lw_2006,p_hw_2006,p_uf_2006,p_ff_2006,p_ags_2006,n_dep_2007,n_ff_2007,n_uf_2007,n_lw_2007,n_hw_2007,n_ags_2007,n_cf_2007,n_cr_2007,p_cr_2007,p_lw_2007,p_hw_2007,p_uf_2007,p_ff_2007,p_ags_2007,n_dep_2008,n_ff_2008,n_uf_2008,n_lw_2008,n_hw_2008,n_ags_2008,n_cf_2008,n_cr_2008,p_cr_2008,p_lw_2008,p_hw_2008,p_uf_2008,p_ff_2008,p_ags_2008,n_dep_2009,n_ff_2009,n_uf_2009,n_lw_2009,n_hw_2009,n_ags_2009,n_cf_2009,n_cr_2009,p_cr_2009,p_lw_2009,p_hw_2009,p_uf_2009,p_ff_2009,p_ags_2009,n_dep_2010,n_ff_2010,n_uf_2010,n_lw_2010,n_hw_2010,n_ags_2010,n_cf_2010,n_cr_2010,p_cr_2010,p_lw_2010,p_hw_2010,p_uf_2010,p_ff_2010,p_ags_2010,n_dep_2011,n_ff_2011,n_uf_2011,n_lw_2011,n_hw_2011,n_ags_2011,n_cf_2011,n_cr_2011,p_cr_2011,p_lw_2011,p_hw_2011,p_uf_2011,p_ff_2011,p_ags_2011,n_dep_2012,n_ff_2012,n_uf_2012,n_lw_2012,n_hw_2012,n_ags_2012,n_cf_2012,n_cr_2012,p_cr_2012,p_lw_2012,p_hw_2012,p_uf_2012,p_ff_2012,p_ags_2012,n_dep_2013,n_ff_2013,n_uf_2013,n_lw_2013,n_hw_2013,n_ags_2013,n_cf_2013,n_cr_2013,p_cr_2013,p_lw_2013,p_hw_2013,p_uf_2013,p_ff_2013,p_ags_2013,n_dep_2014,n_ff_2014,n_uf_2014,n_lw_2014,n_hw_2014,n_ags_2014,n_cf_2014,n_cr_2014,p_cr_2014,p_lw_2014,p_hw_2014,p_uf_2014,p_ff_2014,p_ags_2014,n_dep_2015,n_ff_2015,n_uf_2015,n_lw_2015,n_hw_2015,n_ags_2015,n_cf_2015,n_cr_2015,p_cr_2015,p_lw_2015,p_hw_2015,p_uf_2015,p_ff_2015,p_ags_2015,n_dep_2016,n_ff_2016,n_uf_2016,n_lw_2016,n_hw_2016,n_ags_2016,n_cf_2016,n_cr_2016,p_cr_2016,p_lw_2016,p_hw_2016,p_uf_2016,p_ff_2016,p_ags_2016,n_dep_2017,n_ff_2017,n_uf_2017,n_lw_2017,n_hw_2017,n_ags_2017,n_cf_2017,n_cr_2017,p_cr_2017,p_lw_2017,p_hw_2017,p_uf_2017,p_ff_2017,p_ags_2017',
                   aoi='ws',
                   comid = '179',
                   showAreaSqKm = FALSE,
                   showPctFull = FALSE)

nni_with_total <- rbind(nni, c(A = "Total", colSums(nni[, -1])))

nni <- nni_with_total |>
  filter(comid == 'Total')

nni[] <- lapply(nni, as.numeric)
nni[is.na(nni)] <- 1

# declare NULL metrics created w/in function
year <- NULL
value <- NULL
ags <- NULL
cr <- NULL
totag <- NULL
metric <- NULL
estimated <- NULL
STUSPS <- NULL
#Create N inputs df
nin <- nni[, grepl("^(n)", names(nni)) & !grepl("(cr)", names(nni)) & !grepl("(ags)", names(nni))]

names(nin) <- sapply(names(nin), function(col){
  substr(col, 3, nchar(col) -2)
})

nin <- nin |>
  tidyr::pivot_longer(
    cols = tidyselect::everything(),
    names_to = c("metric", "year"),
    names_sep = "_",
    values_to = "value"
  ) |>
  dplyr::mutate(year = as.integer(year)) |>
  dplyr::mutate(value = value / 1000000)

#Create P inputs df

pin <- nni[, grepl("^(p)", names(nni)) & !grepl("(cr)", names(nni)) & !grepl("(ags)", names(nni))]

names(pin) <- sapply(names(pin), function(col){
  substr(col, 3, nchar(col) -2)
})

pin <- pin |>
  tidyr::pivot_longer(
    cols = tidyselect::everything(),
    names_to = c("metric", "year"),
    names_sep = "_",
    values_to = "value"
  ) |>
  dplyr::mutate(year = as.integer(year)) |>
  dplyr::mutate(value = value / 1000000)

#Create N dfs for lines (cr, agsur, nue)

nlines <- nni[, grepl("^n_cr|^n_ags", names(nni))]

names(nlines) <- sapply(names(nlines), function(col){
  substr(col, 3, nchar(col) -2)
})

nlines <- nlines |>
  tidyr::pivot_longer(
    cols = tidyselect::everything(),
    names_to = c("metric","year"),
    names_sep = "_",
    values_to = "value"
  ) |>
  dplyr::mutate(year = as.integer(year)) |>
  dplyr::mutate(value = value / 1000000) |>
  tidyr::pivot_wider(
    names_from = 'metric', 
    values_from = 'value'
  ) |>
  dplyr::mutate(totag = ags + cr)  |>
  dplyr::mutate(nue = (cr / totag) * 100) |>
  tidyr::pivot_longer(
    cols = !year, 
    names_to="metric", 
    values_to="value")

ncrag <- nlines |>
  dplyr::filter(metric %in% c('ags', 'cr'))

nue <- nlines |>
  dplyr::filter(metric == 'nue') |>
  tidyr::pivot_wider(names_from = 'metric',
                     values_from = 'value')

#Create P dfs for lines (cr, agsur, pue)

plines <- nni[, grepl("^p_cr|^p_ags", names(nni))]

names(plines) <- sapply(names(plines), function(col){
  substr(col, 3, nchar(col) -2)
})


plines <- plines |>
  tidyr::pivot_longer(
    cols = tidyselect::everything(),
    names_to = c("metric","year"),
    names_sep = "_",
    values_to = "value"
  ) |>
  dplyr::mutate(year = as.integer(year)) |>
  dplyr::mutate(value = value / 1000000) |>
  tidyr::pivot_wider(
    names_from = 'metric', 
    values_from = 'value'
  ) |>
  dplyr::mutate(totag = ags + cr)  |>
  dplyr::mutate(nue = (cr / totag) * 100) |>
  tidyr::pivot_longer(
    cols = !year, 
    names_to="metric", 
    values_to="value")

pcrag <- plines |>
  dplyr::filter(metric %in% c('ags', 'cr'))

pue <- plines |>
  dplyr::filter(metric == 'nue') |>
  tidyr::pivot_wider(names_from = 'metric',
                     values_from = 'value')

pdf <- dplyr::bind_rows(plines, pin)

ndf <- dplyr::bind_rows(nlines, nin)

#create estimate column
knownfertyrs <- c(1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,
                  2005,2006,2007,2008,2009,2010,2011,2012,2017)
nwsin <- nin |>
  dplyr::mutate(estimated=dplyr::case_when(
    metric == "dep" ~ FALSE,
    metric == "hw" ~ FALSE,
    metric == "cf" & year %in% c(1987,1992,1997,2002,2007,2012, 2017) ~ FALSE,
    metric == "ff" & year %in% knownfertyrs ~ FALSE,
    metric == "uf" & year %in% knownfertyrs ~ FALSE,
    metric == "lw" & year %in% c(1987,1992,1997,2002,2007,2012,2017) ~ FALSE,
    TRUE ~ TRUE
  )) 

pwsin <- pin |>
  dplyr::filter(metric != 'cr') |>
  dplyr::mutate(estimated=dplyr::case_when(
    metric == "hw" ~ FALSE,
    metric == "lw" & year %in% c(1987,1992,1997,2002,2007,2012,2017) ~ FALSE,
    metric == "ff" & year %in% knownfertyrs ~ FALSE,
    metric == "uf" & year %in% knownfertyrs ~ FALSE,
    TRUE ~ TRUE
  ))

#get ready for plot
colorsn <- c('ff' = '#A3CC51', 'lw'='#B26F2C','hw'='#E51932','uf'='black', 'dep'='#6db6ff', 'cf'='#FFD700')
colorsp <- c('ff' = '#A3CC51', 'lw'='#B26F2C','hw'='#E51932','uf'='black')

nwsin$metric <- factor(nwsin$metric, levels = c('uf','hw','dep','lw','cf','ff'))
pwsin$metric <- factor(pwsin$metric, levels = c('uf','hw','lw','ff'))

#Get COMID location and states for inset map
#states
states <- 
  tigris::states(cb = TRUE, progress_bar = FALSE) |>
  dplyr::filter(!STUSPS %in% c('HI', 'PR', 'AK', 'MP', 'GU', 'AS', 'VI')) |> 
  sf::st_transform(crs = 5070)

#comid
comidint <- as.integer(comid)
flowline <- nhdplusTools::get_nhdplus(comid = comidint, realization = "flowline")
point <- sf::st_centroid(flowline)

#create N bar plot
nbar <- ggplot() + 
  ggpattern::geom_bar_pattern(data = nwsin, 
                              aes(x=year,y=value, fill=metric, 
                                  pattern=factor(estimated, levels=c(TRUE,FALSE),
                                                 labels=c('Estimated','Non-Estimated'))),
                              pattern = ifelse(nwsin$estimated, 'stripe','none'),
                              pattern_color='white',
                              pattern_density=0.05, 
                              pattern_fill = 'white', 
                              pattern_alpha = 0.5, 
                              pattern_spacing=0.025,
                              stat='identity', position='stack', 
                              pattern_size=0.05) +
  labs(title = 'Nitrogen (million kg/year)',
       y = "Budget",
       x = " ") +
  scale_fill_manual(values=colorsn,
                    labels = c('ff' = 'Farm Fertilizer',
                               'uf' = 'Urban Fertilizer',
                               'cf' = 'Crop N-Fixation',
                               'lw' = 'Livestock Manure',
                               'hw' = 'Human Waste',
                               'dep' = 'Total Deposition')) + 
  ggpattern::scale_pattern_manual(name='Estimate Status',
                       values=c('Estimated'='stripe','Non-estimated'='none')) + 
  geom_line(data=ncrag, 
            aes(x=year,y=value, linetype=metric), 
            linewidth=1.25, color="black") + 
  scale_linetype_manual(values = 
                          c("ags"="solid", "cr"="dotted"),
                        labels = c('ags' = 'Agricultural Surplus',
                                   'cr' = 'Crop Removal')) +
  guides(fill=
           guide_legend(order=1, override.aes = list(pattern='none')), 
         pattern=
           guide_legend(order=2, override.aes = list(fill='grey')), 
         linetype=
           guide_legend(title=NULL)) + 
  scale_x_continuous(breaks=seq(1987,2017,by=5)) + 
  scale_color_manual(values=c("Agricultural Surplus"="black"), 
                     guide = 'none') + 
  theme_bw() + 
  theme(plot.title = element_text(size=9, face="bold"), 
        axis.title.y = element_text(size=9), 
        legend.background = element_rect(fill="white", colour = "black"), 
        legend.title = element_blank())

#create p bar plot
pbar <- ggplot() + 
  ggpattern::geom_bar_pattern(data = pwsin, 
                              aes(x=year,y=value, fill=metric, 
                                  pattern=factor(estimated, levels=c(TRUE,FALSE),
                                                 labels=c('Estimated','Non-Estimated'))), 
                              pattern = ifelse(pwsin$estimated, 'stripe','none'),
                              pattern_color='white',
                              pattern_density=0.05, 
                              pattern_fill = 'white', 
                              pattern_alpha = 0.5, 
                              pattern_spacing=0.025,
                              stat='identity', position='stack', pattern_size=0.05) +
  labs(title = 'Phosphorus (million kg/year)',
       y = "Budget",
       x = " ") +
  scale_fill_manual(values=colorsp) + 
  ggpattern::scale_pattern_manual(name='Estimate Status',
                       values=c('Estimated'='stripe',
                                'Non-estimated'='none')) + 
  geom_line(data=pcrag, 
            aes(x=year,y=value, 
                linetype=metric), 
            linewidth=1.25, color="black") + 
  scale_linetype_manual(values = 
                          c("ags"="solid", "cr"="dotted")) +
  guides(fill=
           guide_legend(order=1, override.aes = list(pattern='none')), 
         pattern=
           guide_legend(order=2, override.aes = list(fill='grey')), 
         linetype=
           guide_legend(title=NULL)) + 
  guides(fill="none", pattern = "none", linetype="none") +
  scale_x_continuous(breaks=seq(1987,2017,by=5)) + 
  scale_color_manual(values=c("Agricultural Surplus"="black"), 
                     guide = 'none') + 
  theme_bw() + 
  theme(plot.title = element_text(size=9, face="bold"), 
        axis.title.y = element_text(size=9), 
        legend.background = element_rect(fill="white", colour = "black"), 
        legend.title = element_blank())

#create nue line plots
nue <- ggplot() + 
  geom_line(data=nue, aes(x=year,y=nue), linewidth=1.25, color='seagreen')+
  theme_bw() + 
  scale_x_continuous(breaks=seq(1987,2017,by=5)) + 
  labs(title = 'Nitrogen Use Efficiency',
       y = "%", 
       x=" ") + 
  theme(plot.title = element_text(size=9, face="bold"), 
        axis.title.x = element_text(size=9), 
        axis.title.y = element_text(size=9)) 

pue <- ggplot() + 
  geom_line(data=pue, aes(x=year,y=nue, lty='Nutrient Use Efficiency'), linewidth=1.25, color="seagreen") + 
  theme_bw() + 
  scale_x_continuous(breaks=seq(1987,2017,by=5)) + 
  labs(title = 'Phosphorus Use Efficiency',
       y = "%", 
       x="Year") + 
  theme(plot.title = element_text(size=9, face="bold"), 
        axis.title.x = element_text(size=9), 
        axis.title.y = element_text(size=9, hjust=0.5),
        legend.background = element_rect(fill="white", colour = "black"), 
        legend.title = element_blank())  +
  guides(fill="none", pattern = "none", linetype="none")

#Create inset map
inset <- ggplot() +
  geom_sf(data = states, color = "grey", fill = 'transparent', lwd = .2) + 
  geom_sf(data = point, size = 3.5, color = "red") + theme_void()

#export final figure
inputs <- patchwork::wrap_plots(nbar, pbar, ncol=1, guides="collect")
nue <- patchwork::wrap_plots(nue, pue, ncol=1, guides="collect")


#

timenni <- patchwork::wrap_plots(nue, inputs, ncol=2)

timenni <- cowplot::plot_grid(
  timenni, inset,
  ncol = 1,
  rel_heights = c(3,1))


ggsave(filename = "LUB_GWMA_SET.png", width = 10, height = 8, units = "in", dpi = 1200)









