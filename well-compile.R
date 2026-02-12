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
#install.packages("cdlTools")
library(cdlTools)
library(janitor)
library(here)

setwd("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/dw-nitrate/RET")

# RET data and wells ===========================================================

# wells <- read_delim("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/dw-nitrate/well_logs.txt")
# # 
# welp <- wells |>
#   drop_na(use_domestic)
# 
# welp <- welp |>
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4269) |>
#   separate(col = complete_date,
#            into = c("Month","Date","Year"),
#            sep = "/",
#            remove = TRUE)
#  st_transform(crs = 5072)
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

# RET data =====================================================================

RET <- read_csv("RET-excel.csv")

RET <- RET |>
  drop_na(WellLongitude, WellLatitude) |>
  filter((WellLongitude < (-100))  & (WellLatitude > 41.9)) |>
  st_as_sf(coords = c("WellLongitude", "WellLatitude"), crs = 4269) 

RET <- RET |>
  select(SampleId, Name, Date, Year, ResultNumeric, WellCity, WellState, WellZip, WellCounty, geometry) |>
  rename(AnalyteName = Name,
         Result = ResultNumeric) |>
  mutate(Year = as.character(Year))

# RET updated data =============================================================

RET_2025 <- readxl::read_excel('RET_2025.xlsx')

RET_2025 <- RET_2025 |>
  drop_na(WellLongitude, WellLatidude) |>
  filter((WellLongitude < (-100))  & (WellLatidude > 41.9)) |>
  st_as_sf(coords = c("WellLongitude", "WellLatidude"), crs = 4269) 

RET_2025 <- RET_2025 |>
  select(SampleId, AnalyteName, `Sample Date`, Result, WellCity, WellState, WellZip, WellCounty, geometry) |>
  rename(Date = `Sample Date`) |>
  mutate(Year = substr(Date, 1, 4),
         Date = as.character(Date),
         Result = as.numeric(Result),
         WellZip = as.numeric(WellZip)) 

# Bind rows

RET_all <- bind_rows(RET, RET_2025) |>
  distinct()

# RET_summary <- as.data.frame(table(RET$Year))
# 
# RET_summary <- RET_summary |>
#   rename(year = Var1,
#          RET_total = Freq)

# county level aggregation =====================================================

RET_county <- RET_all |>
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

# zip code level aggregation ===================================================

RET_zip <- RET_all |>
  group_by(WellZip, Year) |>
  summarize(count = n())

RET_zip <- RET_zip |>
  st_drop_geometry() |>
  pivot_wider(id_cols = WellZip,
              names_from = Year,
              values_from = count,
              values_fill = 0)

RET_zip <- RET_zip |>
  rename_with(~ paste0("RET_", .), -WellZip)

# ggplot(RET, aes(color = WellCounty)) +
#   geom_sf(size = 1)

# FRED pending sold data =======================================================

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

# county level pending data ====================================================

# rdc_county <- read_csv('RDC_Inventory_Core_Metrics_County_History.csv')
# 
# rdc_county <- rdc_county |>
#   separate(col = county_name,
#            into = c("county", "state"),
#            sep = ", ",
#            remove = TRUE) |>
#   filter(state == 'or')
# 
# # oregon county fips to names 
# 
# fips_to_name <- read_csv('fips_to_name.csv')
# 
# fips_to_name <- fips_to_name |>
#   filter(`FIPS State and County Codes` > 40000) |>
#   separate(col = `Geographic area name`,
#            into = c("county", "name"),
#            sep = " County",
#            remove = TRUE) |>
#   rename(county_fips = `FIPS State and County Codes`) |>
#   select(c(county_fips, county))

# oregon domestic well data, Andrew Murray

#domes_well <- read.csv('Block_Groups.csv')
domes_well <- read.csv('Blocks.csv')

# select county identifier from GEOID / block group
domes_well <- domes_well |>
  mutate(county_fips = str_sub(as.character(GEOID), 1 , 5),
         wells_2020_num = as.numeric(Wells.2020),
         wells_1990_num = as.numeric(X1990.Wells)) |>
  relocate(GEOID, county_fips, wells_2020_num, wells_1990_num) |>
  drop_na(wells_2020_num, wells_1990_num) |> 
  rename(census_block = GEOID)
  
# summarize to determine number of wells in county
# well_county <- domes_well |>
#   group_by(county_fips) |>
#   summarise(AM_wells.2020 = sum(wells_2020_num),
#             AM_wells.1990 = sum(wells_1990_num)) 

# rdc_mini <- rdc_county |>
#   select(month_date_yyyymm, county_fips, county, pending_listing_count,
#          active_listing_count, quality_flag) |>
#   mutate(year = str_sub(as.character(month_date_yyyymm),1,4))|>
#   relocate(year) |>
#   select(-month_date_yyyymm)
# 
# rdc_mini <- rdc_mini |>
#   group_by(county_fips, year) |>
#   summarise(pending_listing = sum(pending_listing_count),
#             active_listing = sum(active_listing_count)) |>
#   pivot_wider(id_cols = county_fips,
#               names_from = year,
#               values_from = c(pending_listing, active_listing),
#               values_fill = 0) |>
#   mutate(across(everything(), ~replace_na(.x, 0)))
# 
# rdc_mini <- rdc_mini |>
#   left_join(well_county, by = 'county_fips')
# 
# # join to fips to name
# 
# rdc_mini <- fips_to_name |>
#   mutate(county_fips = as.character(county_fips)) |>
#   left_join(rdc_mini, by = 'county_fips')

# add ret data

# RET_county <- RET_county |>
#   rename(county = WellCounty)
# 
# county_all <- RET_county |>
#   left_join(rdc_mini, by = 'county') |>
#   relocate(county_fips) |>
#   drop_na(county)

# housing unit information

# housing_2024 <- readxl::read_excel('CO-EST2024-HU-41.xlsx', col_names = TRUE, skip = 1)
# 
# housing_2024 <- housing_2024 |>
#   mutate(County = str_remove(County, "^\\."),
#          County = str_remove(County, " County, Oregon")) |>
#   rename(county = County)
# 
# housing_2020 <- readxl::read_excel('co-est2020int-hu-41.xlsx', col_names = TRUE, skip = 1)
# 
# housing_2020 <- housing_2020 |>
#   mutate(county = str_remove(county, "^\\."),
#          county = str_remove(county, " County, Oregon")) |>
#   rename_with(~ paste0("hu_", .), -c(county, base_2010))
# 
# housing_2010 <- readxl::read_excel('hu-est00int-02-41.xls', col_names = TRUE)
# 
# housing_2010 <- housing_2010 |>
#   mutate(county = str_remove(county, "^\\."),
#          county = str_remove(county, " County")) |>
#   rename_with(~ paste0("hu_", .), -county)
# 
# housing <- housing_2010 |>
#   left_join(housing_2020, by = 'county') |>
#   left_join(housing_2024, by = 'county') |>
#   drop_na()
# 
# # joining 
# 
# county_all <- county_all |>
#   left_join(housing, by = 'county') |>
#   select(!(contains('active')))

# parcel data ==================================================================

census_blocks <- read_csv('CWS_Boundaries_Latest/Census_Tables/Blocks_V_2_1.csv')

census_blocks <- census_blocks |>
  mutate(state = substr(PWSID, 1, 2))

or_blocks <- census_blocks |>
  filter(state == 'OR')

# reset wd for the next couple lines
setwd("C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/dw-nitrate/RET/Extract_Regrid/Extract_Regrid")

# Load Regrid County File Paths
paths <- read_csv(here("Data/file_links.csv"))|>
  filter(state == "OR")

# Create temporary folder to download spatial data to
temp.path <- here("Oregon_Temp")
dir.create(temp.path, showWarnings = FALSE)

# Load CWS Blocks (Download here: https://github.com/USEPA/ORD_SAB_Model/tree/main/Version_History/2_1/Census_Tables)
cws_blocks <- read_csv(here("Data/Blocks_V_2_1.csv"))

# Failed data frame to catch anything that fails
failed <- data.frame()

# Iterate over the county files
for(n in 1:nrow(paths)){
  # Download Zip File
  dir.create(paste0(temp.path,"/zip"), showWarnings = FALSE)
  download.file(paths$link[n], paste0(temp.path,"/zip/temp.gdb.zip"), method = "curl", quiet = TRUE, mode = "w",
                cacheOK = TRUE,
                extra = getOption("download.file.extra"),
                headers = NULL)
  
  tryCatch({
    # Unzip
    dir.create(paste0(temp.path,"/unzip.gdb"), showWarnings = FALSE)
    zipF <- paste0(temp.path,"/zip/temp.gdb.zip")
    outDir <- paste0(temp.path,"/unzip.gdb")
    unzip(zipF, exdir = outDir)
    
    # Load data
    sf <- st_read(paste0(temp.path,"/unzip.gdb", "/", tolower(paths$state[n]), "_", tolower(paths$county[n]), ".gdb"), quiet = TRUE) |>
      st_transform(st_crs(5070)) |>
      st_make_valid()
    
    # Save spatial file
    #st_write(sf, here("County_Parcels.gpkg"), layer = paste0(paths$county[n],"_",paths$state[n]))
    
    
    
    
    # Drop Geometry and filter to data
    ## !! Keep in mind that different counties can have different data columns. Only some columns are nationally consistent. !!
    df <- sf|>
      st_drop_geometry()|>
      # Filter out CWS Census Blocks
      filter(!census_block %in% cws_blocks$GEOID20)|>
      # Select relevant columns (Double check these)
      select(census_block,lbcs_activity,lbcs_activity_desc,lbcs_function,lbcs_function_desc,lbcs_structure,lbcs_structure_desc,
             lbcs_site,lbcs_site_desc,lbcs_ownership,lbcs_ownership_desc,ll_bldg_count,saledate,saleprice)
    
    # Save text file
    write_csv(df, paste0(here("Data/County_Tables"),"/",paths$county[n],"_",paths$state[n],".csv"))
    
    # Delete temporary files
    unlink(paste0(temp.path,"/zip"), recursive = TRUE)
    unlink(paste0(temp.path,"/unzip.gdb"), recursive = TRUE)
    
    #setTxtProgressBar(pb, n)
    print(paste0("Completed ",paths$NAME[n]," (",paths$GEOID[n],") at: ",round(Sys.time())))
  }, error = function(e) {
    # Append failed county to the 'failed' data frame
    failed <- rbind(failed, paths[n, ])
    message(paste("Error processing county:", paths$county[n], "in state:", st, "-", e$message))
  })
  
  
}



unlink(temp.path, recursive = TRUE)

print(paste0("Completed ", st, " at: ", round(Sys.time())))


# Display failed counties
print("Failed counties:")
print(failed)

# Save failed counties
if(nrow(failed)>0){
  write_csv(failed,here("failed.csv"),delim = ",")
}

# load in all county csvs

file_list <- list.files(path = "C:/Users/mreyno04/OneDrive - Environmental Protection Agency (EPA)/Profile/REPOS/dw-nitrate/RET/Extract_Regrid/Extract_Regrid/Data/County_Tables", 
                        pattern = "\\.csv$", 
                        full.names = TRUE)

county_parcel <- read_csv(file_list) |>
  filter(lbcs_ownership == '1100' & !is.na(saledate)) 

cws_blocks <- cws_blocks |>
  mutate(state = substr(PWSID, 1, 2)) |>
  filter(state == 'OR')




  
























  
