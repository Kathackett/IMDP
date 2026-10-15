## NOTE: Please use the {imdp} package developed and maintained by me, Chris Madsen
## to clean data for IMDP. There are 3 functions in the {imdp} package
## that cover the functionality of this script, as well as the scripts 
## "02_Combining..." and "03_Cleaning Past..." in the folder
## labelled "01_DataCleaning"



# This script cleans data for the Invasive Mussel Defence Program,
#  which is then used to generate figures used in reports and communications to partners.

# It takes as an input an excel file produced in the 
# script "03_Cleaning Past Watercraft Inspection Data.R", which is a combination
# of all years of inspection data (the latter years coming from metabase .csv files)

# Code developed by Chris Madsen.
# 
# pacman::p_load(
#   readxl,
#   ggpubr,
#   ggrepel,
#   ggExtra,
#   tidyverse,
#   sf,
#   RColorBrewer,
#   openxlsx,
#   scales,
#   lubridate,
#   plotrix)
# 
# #=====================================================
# #                       OPTIONS 
# #=====================================================
# 
# setwd("C:/Users/CMADSEN/Downloads/LocalR/ZQMussels")
# 
# my_opts = read_csv(paste0(str_extract(getwd(),".*ZQMussels[/]?"),"/Options.csv"))
# 
# #Which year should we focus on?
# my.year = my_opts$year
# 
# #Update GIS maps? We probably want this turned on unless you are making fine adjustments to some figures.
# update.gis = FALSE
# 
# setwd(my_opts$base_dir)
# 
# # #Are there any stations we would like to exclude from the excel-type figures?
# # #In 2021, we will exclude:
# # stations.to.include = c("Golden","Radium","Olsen","Yahk",
# #                         "Pacific","Osoyoos","Hwy 97c","Mt. Robson",
# #                         "Keremeos","Greenwood","Dawson Creek","Kaleden")
# # 
# # stations.to.put.aside = c("Scheduled Inspection","Boat Launch - Okanagan",
# #                           "Okanagan",
# #                           "Penticton Roving - Hwy 33", 
# #                           "Penticton Roving - Inspection Event")
# 
# #Data folders
# my.data.folder = paste0(my_opts$zqm_figure_local_folder,"data/")
# my.external.data.folder = paste0(my_opts$remote_spatial_data,"Projects/ZQMussels/data/")
# 
# #Which folder should we put specific output files in?
# my.output.folder = paste0(my_opts$zqm_figure_local_folder,"output/")
# my.external.output.folder = paste0(my_opts$zqm_figure_output_remote_folder,my_opts$year)
# zqm.operations.folder = my_opts$zqm_operations_data_folder
# this.years.report.folder = paste0(my_opts$remote_spatial_data,"Projects/ZQMussels/",my.year," IMDP Final Report/")
# 
# #Where is the mussel-fouled tracking sheet?
# MusselFouledTracker = paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/", my.year," data/2022 mussel fouled boats tracking sheet.xlsx")
# 
# #What is the sheet name of the destination regions for the mussel fouled tracking sheet?
# MF_tracker_sheet = "DestRegions"
# 
# #Where can we find the CBSA notifications excel sheet for this year?
# 
# ## NOTE: CURRENTLY NO CBSA FOR 2022!! ##
# # cbsa_dat = read_excel(paste0(my.external.output.folder,"/GIS Maps and Excel Figures/2021 COS inbox notifications.xlsx"),
# #                       sheet = "Filtered list")
# 
# #=====================================================
# #                     END OF OPTIONS
# #=====================================================
# 
# #If there is no folder for excel figures for the target year in the I: drive, make it now.
# if(!dir.exists(paste0(my.external.output.folder,"/GIS Maps and Excel Figures/ExcelFigures"))){
#   dir.create(paste0(my.external.output.folder,"/GIS Maps and Excel Figures/ExcelFigures"))
# }
# 
# 
# # import_data
# #Read in data (cleaned). These data do not have any of the "test" records from metabase.
# dat_all = read_excel(paste0(zqm.operations.folder,"Watercraft Inspection Data/Multiyear data/WatercraftInspectionData_AllYears_Selected_Columns.xlsx"),
#                      col_type = "text")
# 
# #Get Station coordinates, might be unnecessary.
# stations = read_sf(paste0(my.external.data.folder,"inspection_stations.gpkg"))
# 
# #Lookup table of provinces/territories and U.S. States (full names and abbreviations)
# abbrev = read_excel(paste0(my.external.data.folder,"Province_States_Abbreviation_Table.xlsx"))
# 
# #Lookup table for named waterbodies in BC and which FLNRO fisheries region they can be found in. 
# flnro_lookup = read_excel(paste0(my.external.data.folder,"waterbody_name_flrno_region_lookup_table.xlsx")) %>% distinct()
# 
# #Lookup table for what the Previous Knowledge of AIS field's codes mean.
# ais_know = read_excel(paste0(my.external.data.folder,"Previous_Knowledge_of_AIS_lookup_table.xlsx"))
# 
# # data_cleaning
# #Fix some inconsistencies in station name spelling.
# dat_all = dat_all %>% 
#   mutate(Station = case_when(
#     Station == "Cascade Border" ~ "Cascade",
#     str_detect(Station, "Cutts") ~ "Cutts (Hwy 93)",
#     Station == "Hwy 93 Columbia Lake" ~ "Cutts (Hwy 93)",
#     Station == "Olsen (Hwy 3)" ~ "Olsen",
#     Station == "Valemount" ~ "Mt. Robson",
#     str_detect(Station, "Pacific") ~ "Pacific",
#     Station == "Keremeos (Hwy 3)" ~ "Keremeos",
#     str_detect(Station, "^Schedule") ~ "Scheduled Inspection",
#     str_detect(Station, "Covid") ~ "COVID Border",
#     str_detect(Station, "Penticton") ~ "Penticton Roving",
#     str_detect(Station, "Okanagan") ~ "Okanagan",
#     T ~ Station
#   ))
# 
# #We need to clean up the field(s) that talk about which province/state a given inspection is from.
# dat_all = dat_all %>% 
#   #Make the name shorter for now...
#   rename(Source = Previous_Waterbody_1_Province_Or_State) %>% 
#   #Replace NA in Source with previous major city, if we have it. If not, use the province_code of the boat. If we don't have any of those, or if the Source field wasn't blank, keep it as it was.
#   mutate(NewSource = case_when(
#     (Source == "Unknown" | is.na(Source)) & !is.na(Previous_Major_City) & Previous_Major_City != "None" ~ Previous_Major_City,
#     (Source == "Unknown" | is.na(Source)) & (is.na(Previous_Major_City) | Previous_Major_City == "None") ~ Province_Code,
#     T ~ Source)) %>% 
#   #If we replaced the Source field with the closest major city, we need to pull the province/state name out of the string.
#   mutate(NewSource = case_when(
#     str_detect(NewSource, ",.*,") ~ str_extract(NewSource, "(?<=, ).*(?=,)"),
#     T ~ NewSource)) %>%
#   #Finally, change the long form (e.g. "Alberta") to abbreviated form for names (e.g. "AB").
#   left_join(abbrev %>% rename(NewSource = Province_or_State)) %>% 
#   mutate(NewSource = coalesce(Abbrev,NewSource)) %>% 
#   dplyr::select(-Abbrev,-Source) %>% 
#   rename(Previous_Waterbody_1_Province_Or_State = NewSource)
# 
# 
# # calculate_total_boats_and_convert_to_datetime
# #Add total boats (multiply inspections by boat type counters)
# dat_all = dat_all %>% 
#   mutate(TotalBoats = as.numeric(Non_Motorized_Counter) + 
#            as.numeric(Simple_Counter) + 
#            as.numeric(Complex_Counter) + 
#            as.numeric(Very_Complex_Counter)) %>% 
#   mutate(TotalBoats = replace(TotalBoats, TotalBoats == 0, 1))
# 
# #Convert start_time, end_time and TimeOfInspection to datetime format.
# dat_all$Start_Time = convertToDateTime(dat_all$Start_Time)
# dat_all$End_Time = convertToDateTime(dat_all$End_Time)
# dat_all$TimeOfInspection = convertToDateTime(dat_all$TimeOfInspection)
# 
# dat_all = dat_all %>% 
#   mutate(Shift_hours = as.numeric(End_Time - Start_Time)/3600)
# 
# # correct_negative_shift_length_data
# dat_all = dat_all %>% 
#   mutate(Shift_hours = ifelse(Shift_hours < 0, -Shift_hours, Shift_hours))
# 
# # add_shift_unique_identifier
# #Add a unique identifier for each shift. I'm using combinations of start and end time
# #so hopefully they are unique.
# dat_all = dat_all %>% 
#   group_by(Start_Time, End_Time) %>% 
#   mutate(Shift_ID = cur_group_id()) %>% 
#   select(Year,Shift_ID,everything()) %>% 
#   arrange(Shift_ID) %>% 
#   ungroup()
# 
# # data_for_maps
# ### Make Data for GIS maps
# if(update.gis == T){
#   #First, we need to get total inspections, HR inspections, and mussel-fouled inspections by source location.
#   if(!dir.exists(my.data.folder)) dir.create(my.data.folder)
#   
#   setwd(my.data.folder)
#   
#   canada = raster::getData(country = "CAN", level = 1)   
#   usa = raster::getData(country = "USA", level = 1)
#   mex = raster::getData(country = "MEX", level = 1)
#   
#   canada = st_as_sf(canada)
#   usa = st_as_sf(usa)
#   mex = st_as_sf(mex)
#   
#   na_sf = canada %>% 
#     bind_rows(usa, mex)
#   
#   na_sf = st_simplify(na_sf)
#   
#   write_sf(na_sf, 
#            paste0(my_opts$remote_spatial_data,"shared_data_sets/northamerica.gpkg"))
#   
#   na_sf = na_sf %>% 
#     filter(!NAME_1 %in% c("Coahuila","Hidalgo"))
#   
#   #Cleaning up previous waterbody 1 prov/state field...
#   source_dat = dat_all %>%
#     rename(ABBR = Previous_Waterbody_1_Province_Or_State) %>% 
#     mutate(ABBR = case_when(
#       ABBR == "unknown" ~ "Unknown",
#       ABBR == "Calgary" ~ "AB",
#       ABBR == "Mexico" ~ "BN",
#       ABBR == "Wi" ~ "WI",
#       ABBR == "Radium" ~ "BC",
#       ABBR == "Sk" ~ "SK",
#       ABBR == "SA" ~ "SK",
#       ABBR == "Ab" ~ "AB",
#       ABBR == "QB" ~ "QC",
#       ABBR == "Yukon Territory" ~ "YT",
#       ABBR == "YK" ~ "YT",
#       ABBR == 'Yukon Territory' ~ "YT",
#       ABBR == 'Sonora' ~ 'SO',
#       ABBR == 'Baja California Sur' ~ "BS",
#       ABBR == "Northwest Territories" ~ "NT",
#       ABBR == "NWT" ~ "NT",
#       ABBR == "W" ~ "WA",
#       ABBR == "LI" ~ "LA",
#       ABBR == "KA" ~ "KS",
#       T ~ ABBR
#     )) %>% 
#     filter(Year == my.year)
#   
#   #Join total insp, HR insp, MS insp, and commercially hauled insp, to north america shapefile.
#   insp_by_source = na_sf %>% 
#     mutate(ABBR = str_remove(HASC_1, "[A-Z]{2}\\.")) %>% 
#     left_join(source_dat %>% count(ABBR) %>% rename(TotalInsp = n)) %>% 
#     left_join(source_dat %>% 
#                 filter(High_Risk_AIS_Ind == T,
#                        Clean_Drain_Dry_After_Inspection_Ind == F) %>% 
#                 count(ABBR) %>% 
#                 rename(HRInsp = n)) %>% 
#     left_join(source_dat %>% 
#                 filter(MusselsFound_Ind == T) %>% 
#                 count(ABBR) %>%
#                 rename(MFInsp = n)) %>% 
#     left_join(source_dat %>% 
#                 filter(Commercially_Hauled_Ind == T) %>% 
#                 count(ABBR) %>%
#                 rename(CHInsp = n)) %>% 
#     st_transform(crs = 4326) %>% 
#     st_centroid()
#   
#   write_sf(insp_by_source,
#            paste0(this.years.report.folder,"data/spatial/Inspections_by_source_centroid.gpkg"))
#   
#   #Make label shapefiles...
#   #No inspections at all.
#   blank_state_prov_labels = na_sf %>% 
#     mutate(ABBR = str_remove(HASC_1, "[A-Z]{2}\\.")) %>% 
#     filter(!ABBR %in% all_of(insp_by_source %>% 
#                                filter(!is.na(TotalInsp)) %>% 
#                                pull(ABBR))) %>% 
#     filter(NAME_0 != "Mexico") %>% 
#     st_centroid() %>% 
#     write_sf(paste0(this.years.report.folder,"data/spatial/No_inspection_states_labels.shp"))
#   
#   #No High-risk inspections.
#   blank_state_prov_labels = na_sf %>% 
#     mutate(ABBR = str_remove(HASC_1, "[A-Z]{2}\\.")) %>% 
#     filter(!ABBR %in% all_of(insp_by_source %>% 
#                                filter(!is.na(HRInsp)) %>% 
#                                pull(ABBR))) %>% 
#     filter(NAME_0 != "Mexico") %>% 
#     st_centroid() %>% 
#     write_sf(paste0(this.years.report.folder,"data/spatial/No_HR_inspection_states_labels.shp"))
#   
#   #No Mussel-fouled inspections.
#   blank_state_prov_labels = na_sf %>% 
#     mutate(ABBR = str_remove(HASC_1, "[A-Z]{2}\\.")) %>% 
#     filter(!ABBR %in% all_of(insp_by_source %>% 
#                                filter(!is.na(MFInsp)) %>% 
#                                pull(ABBR))) %>% 
#     filter(NAME_0 != "Mexico") %>% 
#     st_centroid() %>% 
#     write_sf(paste0(this.years.report.folder,"data/spatial/No_MF_inspection_states_labels.shp"))
#   
#   #No Commercially-hauled inspections.
#   blank_state_prov_labels = na_sf %>% 
#     mutate(ABBR = str_remove(HASC_1, "[A-Z]{2}\\.")) %>% 
#     filter(!ABBR %in% all_of(insp_by_source %>% 
#                                filter(!is.na(CHInsp)) %>% 
#                                pull(ABBR))) %>% 
#     filter(NAME_0 != "Mexico") %>% 
#     st_centroid() %>% 
#     write_sf(paste0(this.years.report.folder,"data/spatial/No_CH_inspection_states_labels.shp"))
#   
#   # And also to write out inspection data joined to stations.
#   insp_by_station = stations %>% 
#     mutate(map_label = case_when(
#       map_label == "Keremeos (Hwy 3)" ~ "Keremeos",
#       map_label == "Pacific / Surrey" ~ "Pacific",
#       T ~ map_label
#     )) %>% 
#     filter(map_label != "Penticton Roving") %>% 
#     rename(Station = map_label) %>% 
#     left_join(dat_all %>% group_by(Station,Year,Province_Code) %>% count(name = "TotalInspections")) %>%
#     left_join(dat_all %>% group_by(Station,Year,Province_Code) %>% filter(MusselsFound_Ind == T) %>% count(name = "MusselFouled")) %>%
#     left_join(dat_all %>% group_by(Station,Year,Province_Code) %>% filter(High_Risk_AIS_Ind == T) %>% count(name = "HighRisk")) %>%
#     left_join(dat_all %>% group_by(Station,Year,Province_Code) %>% 
#                 filter(Province_Code != "Unknown") %>% 
#                 summarise(number_in_provs = n()) %>% 
#                 summarise(number_of_provs = n())) %>% 
#     distinct() %>%
#     group_by(Station,Year) %>% 
#     mutate(row.number = row_number()) %>% 
#     select(Year, everything()) %>% 
#     select(-Province_Code,-row.number) %>% 
#     group_by(Year,Station) %>% 
#     mutate(TotalInspections = sum(TotalInspections, na.rm=T),
#            MusselFouled = sum(MusselFouled, na.rm=T),
#            HighRisk = sum(HighRisk, na.rm=T)) %>% 
#     distinct() %>% 
#     #We're missing a couple rows of coordinates... let's get them.
#     st_transform(crs = 4326)
#   
#   write_sf(insp_by_station,
#            paste0(this.years.report.folder,"data/spatial/Inspections_Summarised_at_Station_level.gpkg"))
#   
#   #And also to write out inspections (total, HR, MF) by destination location (i.e. waterbody)
#   #Just for 2021!
#   #Here are the lake shapefiles for which we have at least 1 inspection from 2015 - present.
#   #lakes = read_sf("W:/CMadsen/SpatialData/summarized_bc_waterbodies_same_gnis_joined.shp")
#   
#   # summaries = read_excel("I:/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/Multiyear data/InspectionSummaries_GNISNA_WATERSH_Year.xlsx") %>% 
#   #   filter(Year == my.year)
#   #   
#   # lakes = lakes %>% 
#   #   left_join(summaries) %>% 
#   #   filter(!is.na(TotalInspections)) %>% 
#   #   st_centroid()
#   # 
#   # write_sf(lakes, "W:/CMadsen/2021 Invasive Mussel Program Final Report Maps/data/ShapeLayers/Inspections by dest waterbodies for target year.shp")
# }
# 
# # add_lat_long_for_closest_city_fields}
# #Combine the two fields containing closest city info: "Destination_Waterbody_1_Closest_City" and "Destination_Major_City".
# dat_all = dat_all %>% 
#   mutate(CityName = case_when(
#     #When we have info for the closest city field, use that.
#     !is.na(Destination_Waterbody_1_Closest_City) & Destination_Waterbody_1_Closest_City != "Other" ~ Destination_Waterbody_1_Closest_City,
#     #If we lack closest city but have info in 'Destination_Major_City' (more coarse detail), use that.
#     is.na(Destination_Waterbody_1_Closest_City) | Destination_Waterbody_1_Closest_City == "Other" ~ Destination_Major_City,
#     T ~ "Unknown"
#   )) %>% 
#   mutate(CityName = replace_na(CityName, "Unknown")) %>% 
#   mutate(CityName = str_remove_all(CityName, ",.*"))
# 
# #For inspections that lack a destination waterbody name, use the BC geocoder to get lat/long coordinates for city names. If the dest city is outside BC, don't get lat/long for those, in case there's a tiny town in BC with the same name and we incorrectly get the coordinates for that town. 
# city_names_for_coords = tibble(dat_all %>% 
#                                  filter(is.na(Destination_Waterbody_1_Name)) %>% 
#                                  filter(CityName != "Unknown") %>% 
#                                  #Get rid of any that are outside BC. We'll flag those when we make the DestRegion field.
#                                  filter(str_detect(Destination_Major_City, ", British Columbia")) %>% 
#                                  select(CityName) %>% 
#                                  distinct()) 
# 
# #Create new variables that we will fill with the loop below.
# city_names_for_coords$lon = 0
# city_names_for_coords$lat = 0
# 
# #This loop uses the BC geocoder to find the most likely coordinates
# # for each of the unique place names.
# for(i in 1:nrow(city_names_for_coords)){
#   # Pull out place name.
#   my.name = city_names_for_coords[i,]$CityName
#   #Clean up names. Remove anything in brackets.
#   my.name = str_remove_all(my.name, " \\(.*\\)")
#   #Add spaces to names.
#   my.name = str_replace_all(my.name, " ", "%20")
#   
#   url = paste0('https://geocoder.api.gov.bc.ca/addresses.json?addressString=',
#                my.name,'&maxResults=1&outputSRS=4326')
#   
#   my.coords = jsonlite::fromJSON(url)$features$geometry %>% 
#     summarise(lon = str_extract(coordinates, "(?<=c\\().*(?=\\,)"),
#               lat = str_extract(coordinates, "(?<=\\,).*(?=\\))"))
#   
#   city_names_for_coords[i,]$lon = as.numeric(my.coords$lon)
#   city_names_for_coords[i,]$lat = as.numeric(my.coords$lat)
# }
# 
# #Find out which FLRNO region each of these city coords fall into.
# flnro_regions = read_sf(paste0(my_opts$remote_spatial_data,"shared_data_sets/FLNRO_Fishing_Boundaries.shp"))
# 
# city_names_for_coords = city_names_for_coords %>%
#   st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
#   st_transform(crs = 3005) %>% 
#   st_join(flnro_regions, st_intersects)
# 
# #Join those coordinates to dat_all. For inspections with Unknown nearest city, those remain blank for lat and long.
# dat_all = dat_all %>% 
#   left_join(city_names_for_coords %>% 
#               st_drop_geometry() %>% 
#               rename(City_region_g = REGION_G,
#                      City_region_n = REGION_N))
# 
# # Subset multiyear dataset for this years data}
# #Set aside the records for mussel-fouled boats.
# dat_mf = dat_all %>% filter(MusselsFound_Ind == T)
# 
# #And set aside the records for high-risk boats.
# dat_hr = dat_all %>% filter(High_Risk_AIS_Ind == T) %>% 
#   filter(Clean_Drain_Dry_After_Inspection_Ind == F | Year < 2020)
# 
# dat = dat_all %>% filter(Year == my.year)
# 
# # sort_into_destination_regions
# #We also need to pull together the destination waterbody region from a couple columns.
# dat[dat$DestNotBC == 'FALSE',]$DestNotBC = NA
# dat[dat$OceanBoat == 'FALSE',]$OceanBoat = NA
# dat[dat$DryStorage == 'FALSE',]$DryStorage = NA
# 
# dat = dat %>% 
#   #Combine the 2 categories of destinations that aren't a waterbody.
#   mutate(Dest_not_wb = coalesce(DestNotBC,DryStorage)) %>% 
#   mutate(DestRegion = case_when(
#     #If we have the destination name, use that.
#     !is.na(Destination_Waterbody_1_Name) ~ Destination_Waterbody_1_Name,
#     #If we don't have name, but have city, we'll use the city region we found above.
#     is.na(Destination_Waterbody_1_Name) & !is.na(City_region_n) ~ City_region_n,
#     #If we have no name or city, and either of the categories flagged (e.g. "DestNotBC"), use those.
#     Dest_not_wb == T ~ "Outside BC",
#     DryStorage == T ~ "Dry Storage",
#     T ~ "Unknown"))
# 
# #Same thing for our little mussel-fouled table...
# dat_mf[dat_mf$DestNotBC == 'FALSE',]$DestNotBC = NA
# dat_mf[dat_mf$OceanBoat == 'FALSE',]$OceanBoat = NA
# dat_mf[dat_mf$DryStorage == 'FALSE',]$DryStorage = NA
# 
# dat_mf = dat_mf %>% 
#   #Combine the 2 categories of destinations that aren't a waterbody.
#   mutate(Dest_not_wb = coalesce(DestNotBC,DryStorage)) %>% 
#   mutate(DestRegion = case_when(
#     #If we have the destination name, use that.
#     !is.na(Destination_Waterbody_1_Name) ~ Destination_Waterbody_1_Name,
#     #If we don't have name, but have city, we'll use the city region we found above.
#     is.na(Destination_Waterbody_1_Name) & !is.na(City_region_n) ~ City_region_n,
#     #If we have no name or city, and either of the categories flagged (e.g. "DestNotBC"), use those.
#     Dest_not_wb == T ~ "Outside BC",
#     DryStorage == T ~ "Dry Storage",
#     T ~ "Unknown"))
# 
# #And same thing for high-risk inspections.
# #Same thing for our little mussel-fouled table...
# dat_hr[dat_hr$DestNotBC == 'FALSE',]$DestNotBC = NA
# dat_hr[dat_hr$OceanBoat == 'FALSE',]$OceanBoat = NA
# dat_hr[dat_hr$DryStorage == 'FALSE',]$DryStorage = NA
# 
# dat_hr = dat_hr %>% 
#   #left_join(dat_hr %>% 
#   #Combine the 2 categories of destinations that aren't a waterbody.
#   mutate(Dest_not_wb = coalesce(DestNotBC,DryStorage)) %>% 
#   mutate(DestRegion = case_when(
#     #If we have the destination name, use that.
#     !is.na(Destination_Waterbody_1_Name) ~ Destination_Waterbody_1_Name,
#     #If we don't have name, but have city, we'll use the city region we found above.
#     is.na(Destination_Waterbody_1_Name) & !is.na(City_region_n) ~ City_region_n,
#     #If we have no name or city, and either of the categories flagged (e.g. "DestNotBC"), use those.
#     Dest_not_wb == T ~ "Outside BC",
#     DryStorage == T ~ "Dry Storage",
#     T ~ "Unknown"))
# 
# #select(Watercraft_Risk_Assessment_ID,DestRegion))
# 
# setwd(paste0(my_opts$zqm_figure_local_folder,'data/'))
# 
# # Output data that will be used to make figures.
# openxlsx::write.xlsx(dat, "figure_dat.xlsx", overwrite = T)
# openxlsx::write.xlsx(dat_all, "figure_dat_all.xlsx", overwrite = T)
# openxlsx::write.xlsx(dat_hr, "figure_dat_hr.xlsx", overwrite = T)
# openxlsx::write.xlsx(dat_mf, "figure_dat_mf.xlsx", overwrite = T)
