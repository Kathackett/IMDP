# loading the data

if (!exists('dat')) {
  dat <- read.xlsx(paste0(my.data.folder, '/figure_dat.xlsx')) |> 
    mutate(TimeOfInspection = convertToDateTime(TimeOfInspection)) |> 
    as_tibble() |> 
      mutate(Start_Time = as.POSIXct(
        Start_Time * 86400,
        origin = "1899-12-30",
        tz = "UTC"
      )) |> 
      mutate(End_Time = as.POSIXct(
        End_Time * 86400,
        origin = "1899-12-30",
        tz = "UTC"
      ))
  
  #|> 
    # mutate(
    #   Station = case_when(
    #     Station == "Penticton Roving" ~ "Penticton",
    #     Station == "Lower Mainland Roving" ~ "Lower Mainland",
    #     TRUE ~ Station
    #   )
    # )
}
dat = dat |> 
  mutate(Station = case_when(Station == "Keremeos (Hwy 3)" ~ "Keremeos",TRUE ~ Station))
if(!exists('dat_all')){
  dat_all = read.xlsx(paste0(my.data.folder,'/figure_dat_all.xlsx')) |>
    mutate(TimeOfInspection = openxlsx::convertToDateTime(TimeOfInspection)) |> 
    as_tibble()#|> 
    # mutate(
    #   Station = case_when(
    #     Station == "Penticton Roving" ~ "Penticton",
    #     Station == "Lower Mainland Roving" ~ "Lower Mainland",
    #     TRUE ~ Station
    #   )
    # )
}

dat_all = dat_all |> 
  mutate(Station = case_when(Station == "Keremeos (Hwy 3)" ~ "Keremeos",TRUE ~ Station))

# In case the 'figure_dat.xlsx' file has been updated past the year we 
# are currently working on (e.g., I've run the imdp code for 2023 but now
# we're looking back at 2022 data... sigh)
if(dat[1,]$Year != my.year){
  dat = dat_all |> 
    dplyr::filter(Year == my.year)
}

if(!exists('dat_hr')){
  dat_hr = read.xlsx(paste0(my.data.folder,'/figure_dat_hr.xlsx')) |> 
    mutate(TimeOfInspection = openxlsx::convertToDateTime(TimeOfInspection)) |> 
    as_tibble()
}
# High risk inspections
dat_hr = dat_hr |> 
  mutate(Station = case_when(Station == "Keremeos (Hwy 3)" ~ "Keremeos",
                             Station == "Penticton Roving" ~ "Penticton",
                             Station == "Sumas Border" ~ "Sumas (Huntington)",
                             Station == "Lower Mainland Roving" ~ "Lower Mainland",
                             Station == "Peace Arch Crossing" ~ "Douglas Crossing",
                             TRUE ~ Station),
         Station = str_replace(Station, "Lower Mainland Roving", "Lower Mainland"))




if(!exists('dat_mf')){
  dat_mf = read.xlsx(paste0(my.data.folder,'/figure_dat_mf.xlsx')) |>
    mutate(TimeOfInspection = openxlsx::convertToDateTime(TimeOfInspection)) |> 
    as_tibble()
}
# Mussel fouled inspections
dat_mf = dat_mf |> 
  mutate(Station = case_when(Station == "Keremeos (Hwy 3)" ~ "Keremeos",
                             Station == "Penticton Roving" ~ "Penticton",
                             Station == "Sumas Border" ~ "Sumas (Huntington)",
                             Station == "Lower Mainland Roving" ~ "Lower Mainland",
                             Station == "Peace Arch Crossing" ~ "Douglas Crossing",
                             TRUE ~ Station),
         Station = str_replace(Station, "Lower Mainland Roving", "Lower Mainland"))

flnro_lookup = read_excel(paste0(my.external.data.folder,"waterbody_name_flrno_region_lookup_table.xlsx")) |> distinct()

abbrev = read_excel(paste0(my.external.data.folder,"Province_States_Abbreviation_Table.xlsx"))

#Lookup table for what the Previous Knowledge of AIS field's codes mean.
ais_know = read_excel(paste0(my.external.data.folder,"Previous_Knowledge_of_AIS_lookup_table.xlsx"))

# Outline of BC
bc_bound = bcmaps::bc_bound() |> st_transform(crs = 4326)

# Apply corrections to some of the Lower Mainland or Penticton Roving station names, using information that was places in the shift start comment field.
dat = dat |>
  mutate(Station = case_when(
    str_detect(Shift_Start_Comment,"[s,S]cheduled") ~ 'Scheduled Inspection (Other Notification)',
    str_detect(Station, "Roving$") & str_detect(Shift_Start_Comment,'97') ~ 'Hwy 97c',
    T ~ Station
  ))
