library(tidyverse)
library(dplyr)
library(stringr)
library(openxlsx)


## this file is in the Zebra Quagga mussel project folder.
all_data = read.csv("./02_IMDP_Figure_Generator/data/figure_dat_all.csv")
my.external.data.folder <- "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/Projects/ZQMussels/data/"
ais_know = openxlsx::read.xlsx(paste0(my.external.data.folder,"Previous_Knowledge_of_AIS_lookup_table.xlsx"))


selected_data = all_data |> 
  select(Year, TimeOfInspection, Start_Time, End_Time,
         Watercraft_Risk_Assessment_ID, MusselsFound_Ind, High_Risk_AIS_Ind, Station,
         Watercraft_Has_Drainplugs_Ind, Drainplug_Removed_at_Inspection_Ind, 
         Previous_AIS_Knowledge_Ind, Previous_AIS_Knowledge_Source_Code_ID,
         Previous_Inspection_Ind, Previous_Inspection_Source_Name,
         Commercially_Hauled_Ind,
         Previous_Waterbody_1_Name,
         Previous_Waterbody_1_Closest_City, Previous_Waterbody_1_Province_Or_State,
         Destination_Waterbody_1_Name, Destination_Waterbody_1_Closest_City, Destination_Waterbody_1_Province_Or_State)

selected_data = selected_data |>
  left_join(
    ais_know |>
      select(
        Previous.Ais.Knowledge.Source.Code.ID,
        Description
      ),
    by = c(
      "Previous_AIS_Knowledge_Source_Code_ID" =
        "Previous.Ais.Knowledge.Source.Code.ID"
    )
  ) |>
  mutate(
    Previous_AIS_Knowledge_Source_Code_ID = Description
  ) |>
  select(-Description)

tz_local <- "America/Vancouver"

excel_to_dt <- function(x) {
  x <- as.character(x)
  secs <- rep(NA_real_, length(x))
  
  is_serial <- !is.na(x) & str_detect(x, "^[0-9]+\\.?[0-9]*$")
  secs[is_serial] <- round(as.numeric(x[is_serial]) * 86400)
  out <- as.POSIXct(secs, origin = "1899-12-30", tz = "UTC")
  
  is_text <- !is.na(x) & !is_serial
  out[is_text] <- ymd_hms(x[is_text], tz = "UTC", quiet = TRUE)
  
  force_tz(out, tzone = tz_local)
}

fix_2025_times <- function(dat, buffer_minutes = 15) {
  
  fixed <- dat %>%
    filter(Year == 2025) %>%
    mutate(
      Start.Time = excel_to_dt(Start_Time),
      End.Time   = excel_to_dt(End_Time),
      Insp_raw   = excel_to_dt(
        coalesce(as.character(TimeOfInspection), as.character(Start_Time))
      ),
      
      Insp_alt = with_tz(force_tz(Insp_raw, "UTC"), tz_local),
      
      ok_raw = Insp_raw >= (Start.Time - minutes(buffer_minutes)) &
        Insp_raw <= (End.Time + minutes(buffer_minutes)),
      ok_alt = Insp_alt >= (Start.Time - minutes(buffer_minutes)) &
        Insp_alt <= (End.Time + minutes(buffer_minutes)),
      
      Inspection.Datetime = case_when(
        ok_raw ~ Insp_raw,
        ok_alt ~ Insp_alt,
        TRUE   ~ Insp_raw
      ),
      Inspection.Status = case_when(
        ok_raw ~ "ok",
        ok_alt ~ "converted_from_utc",
        TRUE   ~ "outside_shift"
      ),
      Inspection.Hour = hour(Inspection.Datetime)
    )
  
  fixed %>%
    transmute(
      Watercraft_Risk_Assessment_ID = as.numeric(Watercraft_Risk_Assessment_ID),
      Inspection.Datetime,
      Inspection.Hour,
      Inspection.Status
    )
}


fixed_times <- fix_2025_times(selected_data)


selected_data = selected_data |> 
  left_join(
    fixed_times |> 
      rename(TimeOfInspection = Inspection.Datetime) |> 
      select(Watercraft_Risk_Assessment_ID, TimeOfInspection),
    by = "Watercraft_Risk_Assessment_ID",
    suffix = c("", ".new")
  ) %>%
  mutate(
    TimeOfInspection = lubridate::ymd_hms(TimeOfInspection),
    TimeOfInspection = coalesce(TimeOfInspection.new, TimeOfInspection)
  ) %>%
  select(-TimeOfInspection.new)


write.csv(selected_data,"./data/reduced_inspections_all_years.csv", row.names = FALSE)
