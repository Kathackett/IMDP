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
  
  # Return only the ID and the new columns, like your old dat_2025_join
  fixed %>%
    transmute(
      Watercraft_Risk_Assessment_ID = as.numeric(Watercraft_Risk_Assessment_ID),
      Inspection.Datetime,
      Inspection.Hour,
      Inspection.Status
    )
}