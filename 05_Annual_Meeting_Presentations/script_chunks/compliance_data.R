if(!file.exists(here::here("05_Annual_Meeting_Presentations/output/compliance_table.csv"))){

compliance_dat = tibble(
  Station = c("Mt. Robson","Osoyoos","Olsen","Yahk","Radium","Dawson Creek","Golden","Pacific","Average Compliance"),
  `2017` = c(0.97,0.95,NA,0.73,0.75,0.88,0.76,0.48,0.77),
  `2018` = c(0.97,1,0.9,0.84,0.85,0.94,0.85,0.58, 0.81),
  `2019` = c(0.98,1,0.93,0.89,0.89,0.93,0.85,0.6, 0.83),
  `2020` = c(0.966,0.87,0.884,0.884,0.838,NA,0.863,0.666,0.877),
  `2021` = c(0.98,0.98,0.91,0.89,0.88,0.88,0.86,0.75,0.874),
  `2022` = c(0.97,0.99,0.92,NA,0.88,0.92,0.89,0.59,0.880),
  `2023` = c(0.97,1,0.885,0.9,NA,0.94,0.87,0.59,0.88),
  `2024` = c(0.984,0.996,0.870,0.901,NA,0.961,0.921,0.792,0.907),
  `2025` = c(0.978,0.981,0.902,0.938,0.88,0.94,0.915,0.845,0.922)
  
)
} else{
  compliance_dat = read.csv(here::here("05_Annual_Meeting_Presentations/output/compliance_table.csv"))
}

stations.to.include = c("Mt. Robson","Osoyoos","Olsen","Yahk","Radium","Dawson Creek","Golden","Pacific")

if (as.character(most_rec_year) %in% names(compliance_dat)) {
  compliance_dat <- compliance_dat |>
    select(-all_of(as.character(most_rec_year)))
}

compliance_dat <- compliance_dat |>
  tibble::column_to_rownames("Year") |>
  t() |>
  as.data.frame() |>
  tibble::rownames_to_column("Station") |>
  tibble::as_tibble()

# Read in blowbys
blowb = vroom::vroom(paste0("//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years/metabase_blowby_table_2024_onwards.csv")) |> 
  purrr::set_names(snakecase::to_snake_case) |> 
  dplyr::rename(Workflow_ID = observer_workflow_id) |> 
  dplyr::mutate(Workflow_ID = as.numeric(Workflow_ID)) |> 
  dplyr::mutate(blow_by_time = lubridate::mdy_hm(blow_by_time, tz = "UTC") |> 
                  lubridate::with_tz("America/Vancouver"))


current_dat = all_dat |> 
  dplyr::filter(Year == most_rec_year)

blowb = blowb |> 
  left_join(
    current_dat |> 
      dplyr::select(Workflow_ID,
                    Station) |> 
      dplyr::distinct()
  ) |> 
  dplyr::filter(!is.na(Station))

dat_w_blowbys = current_dat |> 
  filter(!Station %in% c("Lower Mainland Roving","Penticton Roving","Cascade")) |> 
  dplyr::filter(Station %in% stations.to.include) |> 
  group_by(Station,Shift_ID,Workflow_ID, Year) |> 
  summarise(NumberInsp = n()) |> 
  #Add number of non-motorized blow-bys per unique station / shift combo.
  left_join(
    blowb |> 
      dplyr::filter(watercraft_complexity == 'Non-motorized') |> 
      dplyr::count(Workflow_ID,sort = T, name = "Non_Motorized_Blow_Bys_Counter")
  ) |> 
  #Add number of motorized blow-bys per unique station / shift combo.
  left_join(
    blowb |> 
      dplyr::filter(watercraft_complexity != 'Non-motorized') |> 
      dplyr::count(Workflow_ID,sort = T, name = "Motorized_Blow_Bys_Counter")
  ) |> 
  dplyr::mutate(dplyr::across(c(Non_Motorized_Blow_Bys_Counter,Motorized_Blow_Bys_Counter), \(x) tidyr::replace_na(x,0))) |> 
  group_by(Station, Year) |> 
  summarise(NumberInsp = sum(NumberInsp),
            Non_Motorized_Blow_Bys_Counter = sum(Non_Motorized_Blow_Bys_Counter),
            Motorized_Blow_Bys_Counter = sum(Motorized_Blow_Bys_Counter)) 


year_col <- as.character(unique(dat_w_blowbys$Year))

compliance_current <- dat_w_blowbys |>
  mutate(
    compliance = 1 - (
      (Motorized_Blow_Bys_Counter + Non_Motorized_Blow_Bys_Counter) /
        NumberInsp
    )
  ) |>
  select(Station, compliance)

compliance_dat <- compliance_dat |>
  left_join(compliance_current, by = "Station") |>
  rename(!!year_col := compliance)

compliance_dat <- compliance_dat |>
  mutate(
    !!year_col := if_else(
      Station == "Average Compliance",
      mean(.data[[year_col]], na.rm = TRUE),
      .data[[year_col]]
    )
  )

compliance_dat <- compliance_dat |>
  tibble::column_to_rownames("Station") |>
  t() |>
  as.data.frame() |>
  tibble::rownames_to_column("Year") |>
  tibble::as_tibble()

if (file.exists(here::here("05_Annual_Meeting_Presentations/output/compliance_table.csv"))) {
  file.remove(here::here("05_Annual_Meeting_Presentations/output/compliance_table.csv"))
}
write.csv(
  compliance_dat,
  here::here("05_Annual_Meeting_Presentations/output/compliance_table.csv"),
  row.names = FALSE
)
