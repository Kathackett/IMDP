
csv_path <- here::here("05_Annual_Meeting_Presentations/output/compliance_table.csv")
year_col <- as.character(most_rec_year)

stations.to.include <- c("Mt. Robson", "Osoyoos", "Olsen", "Yahk",
                         "Radium", "Dawson Creek", "Golden", "Pacific")

expected_stations <- c(stations.to.include, "Average Compliance")

build_history <- function() {
  tibble::tibble(
    Station = expected_stations,
    `2017` = c(0.97, 0.95, NA, 0.73, 0.75, 0.88, 0.76, 0.48, 0.77),
    `2018` = c(0.97, 1, 0.9, 0.84, 0.85, 0.94, 0.85, 0.58, 0.81),
    `2019` = c(0.98, 1, 0.93, 0.89, 0.89, 0.93, 0.85, 0.6, 0.83),
    `2020` = c(0.966, 0.87, 0.884, 0.884, 0.838, NA, 0.863, 0.666, 0.877),
    `2021` = c(0.98, 0.98, 0.91, 0.89, 0.88, 0.88, 0.86, 0.75, 0.874),
    `2022` = c(0.97, 0.99, 0.92, NA, 0.88, 0.92, 0.89, 0.59, 0.880),
    `2023` = c(0.97, 1, 0.885, 0.9, NA, 0.94, 0.87, 0.59, 0.88),
    `2024` = c(0.984, 0.996, 0.870, 0.901, NA, 0.961, 0.921, 0.792, 0.907),
    `2025` = c(0.978, 0.981, 0.902, 0.938, 0.88, 0.94, 0.915, 0.845, 0.922)
  )
}

compliance_dat <- if (file.exists(csv_path)) {
  readr::read_csv(csv_path, show_col_types = FALSE, name_repair = "minimal")
} else {
  NULL
}

if (is.null(compliance_dat) ||
    !"Station" %in% names(compliance_dat) ||
    !setequal(compliance_dat$Station, expected_stations)) {
  compliance_dat <- build_history()
}

compliance_dat <- compliance_dat |>
  select(-any_of(year_col))

# blow by time
blowb <- vroom::vroom(
  "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Zebra_Quagga_Mussel/Operations/Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years/metabase_blowby_table_2024_onwards.csv"
) |>
  purrr::set_names(snakecase::to_snake_case) |>
  rename(Workflow_ID = observer_workflow_id) |>
  mutate(
    Workflow_ID = as.numeric(Workflow_ID),
    blow_by_time = lubridate::mdy_hm(blow_by_time, tz = "UTC") |>
      lubridate::with_tz("America/Vancouver")
  )

current_dat <- all_dat |>
  filter(Year == most_rec_year, Station %in% stations.to.include)

blowb <- blowb |>
  inner_join(
    current_dat |> distinct(Workflow_ID, Station),
    by = "Workflow_ID"
  )

blowby_counts <- blowb |>
  count(Station) |>
  rename(blow_bys = n)

inspection_counts <- current_dat |>
  count(Station, name = "inspections")

current_compliance <- inspection_counts |>
  left_join(blowby_counts, by = "Station") |>
  mutate(
    blow_bys = replace_na(blow_bys, 0),
    !!year_col := round(1 - blow_bys / inspections, 3)
  ) |>
  select(Station, all_of(year_col))

# calculate the years current copmliance
avg_current <- round(mean(current_compliance[[year_col]], na.rm = TRUE), 3)

# join the average compliance to the current year
current_compliance <- bind_rows(
  current_compliance,
  tibble::tibble(Station = "Average Compliance", !!year_col := avg_current)
)

compliance_dat <- compliance_dat |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))


# join the new row onto the main table
compliance_dat <- compliance_dat |>
  left_join(current_compliance, by = "Station") |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

readr::write_csv(compliance_dat, csv_path)
