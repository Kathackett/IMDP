library(tidyverse)
library(DT)


inspection_stations = sf::read_sf("./04_Extra_Figures_and_Scripts/inspection_stations.gpkg")
unique(inspection_stations$station_name)

inspection_stations = inspection_stations |> 
  mutate(station_name = case_when(
    station_name == "Scheduled Inspection" ~ "Penticton",
    station_name == "Lower Mainland - roving crew" ~ "Lower Mainland",
    TRUE ~ station_name
  )) |> 
  mutate(station_type = case_when(
    station_name == "Cascade" ~ "Roving Inspection Crew",
    TRUE ~ station_type
  )) |> 
  
  filter(station_name != "Balfour") |> 
  distinct(station_name, .keep_all = TRUE)


library(leaflet)

leaflet(inspection_stations) |>
  addProviderTiles(providers$OpenStreetMap) |>
  addCircleMarkers(
    radius = 4,
    color = "blue",
    label = ~station_name,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "right",
      textsize = "12px"
    )
  )

write.table(inspection_stations |> sf::st_drop_geometry(), "./04_Extra_Figures_and_Scripts/inspection_stations_table.txt", sep = "\t", row.names = F)

sf::write_sf(inspection_stations, "./04_Extra_Figures_and_Scripts/inspection_stations.gpkg")
