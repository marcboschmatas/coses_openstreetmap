## GEOCODING BUSINESSES W/O COORDINATES
# Database: Diputació de Barcelona


library(tidyverse)
library(tidygeocoder)



diba <- unzip(read_csv("data/diba_negocis_2022_11_23_def.csv"))

# these are the addresses to be geocoded - they need to have a full address
diba_nocoords <- diba[(is.na(diba$lat)|is.na(diba$lon)) & !is.na(diba$`Adreça completa`),]

# keeping the ones w/o an address just in case
diba_noaddress <- diba[is.na(diba$`Adreça completa`),]

# geocoding coordinates
geo_diba_nocoords <- geo(address = diba_nocoords$`Adreça completa`,method = "arcgis")

geo_diba_nocoords <- geo_diba_nocoords[!(is.na(geo_diba_nocoords$lat)) &
                                           !(is.na(geo_diba_nocoords$long)),]
# put coordinates into the original df


diba_nocoords2 <- diba_nocoords |> 
  select(-c(lat,lon)) |> 
  inner_join(geo_diba_nocoords, by = c("Adreça completa" = "address")) |> 
  rename("lon" = "long") |> 
  distinct()


# make definitive df. It returns all rows with both address and coordinates (Removes all we could not geocode)


diba_def <- diba |> 
  filter(!(`_id` %in% diba_nocoords$`_id`) &
           !(`_id` %in% diba_noaddress$`_id`)) |> 
  bind_rows(diba_nocoords2)



