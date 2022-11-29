library(tidyverse)
library(httr)
library(jsonlite)
library(osmdata)
library(sf)



# download schools in Palafrugell missing the isced:level keys


escoles <- opq("Palafrugell") |> 
  add_osm_features("[\"amenity\"=\"school\"][\"isced_level\"!~\".*\"][\"ref\"]") |> # this is a lot more cumbersome but allows to filter for missing tags
  osmdata_sf()

escoles_punts <- escoles$osm_points
escoles_pol <- escoles$osm_polygons
escoles_mpol <- escoles$osm_multipolygons


refs <- unique(c(escoles_pol$ref, escoles_mpol$ref))
refs <- refs[!is.na(refs)]

reftext <- sapply(refs, \(x) paste0("'",str_pad(x, width=8, pad = 0),"'")) |> 
  paste(collapse = ", ")


baseurl <- "https://analisi.transparenciacatalunya.cat/resource/kvmv-ahh4.json?$query="


query = paste0("SELECT * WHERE codi_centre IN (", reftext, ") AND curs='2022/2023'")
query <- str_replace_all(query," ","%20")


escoles_gene <- GET(paste0(baseurl,query)) |> 
  content(as = "text") |> 
  fromJSON()

# here any other variable that we need may be added. EG operator and operator:type

escoles_gene_titularitat <- escoles_gene |> 
  select(codi_centre, nom_naturalesa, nom_titularitat) |> 
  rename("operator_2" = "nom_titularitat") |> # need to change the name because some schools already have an operator defined - need to check manually whether to change it or not
  mutate("operator:type" = ifelse(nom_naturalesa == "Públic", "public", "private")) |> 
  select(-nom_naturalesa)

# add these two columns to the osm objects
# I only create a table w/o geometry because I'm assuming this will be used for manual input - will change it later on to make an importation possible

escoles_osm <- bind_rows(st_drop_geometry(escoles_mpol), st_drop_geometry(escoles_pol)) |>  # we do not add points because those are parts of other objects
  left_join(escoles_gene_titularitat, by = c("ref" = "codi_centre"))




# transform registers to ISCED level


escoles_gene_isced <- escoles_gene |> 
  select(codi_centre, "epri", "einf1c", "eso", "batx", "cfpm", "aa03", "cfps", "pfi") |> 
  pivot_longer(-codi_centre,
               names_to = "level_name",
               values_to = "junk") |> 
  filter(!is.na(junk)) |> 
  select(-junk) |> 
  mutate(`isced:level` = case_when(level_name == "epri" ~ 1,
                                 level_name == "einf1c" ~ 0,
                                 level_name == "einf2c" ~ 0,
                                 level_name == "eso" ~ 3,
                                 level_name == "batx" ~ 3,
                                 level_name == "cfpm" ~ 3,
                                 level_name == "cfps" ~ 5,
                                 level_name == "aa03" ~ 4,
                                 level_name == "pfi" ~ 2,
                                 TRUE ~ as.numeric(NA))) |> # very much work in progress - need to put every single level
  group_by(codi_centre) |> 
  arrange(`isced:level`) |> 
  distinct() |> 
  summarise("isced:level" = paste(unique(`isced:level`), collapse = "; "))
