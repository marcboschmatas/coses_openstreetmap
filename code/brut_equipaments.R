library(tidyverse)
library(sf)
library(httr)
library(osmdata)
library(jsonlite)
library(tmap)
# agafar bbox
o <- opq("Palafrugell")

get_bbox <- function(opq){
  bbox <- opq$bbox
  bbox <- as.vector(sapply(str_split(bbox,","), as.numeric))
  return(bbox)
} 


bbox <- get_bbox(o)

# QUERY NOMÉS PER AGAFAR BBOX

query_location <- paste0(c("SELECT * WHERE latitud <= ", bbox[3],
         " AND latitud >= ", bbox[1],
         " AND longitud <= ", bbox[4],
         " AND longitud >= ", bbox[2]),
       collapse = "")


baseurl <- "https://analisi.transparenciacatalunya.cat/resource/8gmd-gz7i.json?$query="       

query_location <- str_replace_all(query_location," ","%20")

equipaments_bbox <- GET(paste0(baseurl,query_location, collapse = "")) |> 
  content(as = "text") |> 
  fromJSON()

# mapa per comprovar
tmap_mode("view")

equipaments_bbox |> 
  st_as_sf(coords = c("longitud", "latitud"), crs = "EPSG:4326") |> 
  tm_shape() + 
  tm_dots()




# llista de tipus d'equipaments

query <- str_replace_all("SELECT DISTINCT categoria", " ", "%20")

equipaments_llista <- GET(paste0(baseurl,query, collapse = "")) |> 
  content(as = "text") |> 
  fromJSON()



# ANAR-HO PARTINT UN A UN - EDUCACIÓ QUEDA PENDENT PERQUÈ TENIM MÉS INFO

# fer llista de totes les combinacions
equipaments_categories <- equipaments_llista |> 
  separate(categoria, into = c("categoria_1", "categoria_2", "categoria_3", "categoria_4",
                               "categoria_5", "categoria_6","categoria_7",
                               "categoria_8","categoria_9","categoria_10",
                               "categoria_11","categoria_12","categoria_13"), sep = "\\|")



equipaments_categories_13 <- equipaments_categories |> 
  select(categoria_1:categoria_3)
equipaments_categories_46 <- equipaments_categories |> 
  select(categoria_4:categoria_6) |> 
  rename("categoria_1" = "categoria_4",
         "categoria_2" = "categoria_5",
         "categoria_3" = "categoria_6")
equipaments_categories_79 <- equipaments_categories |> 
  select(categoria_7:categoria_9) |> 
  rename("categoria_1" = "categoria_7",
         "categoria_2" = "categoria_8",
         "categoria_3" = "categoria_9")
equipaments_categories_1012 <- equipaments_categories |> 
  select(categoria_10:categoria_12) |> 
  rename("categoria_1" = "categoria_10",
         "categoria_2" = "categoria_11",
         "categoria_3" = "categoria_12")


llista_categories <- distinct(bind_rows(equipaments_categories_13,
                                        equipaments_categories_46,
                                        equipaments_categories_79,
                                        equipaments_categories_1012)) |> 
  filter(categoria_1 != "" &
           !is.na(categoria_1))

write_csv(llista_categories, "~/categories_equipaments.csv")
