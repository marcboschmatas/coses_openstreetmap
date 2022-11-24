# script to check if websites of OSM elements are online
library(sf)
library(osmdata)
library(RCurl)



# download restaurants in Girona for whom we have a website

res <- opq("Girona") |> # this sets the area to search
  add_osm_feature(key = "amenity", value = "restaurant") |> 
  add_osm_feature(key = "website") |> # this makes sure we get objects w/ both amenity = restaurant and website = something
  osmdata_sf() # this transforms it to a list of sf tables


points <- res$osm_points
polygons <- res$osm_polygons



#' @title: prepare_data
#' @description: remove geometries, keep the columns we want, filter entries w/o a webpage
#' @param x a sf object
#' @param cols columns to keep
#' @return a tibble
prepare_data <- function(x, cols = c("osm_id", "name", "amenity", "name.ca",
                                     "website")){
  x |> 
    filter(!is.na(website)) |> 
    st_drop_geometry() |> 
    select(cols)
}


res_clean <- bind_rows(prepare_data(points), prepare_data(polygons))

# check that website is up


res_clean$url_exists <- url.exists(res_clean$website)


# df of restaurants w/o a website (need to check manually, some of them exist)

res_nowebsite <- res_clean[res_clean$url_exists == FALSE,]

