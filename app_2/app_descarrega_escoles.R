#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(osmdata)
library(sf)
library(rvest)

# Define UI for application that projects a table
ui <- fluidPage(

    # Application title
    titlePanel("Descàrrega dades escoles"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("location",
                      label = "Municipi/Comarca"),
            downloadButton("downloadData", "Download")),
        


        mainPanel(
          p("Aquesta aplicació descarrega les dades de les escoles de qualsevol municipi o comarca de Catalunya. Atès que filtra per un rectangle al voltant del municipi o comarca, poden sortir centres de municipis o comarques veïnes. Per qualsevol dubte o problema, podeu obrir un issue a https://github.com/marcboschmatas/coses_openstreetmap. Versió en progrés.
"),
           tableOutput("table")
        ),
        position = "left"
    )
)
get_school_data <- function(location){
    escoles <- opq(location) |> 
      add_osm_features("[\"amenity\"=\"school\"][\"ref\"]") |> # this is a lot more cumbersome but allows to filter for missing tags
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
    
    # clean all easy variables
    
    escoles_gene_osm <- escoles_gene |> 
      select(-any_of(c("curs", "codi_titularitat", "codi_naturalesa", 
                       "codi_delegaci", "nom_delegaci", "codi_comarca",
                       "codi_municipi", "codi_municipi_6", 
                       "codi_localitat", "codi_districte_municipal", 
                       "nom_comarca", "zona_educativa",
                       "coordenades_utm_x", "coordenades_utm_y",
                       "coordenades_geo_x", "coordenades_geo_y"))) |> # eliminar columnes que no calen
      rename(any_of(c("operator" = "nom_titularitat",
             "name" = "denominaci_completa",
             "ref" = "codi_centre",
             "contact:phone" = "tel_fon",
             "contact:fax" = "fax",
             "contact:email" = "e_mail_centre",
             "website" = "url",
             "addr:city" = "nom_municipi",
             "addr:place" = "nom_localitat",
             "addr:postcode" = "codi_postal",
             "source:date" = "any"))) |> # canviar noms de columnes que podem adaptar "tal qual"
      mutate("contact:fax" = paste0("+34",`contact:fax`),
             "contact:phone" = paste0("+34", `contact:phone`),
             "addr:street" = str_extract(adre_a, "^[^\\,]+"),
             "addr:housenumber" = str_extract(adre_a, "[^\\, ]+$"),
             "operator:type" = ifelse(nom_naturalesa == "Públic", "public", "private"))
    
    
    # get table of isced levels min and max age
    
    eqs <- "https://wiki.openstreetmap.org/wiki/Import_schools_in_Catalunya" |> 
      read_html() |> 
      html_nodes(xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/table[2]') |> 
      html_table()
    eqs <- eqs[[1]] |> 
      mutate(Cycle = str_split(Cycle, ", ")) |> 
      unnest(cols = Cycle) |> 
      mutate(Cycle = str_to_lower(Cycle))
    
    
    # generate the more difficult columns
    
    escoles_gene_isced <- escoles_gene |> 
      select(any_of(c("codi_centre", eqs$Cycle))) |> 
      pivot_longer(-codi_centre,
                   names_to = "level_name",
                   values_to = "junk") |> 
      filter(!is.na(junk)) |> 
      select(-junk) |> 
      left_join(eqs, by = c("level_name" = "Cycle")) |> 
      mutate("school" = case_when(level_name %in% c("einf1c", "einf2c") ~ NA_character_,
                                  level_name == "epri" ~ "primary",
                                  level_name %in% c("eso", "batx") ~ "secondary",
                                  level_name %in% c("aa01", "cfpm", "ppas", 
                                                    "aa03", "cfps", "pfi", "pa01",
                                                    "cfam", "pa02", "cfas",
                                                    "esdi", "adr", "crbc",
                                                    "dans", "musp", "muss",
                                                    "tegm", "tegs") ~ "professional",
                                  level_name == "ee" ~ "special_education_needs",
                                  TRUE ~ NA_character_),
             "amenity" = case_when(level_name == "muse" ~ "music_school",
                                   level_name == "dane" ~ NA_character_,
                                   level_name == "idi" ~ "language_school",
                                   TRUE ~ "school")) |> # falta posar les escoles de dansa
      arrange(`ISCED level`) |> 
      group_by(codi_centre) |> 
      summarise("isced:level" = paste(unique(`ISCED level`), collapse = "; "),
                "min_age" = min(min_age),
                "max_age" = ifelse(length(c(max_age)[is.na(c(max_age))]) > 0, NA_integer_,max(max_age)),
                "amenity" = paste(unique(`amenity`), collapse = "; "),
                "school" = paste(unique(school)[!is.na(c(unique(school)))], collapse = "; "))
    
    
    escoles_gene_osm <- escoles_gene_osm |> 
      select(-any_of(c(eqs$Cycle, "nom_naturalesa", "adre_a"))) |> 
      left_join(escoles_gene_isced, by = c("ref" = "codi_centre"))
    escoles_gene_osm
}
server <- function(input, output) {
  datasetInput <- reactive({get_school_data(location=input$location)})
    output$table <- renderTable(datasetInput())
    output$downloadData <- downloadHandler(
      filename = function(){
        paste(input$location, ".csv", sep = "")
      },
      content = function(file){
        write.csv(datasetInput(), file, row.names=FALSE)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
