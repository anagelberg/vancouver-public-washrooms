
library(shiny)
library(tidyverse)
library(leaflet)


bathroom_data = read_delim("public-washrooms.csv", delim=";")

bathroom_data = bathroom_data %>% 
                separate_wider_delim(geo_point_2d, ", ", names=c("Lat", "Long")) %>% 
                mutate(Lat = as.numeric(Lat)) %>% 
                mutate(Long = as.numeric(Long))


# TODO: change color based on if currently Open 
# TODO: Make the map pop ups
# TODO: Include a way to link 
# TODO: filter bathrooms shown based on requirements
# TODO: Fix the error at sign in (need location) to a loading bar. 


# Define UI for application 
ui <- bootstrapPage(
  
      tags$head(
        tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
        tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}")#,
      ),
      # This script gets the users location
      tags$script('
      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
        function onError (err) {
          Shiny.onInputChange("geolocation", false);
        }
              
        function onSuccess (position) {
          setTimeout(function () {
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.onInputChange("geolocation", true);
            Shiny.onInputChange("lat", coords.latitude);
            Shiny.onInputChange("long", coords.longitude);
          }, 1100)
        }
      });
              '),
      
      
    

    leafletOutput("bathroom_map", width="100%", height="100%"),
    
    # Top right titles
    absolutePanel(
      top = 10, right = 10, style = "z-index:500; text-align: right;",
      tags$h1("Public Washrooms in Vancouver, BC"),
      tags$a("About this tool", href="")
    ),
    
    )


# # Define server logic 
server <- function(input, output) {
  
  
  
    popup_content <- function(data) {
      content <- paste(sep = "<br/>",
                       paste0("<b>", data$NAME, "</b>"), 
                       data$ADDRESS, 
                       data$LOCATION
      )
      return(content)
    }

    output$bathroom_map <- renderLeaflet({
      leaflet(bathroom_data) %>%
        setView(input$long, input$lat, zoom=16) %>% 
        addCircles(input$long, input$lat) %>% #current location
        addProviderTiles("CartoDB.Positron") %>% 
        addMarkers(popup=popup_content(bathroom_data))
    })
}

# # Run the application 
shinyApp(ui = ui, server = server)
