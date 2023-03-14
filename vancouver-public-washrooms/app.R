library(shiny)
library(tidyverse)
library(leaflet)

# Note: using CSV downloaded March 2023 due to Vancouver City API database not including all data.
bathroom_data = read_delim("public-washrooms.csv", delim=";")

bathroom_data = bathroom_data %>%
                separate_wider_delim(geo_point_2d, ", ", names=c("Lat", "Long")) %>%
                mutate(Lat = as.numeric(Lat)) %>%
                mutate(Long = as.numeric(Long))



# TODO: filter bathrooms shown based on requirements

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

    # Left filtering panel
    absolutePanel(
      top = 100, left = 10, draggable = TRUE, width = "20%", style = "z-index:500; min-width: 300px;",
      actionButton("show_current_location", "Show my location", class = "btn-primary"),
    )

    )


# Define server logic
server <- function(input, output, session) {


    popup_content <- function(data) {
      # returns HTML for marker content
      content <- paste(sep = "<br/>",
                       paste0("<b>", data$NAME, "</b>"),
                       data$ADDRESS,
                       data$LOCATION,
                       paste0("Wheel access: ", data$WHEEL_ACCESS),
                       paste0("Summer Hours: ", data$SUMMER_HOURS),
                       paste0("Winter Hours: ", data$WINTER_HOURS),
                       paste0("Note: ", data$NOTE),
                       paste0("<a href='https://www.google.com/maps/search/?api=1&query=", data$Lat, ",", data$Long, "'>Load Location in Google Maps</a>")
      )
      return(content)
    }
    # Draws default map
    output$bathroom_map <- renderLeaflet({
      leaflet(bathroom_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addMarkers(popup=popup_content(bathroom_data))
    })

    # Filter controls
    observeEvent(input$show_current_location, {
    tryCatch({
        Sys.sleep(1)
        shiny::validate(
          need(input$geolocation, message = FALSE)
        )
    
        leafletProxy("bathroom_map", session) %>%
          setView(input$long, input$lat, zoom=15) %>% #current location
          addCircles(input$long, input$lat) #current location
      },
        error=function(e) {
          showModal(modalDialog(title = "Sorry!",
                                tags$p("We couldn't fetch your location."),
                                tags$p("Please ensure you've allowed location sharing."), 
                                tags$p("You may need to refresh your browser after clicking 'allow'")))
        }

        )

    })


}

# Run the application
shinyApp(ui = ui, server = server)
























