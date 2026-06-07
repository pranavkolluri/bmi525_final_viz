library(shiny)
library(tidyverse)
library(leaflet)

weather <- read_csv("data/curiosity_rems_clean.csv", show_col_types = FALSE) %>%
  mutate(earth_date = as.Date(earth_date))

location <- read_csv("data/curiosity_location_clean.csv", show_col_types = FALSE) %>%
  left_join(select(weather, sol, max_air_temp, min_air_temp, pressure), by = "sol")

mars_tiles <- paste0(
  "https://trek.nasa.gov/tiles/Mars/EQ/",
  "Mars_Viking_MDIM21_ClrMosaic_global_232m/",
  "1.0.0/default/default028mm/{z}/{y}/{x}.jpg"
)

# Tab 1 (the map tab) code is mostly in the server function as it requires a lot of leaflet interactive things

# Tab 2: Temperature ribbon over the full mission

makeTempPlot <- function(temp_type) {
  if (temp_type == "air") {
    dat <- weather %>%
      filter(!is.na(max_air_temp), !is.na(min_air_temp)) %>%
      mutate(lo = min_air_temp, hi = max_air_temp, mid = (lo + hi) / 2)
    col <- "#4e79a7"
    lbl <- "Air Temperature (°C)"
  } else {
    dat <- weather %>%
      filter(!is.na(max_ground_temp), !is.na(min_ground_temp)) %>%
      mutate(lo = min_ground_temp, hi = max_ground_temp, mid = (lo + hi) / 2)
    col <- "#e15759"
    lbl <- "Ground Temperature (°C)"
  }

  ggplot(dat, aes(x = earth_date)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = col, alpha = 0.4) +
    geom_line(aes(y = mid), color = col, linewidth = 0.3) +
    labs(title = paste(lbl, "Range -- Curiosity Mission"),
         x = NULL, y = lbl) +
    theme_minimal(base_size = 14)
}


# Tab 3: Variable vs solar longitude (seasonal pattern, all Mars years overlaid)

makeSeasonPlot <- function(var) {
  lbl <- switch(var,
    "max_air_temp" = "Max Air Temp (°C)",
    "min_air_temp" = "Min Air Temp (°C)",
    "pressure" = "Pressure (Pa)"
  )

  dat <- weather %>%
    select(ls, y = all_of(var)) %>%
    filter(!is.na(ls), !is.na(y))

  ggplot(dat, aes(x = ls, y = y)) +
    geom_point(alpha = 0.25, size = 0.9, color = "#4e79a7") +
    geom_smooth(method = "loess", se = FALSE, color = "#e15759", linewidth = 1) +
    scale_x_continuous(
      breaks = c(0, 90, 180, 270, 360),
      labels = c("0\n(N. Spring)", "90\n(N. Summer)", "180\n(N. Autumn)", "270\n(N. Winter)", "360")
    ) +
    labs(title = paste(lbl, "by Solar Longitude"),
         subtitle = "All Mars years overlaid",
         x = "Solar Longitude (LS)", y = lbl) +
    theme_minimal(base_size = 14)
}


# Tab 4: Atmospheric pressure over the full mission

makePressurePlot <- function() {
  weather %>%
    filter(!is.na(pressure)) %>%
    ggplot(aes(x = earth_date, y = pressure)) +
    geom_line(color = "#4e79a7", linewidth = 0.4, alpha = 0.8) +
    #geom_smooth(method = "loess", se = FALSE, color = "#e15759", linewidth = 1) +
    labs(title = "Atmospheric Pressure -- Curiosity Mission",
         x = NULL, y = "Pressure (Pa)") +
    theme_minimal(base_size = 14)
}


ui <- fluidPage(
  titlePanel("Curiosity Rover: Mars Weather Explorer"),

  tabsetPanel(
    tabPanel("Traverse",
      sidebarLayout(
        sidebarPanel(
          selectInput("loc_color", "Color by",
                      choices = c("Sol" = "sol",
                                  "Max Air Temp" = "max_air_temp",
                                  "Pressure" = "pressure")),
          hr(),
          uiOutput("sol_info")
        ),
        mainPanel(leafletOutput("map", height = "550px"))
      )
    ),

    tabPanel("Temperature",
      sidebarLayout(
        sidebarPanel(
          radioButtons("temp_type", "Sensor",
                       choices = c("Air" = "air", "Ground" = "ground"),
                       selected = "air")
        ),
        mainPanel(plotOutput("plot1"))
      )
    ),

    tabPanel("Seasonal Patterns",
      sidebarLayout(
        sidebarPanel(
          selectInput("season_var", "Variable",
                      choices = c("Max Air Temp" = "max_air_temp",
                                  "Min Air Temp" = "min_air_temp",
                                  "Pressure" = "pressure"))
        ),
        mainPanel(plotOutput("plot2"))
      )
    ),

    tabPanel("Atmospheric Pressure",
      mainPanel(plotOutput("plot3"))
    )
  )
)


server <- function(input, output, session) {

  selected_sol <- reactiveVal(NULL)

  # zoom level 7 is the max zoom, so force that to be the last tile level
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(crs = leafletCRS("L.CRS.EPSG4326"))) %>%
      addTiles(urlTemplate = mars_tiles, attribution = "NASA Mars Trek",
               options = tileOptions(maxNativeZoom = 7, maxZoom = 18)) %>%
      setView(lng = 137.4, lat = -4.6, zoom = 6) %>%
      addScaleBar(position = "bottomleft")
  })

  observe({
    var <- input$loc_color
    pal <- colorNumeric("inferno", location[[var]], na.color = "grey70")
    lbl <- switch(var,
      "sol" = "Sol",
      "max_air_temp" = "Max Air Temp (C)",
      "pressure" = "Pressure (Pa)"
    )

    dat <- location %>%
      filter(!is.na(.data[[var]])) %>%
      mutate(fill_col = pal(.data[[var]]))

    leafletProxy("map", data = dat) %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%
      addPolylines(lng = ~longitude, lat = ~latitude,
                   color = "white", weight = 5, opacity = 0.3) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        fillColor = ~fill_col,
        color = NA, fillOpacity = 0.85, radius = 3,
        layerId = ~as.character(sol)
      ) %>%
      addLegend("bottomright", pal = pal, values = location[[var]], title = lbl)
  })

  observeEvent(input$map_marker_click, {
    selected_sol(as.integer(input$map_marker_click$id))
  })

  output$sol_info <- renderUI({
    s <- selected_sol()
    if (is.null(s)) return(p(em("Click a point on the map to see sol data.")))

    w <- weather %>% filter(sol == s)
    loc <- location %>% filter(sol == s)
    if (nrow(w) == 0) return(p(paste("No weather data for Sol", s)))

    fmt <- function(x, unit) if (is.na(x)) "--" else paste0(x, unit)

    tagList(
      h4(paste0("Sol ", s)),
      p(strong("Date: "), format(w$earth_date)),
      tags$table(style = "width:100%",
        tags$tr(tags$td("Max Air Temp"), tags$td(fmt(w$max_air_temp, " C"))),
        tags$tr(tags$td("Min Air Temp"), tags$td(fmt(w$min_air_temp, " C"))),
        tags$tr(tags$td("Pressure"), tags$td(fmt(w$pressure, " Pa"))),
        tags$tr(tags$td("UV"), tags$td(fmt(w$uv, ""))),
        tags$tr(tags$td("Weather"), tags$td(fmt(w$weather, ""))),
        tags$tr(tags$td("Elevation"), tags$td(fmt(round(loc$elevation, 1), " m")))
      )
    )
  })

  output$plot1 <- renderPlot(makeTempPlot(input$temp_type))
  output$plot2 <- renderPlot(makeSeasonPlot(input$season_var))
  output$plot3 <- renderPlot(makePressurePlot())
}

shinyApp(ui = ui, server = server)
