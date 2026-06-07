library(shiny)
library(tidyverse)

weather <- read_csv("data/curiosity_rems_clean.csv", show_col_types = FALSE) %>%
  mutate(earth_date = as.Date(earth_date))

location <- read_csv("data/curiosity_location_clean.csv", show_col_types = FALSE) %>%
  left_join(select(weather, sol, max_air_temp, min_air_temp, pressure), by = "sol")


# Tab 1: Traverse path through Gale Crater

makeLocationPlot <- function(color_var) {
  lbl <- switch(color_var,
    "sol" = "Sol",
    "max_air_temp" = "Max Air Temp (°C)",
    "pressure" = "Pressure (Pa)"
  )

  dat <- location %>% filter(!is.na(.data[[color_var]]))

  ggplot(dat, aes(x = easting, y = northing, color = .data[[color_var]])) +
    geom_path(linewidth = 0.5, alpha = 0.8) +
    geom_point(data = slice_head(dat, n = 1), size = 3, shape = 17, color = "white") +
    scale_color_viridis_c(option = "inferno", name = lbl) +
    coord_fixed() +
    labs(title = "Curiosity Traverse — Gale Crater",
         x = "Easting (m)", y = "Northing (m)") +
    theme_minimal(base_size = 14) +
    theme(axis.text = element_text(size = 9))
}


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
    labs(title = paste(lbl, "Range — Curiosity Mission"),
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
      labels = c("0°\n(N. Spring)", "90°\n(N. Summer)", "180°\n(N. Autumn)", "270°\n(N. Winter)", "360°")
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
    geom_smooth(method = "loess", se = FALSE, color = "#e15759", linewidth = 1) +
    labs(title = "Atmospheric Pressure — Curiosity Mission",
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
                                  "Pressure" = "pressure"))
        ),
        mainPanel(plotOutput("plot_loc", height = "550px"))
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
  output$plot_loc <- renderPlot(makeLocationPlot(input$loc_color))
  output$plot1 <- renderPlot(makeTempPlot(input$temp_type))
  output$plot2 <- renderPlot(makeSeasonPlot(input$season_var))
  output$plot3 <- renderPlot(makePressurePlot())
}

shinyApp(ui = ui, server = server)
