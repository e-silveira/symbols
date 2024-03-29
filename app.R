library(shiny)
library(shinyhelper)
library(bslib)

theme_light <- bs_theme(
    primary = "#BBBBBB",
    secondary = "#555555",
    preset = "bootstrap"
)

ui <- page_fluid(
    theme = theme_light,
    page_navbar(
        title = "Symbols!",
        nav_panel(
            title = "Data",
            icon = bs_icon("database-up"),
            ui_input("input")
        ),
        nav_panel(
            title = "Discretize",
            icon = bs_icon("bricks"),
            ui_discretize("discretize"),
        ),
        nav_panel(
            title = "Classify",
            icon = bs_icon("boxes"),
            ui_classify("classify"),
        ),
        nav_spacer(),
        nav_item(
            input_dark_mode(id = "dark_mode", mode = "light")
        )
    )
)

server <- function(input, output, session) {
    data <- server_input("input")
    observe_helpers(help_dir = "help")
    server_discretize("discretize", data$original, data$modified)
    server_classify("classify", data$modified)
}

shinyApp(ui, server)
