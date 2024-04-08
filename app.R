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
            ui_input("input")
        ),
        nav_panel(
            title = "Discretize",
            ui_discretize("discretize"),
        ),
        nav_panel(
            title = "Classify",
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
    server_discretize("discretize", data)
    server_classify("classify", data)
}

shinyApp(ui, server)
