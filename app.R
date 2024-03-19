library(shiny)
library(shinyhelper)
library(bslib)

theme_light <- bs_theme(
    primary = "#757575",
    secondary = "#757575",
    preset = "bootstrap"
)

ui <- page_fluid(
    theme = theme_light,
    page_navbar(
        title = "Symbols!",
        nav_panel(
            title = "Input",
            icon = bs_icon("database-up")
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
    observe_helpers(help_dir = "help")
    server_discretize("discretize")
    server_classify("classify")
}

shinyApp(ui, server)
