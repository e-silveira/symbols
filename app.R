library(shiny)
library(shinyhelper)
library(bslib)

ui <- page_fluid(
    theme = bs_theme(
        primary = "#757575",
        secondary = "#6B6B6B",
        preset = "bootstrap"
    ),
    page_navbar(
        title = "Symbols!",
        tabPanel(
            title = "Discretize",
            icon = bs_icon("bricks"),
            ui_discretize("discretize"),
        ),
        tabPanel(
            title = "Classify",
            icon = bs_icon("boxes"),
            ui_classify("classify"),
        )
    )
)

server <- function(input, output, session) {
    observe_helpers(help_dir = "help")
    server_discretize("discretize")
    server_classify("classify")
}

shinyApp(ui, server)
