library(shiny)
library(shinyhelper)

ui <- fluidPage(
    navbarPage(
        title = "Symbols!",
        tabPanel(
            title = "Discretize",
            ui_discretize("discretize"),
        ),
        tabPanel(
            title = "Classify",
        )
    )
)

server <- function(input, output, session) {
    observe_helpers(help_dir = "help")
    server_discretize("discretize")
}

shinyApp(ui, server)
