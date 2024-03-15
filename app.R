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
