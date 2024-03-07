library(shiny)

ui <- fluidPage(
    navbarPage(
        title = "Symbols!",
        tabPanel(
            title = "Discretize",
            ui_discretize("discretize")
        ),
        tabPanel(
            title = "Classify",
        )
    )
)

server <- function(input, output, session) {
    server_discretize("discretize")
}

shinyApp(ui, server)
