library(shiny)

ui <- fluidPage(
    navbarPage(
        title = "Symbols!",
        tabPanel(
            title = "Discretize",
            discretize_ui("discretize")
        ),
        tabPanel(
            title = "Classify",
        )
    )
)

server <- function(input, output, session) {
    discretize_server("discretize")
}

shinyApp(ui, server)
