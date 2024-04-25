library(shiny)

ui <- fluidPage(
    p("Hello, world!")
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
