library(shiny)

ui <- fluidPage(
    titlePanel("Symbolize"),
    sidebarLayout(
        sidebarPanel(
            user_input_ui("user_input"),
            selection_ui("selection"),
            symbolic_options_ui("symbolic_options"),
            forecasting_options_ui("forecasting_options"),
        ),
        mainPanel(
            symbolize_ui("symbolize")
        ),
    ),
)

server <- function(input, output, session) {
    data <- user_input_server("user_input")

    selected_columns <- selection_server("selection", data)
    symbolic_options <- symbolic_options_server("symbolic_options")
    forecasting_options <- forecasting_options_server("forecasting_options")

    symbolize_server(
        "symbolize",
        selected_columns,
        symbolic_options,
        forecasting_options
    )

    output$data <- renderTable(head(data()))
}

shinyApp(ui, server)
