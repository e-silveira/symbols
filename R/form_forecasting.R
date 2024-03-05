library(shiny)

forecasting_options_ui <- function(id) {
    tagList(
        checkboxInput(
            inputId = NS(id, "apply"),
            label = "Show forecasting."
        ),
        numericInput(
            inputId = NS(id, "periods"),
            label = "Specify the number of time units.",
            value = 10,
        ),
        selectInput(
            inputId = NS(id, "seasonality"),
            label = "Seasonality kind.",
            choices = c("Daily", "Weekly", "Yearly"),
            multiple = TRUE
        ),
        selectInput(
            inputId = NS(id, "mode"),
            label = "Seasonality mode.",
            choices = c("Additive", "Multiplicative")
        )
    )
}

forecasting_options_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        reactive({
            list(
                apply = input$apply,
                periods = input$periods,
                kind = input$seasonality,
                mode = input$mode
            )
        })
    })
}
