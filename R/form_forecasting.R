library(shiny)
library(bslib)

ui_forecasting <- function(id) {
    tagList(
        checkboxInput(
            inputId = NS(id, "apply"),
            label = "Apply forecasting."
        ) |> helper_("form_forecasting_apply"),
        numericInput(
            inputId = NS(id, "periods"),
            label = "Specify the number of time units:",
            value = 10,
        ) |> helper_("form_forecasting_periods"),
        selectInput(
            inputId = NS(id, "seasonality"),
            label = "Select the seasonality kind:",
            choices = c("Daily", "Weekly", "Yearly"),
            multiple = TRUE
        ) |> helper_("form_forecasting_seasonality"),
        selectInput(
            inputId = NS(id, "mode"),
            label = "Select the seasonality mode:",
            choices = c("Additive", "Multiplicative")
        ) |> helper_("form_forecasting_mode"),
    )
}

server_forecasting <- function(id) {
    moduleServer(id, function(input, output, session) {
        reactive({
            list(
                apply = input$apply,
                periods = input$periods,
                seasonality = input$seasonality,
                mode = input$mode
            )
        })
    })
}
