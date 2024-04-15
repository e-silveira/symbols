library(prophet)

ui_forecast_inputs <- function(id) {
    list(
        div(
            accordion(
                open = FALSE,
                accordion_panel(
                    title = "Attribute",
                    selectizeInput(
                        inputId = NS(id, "attribute"),
                        label = "Select the attribute:",
                        choices = get_numeric_colnames(default_dataframe)
                    ),
                    selectInput(
                        inputId = NS(id, "time"),
                        label = "Select the time attribute:",
                        choices = NULL,
                    ),
                    selectInput(
                        inputId = NS(id, "date_format"),
                        label = "Select the date format:",
                        choices = date_formats,
                    )
                ),
                accordion_panel(
                    title = "Forecast",
                    tagList(
                        input_switch(
                            id = NS(id, "apply"),
                            label = "Apply forecasting."
                        ),
                        numericInput(
                            inputId = NS(id, "periods"),
                            label = "Specify the number of time units:",
                            value = 10,
                        ),
                        selectizeInput(
                            inputId = NS(id, "seasonality"),
                            label = "Select the seasonality kind:",
                            choices = c("Daily", "Weekly", "Yearly"),
                            multiple = TRUE,
                            options = list(plugins = "remove_button")
                        ),
                        selectInput(
                            inputId = NS(id, "mode"),
                            label = "Select the seasonality mode:",
                            choices = c("Additive", "Multiplicative")
                        ),
                    )
                ),
            )
        ),
        actionButton(
            inputId = NS(id, "apply"),
            label = "Apply",
        )
    )
}

ui_forecast_outputs <- function(id) {
    list(
        navset_underline(
            nav_panel(
                title = "Table",
                dataTableOutput(
                    outputId = NS(id, "table")
                ),
            ),
            nav_panel(
                title = "Plot",
                plotOutput(
                    outputId = NS(id, "plot")
                ),
            ),
        )
    )
}

ui_forecast <- function(id) {
    layout_sidebar(
        sidebar = sidebar(
            width = "30%",
            tab_header("Forecast"),
            !!!ui_forecast_inputs(id),
        ),
        !!!ui_forecast_outputs(id),
    )
}

server_forecast <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        observeEvent(data(), {
            updateSelectizeInput(
                inputId = "attribute",
                choices = get_numeric_colnames(data())
            )
        })

        forecasting <- reactive({
            if (!isTruthy(input$time)) {
                return(NULL)
            }

            df <- data.frame(
                ds = data()[[input$time]],
                y = data()[[input$attribute]]
            )

            m <- prophet(df)
        }) |> bindEvent(input$apply)

        output$plot <- renderPlot({

        }) |> bindEvent(input$apply)
    })
}

time_column_or_stop <- function(column, date_format) {
    tryCatch(
        {
            parse_date_time(column, date_format)
        },
        condition = function(cond) {
            validate("Date format introduced invalid values.")
        }
    )
}
