library(prophet)
library(stringr)
library(ggplot2)
library(DT)

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
                        choices = get_date_colnames(default_dataframe),
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
                        numericInput(
                            inputId = NS(id, "periods"),
                            label = "Specify the number of time units:",
                            value = 10,
                        ),
                        div(
                            h6("Seasonality:"),
                            input_switch(
                                id = NS(id, "daily"),
                                label = "Daily"
                            ),
                            input_switch(
                                id = NS(id, "weekly"),
                                label = "Weekly"
                            ),
                            input_switch(
                                id = NS(id, "yearly"),
                                label = "Yearly"
                            )
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
            nav_panel(
                title = "Decomposition",
                plotOutput(
                    outputId = NS(id, "decomposition")
                )
            )
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

            updateSelectizeInput(
                inputId = "time",
                choices = get_date_colnames(data())
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

            m <- prophet(
                df,
                daily.seasonality = input$daily,
                weekly.seasonality = input$weekly,
                yearly.seasonality = input$yearly,
                seasonality.mode = str_to_lower(input$mode)
            )

            future <- make_future_dataframe(m, periods = input$periods)

            list(model = m, prediction = predict(m, future))
        }) |> bindEvent(input$apply)

        output$table <- renderDataTable(
            {
                req(forecasting())
                forecasting()$prediction
            },
            options = list(
                lengthChange = FALSE,
                paging = TRUE, searching = FALSE,
                scrollX = TRUE, scrollY = TRUE
            )
        ) |> bindEvent(input$apply)

        output$plot <- renderPlot({
            fc <- forecasting()

            if (!isTruthy(fc)) {
                validate("You need a time column in order to forecast.")
            }

            plot(fc$model, fc$prediction) +
                theme_minimal() +
                scale_color_viridis_d()
        }) |> bindEvent(input$apply)

        output$decomposition <- renderPlot({
            fc <- forecasting()
            prophet_plot_components(fc$model, fc$prediction)
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
