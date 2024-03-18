library(shiny)
library(shinyhelper)
library(lubridate)

ui_input <- function(id) {
    tagList(
        fileInput(
            inputId = NS(id, "file"),
            label = "Select a data file:",
            accept = ".csv",
            placeholder = "santa_maria_dengue.csv"
        ) |> helper_("form_input_file"),
        selectInput(
            inputId = NS(id, "attr"),
            label = "Select the column to discretize:",
            choices = NULL,
        ) |> helper_("form_input_attr"),
        selectInput(
            inputId = NS(id, "time"),
            label = "Select the time column:",
            choices = NULL,
        ) |> helper_("form_input_time"),
        selectInput(
            inputId = NS(id, "date_format"),
            label = "Select the date format:",
            choices = date_formats,
        ) |> helper_("form_input_date_format"),
    )
}

server_input <- function(id) {
    moduleServer(id, function(input, output, session) {
        current_data <- reactiveVal(read.csv("./data/santa_maria_dengue.csv"))

        data <- reactive({
            if (isTruthy(input$file)) {
                current_data(read.csv(input$file$datapath))
            }
            current_data()
        })

        observeEvent(data(), {
            updateSelectInput(
                session,
                "attr",
                choices = get_numeric_colnames(data()),
            )

            updateSelectInput(
                session,
                "time",
                choices = get_date_colnames(data()),
            )
        })

        reactive({
            req(data(), input$attr)

            attr <- data()[[input$attr]] |> as.numeric()

            time <- NULL
            if (isTruthy(input$time)) {
                time <- time_column_or_stop(
                    data()[[input$time]],
                    input$date_format
                )
            }

            list(
                time,
                input$time,
                attr,
                input$attr
            )
        })
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
