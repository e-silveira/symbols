library(shiny)
library(dplyr)
library(lubridate)

selection_ui <- function(id) {
    tagList(
        selectInput(
            inputId = NS(id, "attribute"),
            label = "Select an attribute.",
            choices = NULL,
        ),
        selectInput(
            inputId = NS(id, "time"),
            label = "Select the time column.",
            choices = NULL,
        ),
    )
}

selection_server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        observeEvent(data(), {
            updateSelectInput(
                session,
                "attribute",
                choices = get_numeric_colnames(data())
            )
            updateSelectInput(
                session,
                "time",
                choices = get_date_colnames(data())
            )
        })

        reactive({
            req(input$attribute)

            if (isTruthy(input$time) && input$time %in% colnames(data())) {
                list(
                    Time = as.Date(data()[[input$time]]),
                    Attribute = as.numeric(data()[[input$attribute]])
                )
            } else {
                list(
                    Time = NULL,
                    Attribute = as.numeric(data()[[input$attribute]])
                )
            }
        })
    })
}
