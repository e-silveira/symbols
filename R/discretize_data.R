library(shiny)
library(lubridate)

ui_discretize_data <- function(id) {
    tagList(
        selectInput(
            inputId = NS(id, "attr"),
            label = "Select the column to discretize:",
            choices = NULL,
        ),
    )
}

server_discretize_data <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        observeEvent(data(), {
            updateSelectInput(
                session,
                "attr",
                choices = get_numeric_colnames(data()),
            )
        })

        reactive({
            req(data(), input$attr)

            attr <- data()[[input$attr]] |> as.numeric()

            list(
                attr,
                input$attr
            )
        })
    })
}
