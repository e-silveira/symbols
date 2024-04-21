library(shiny)
library(dplyr)
library(bslib)

ui_classify_data <- function(id) {
    tagList(
        selectInput(
            inputId = NS(id, "target"),
            label = "Select the target attribute:",
            choices = NULL,
        ),
        selectizeInput(
            inputId = NS(id, "features"),
            label = "Select the features:",
            choices = NULL,
            multiple = TRUE,
            options = list(plugins = "remove_button")
        )
    )
}

server_classify_data <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        cols <- reactive({
            colnames(data()) |> sort()
        })

        observeEvent(cols(), {
            updateSelectInput(
                session,
                "target",
                choices = cols(),
                selected = if (isTruthy(input$target)) input$target else NULL
            )

            # Update if there is a new discretized column.
            updateSelectizeInput(
                session,
                "features",
                choices = cols(),
                selected = input$features
            )
        })

        observeEvent(input$target, {
            updateSelectizeInput(
                session,
                "features",
                choices = setdiff(cols(), input$target),
                selected = input$features
            )
        })

        reactive({
            select(data(), all_of(c(input$target, input$features)))
        })
    })
}
