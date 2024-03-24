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
                choices = cols()
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
