library(shiny)
library(stringr)
library(purrr)
library(magrittr)

ui_discretize_symbolic <- function(id) {
    tagList(
        numericInput(
            inputId = NS(id, "alpha"),
            label = "Select the number of classes:",
            value = 4,
        ),
        textInput(
            inputId = NS(id, "classes"),
            label = "Specify the class names:"
        ),
        numericInput(
            inputId = NS(id, "compr"),
            label = "Specify the group size:",
            value = 1,
        ),
        selectInput(
            inputId = NS(id, "method"),
            label = "Select the method:",
            choices = c("SAX", "dwSAX", "qSAX"),
        ),
    )
}

server_discretize_symbolic <- function(id) {
    moduleServer(id, function(input, output, session) {
        class_names <- reactive({
            if (isTruthy(input$classes)) {
                str_split_1(input$classes, ",") |>
                    reduce(function(acc, x) if (x == "") acc else c(acc, x)) |>
                    map_chr(str_trim)
            } else {
                NULL
            }
        })

        observeEvent(class_names(), {
            if (length(class_names()) > 0) {
                updateNumericInput(
                    inputId = "alpha",
                    value = length(class_names())
                )
            }
        })

        reactive({
            list(
                alpha = input$alpha,
                compr = input$compr,
                method = str_to_lower(input$method),
                classes = class_names()
            )
        })
    })
}
