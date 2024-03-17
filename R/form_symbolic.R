library(shiny)
library(shinyhelper)
library(stringr)
library(purrr)
library(magrittr)

ui_symbolic <- function(id) {
    tagList(
        numericInput(
            inputId = NS(id, "alpha"),
            label = "How many classes?",
            value = 4,
        ) |> helper_("form_symbolic_alpha"),
        textInput(
            inputId = NS(id, "classes"),
            label = "What class names?"
        ) |> helper_("form_symbolic_classes"),
        numericInput(
            inputId = NS(id, "compr"),
            label = "What group size?",
            value = 1,
        ) |> helper_("form_symbolic_compr"),
        selectInput(
            inputId = NS(id, "method"),
            label = "What method?",
            choices = c("SAX", "dwSAX", "qSAX"),
        ) |> helper_("form_symbolic_method"),
    )
}

server_symbolic <- function(id) {
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
