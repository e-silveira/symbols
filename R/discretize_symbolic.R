library(shiny)
library(stringr)
library(purrr)
library(magrittr)

ui_discretize_symbolic <- function(id) {
    tagList(
        numericInput(
            inputId = NS(id, "n"),
            label = "Select the number of classes:",
            value = 4,
        ),
        textInput(
            inputId = NS(id, "alphabet"),
            label = "Specify the class names:"
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
        alphabet <- reactive({
            if (isTruthy(input$alphabet)) {
                str_split_1(input$alphabet, ",") |>
                    reduce(function(acc, x) if (x == "") acc else c(acc, x)) |>
                    map_chr(str_trim)
            } else {
                NULL
            }
        })

        observeEvent(alphabet(), {
            if (length(alphabet()) > 0) {
                updateNumericInput(
                    inputId = "n",
                    value = length(alphabet())
                )
            }
        })

        observeEvent(input$n, {
            if (isTruthy(input$alphabet)) {
                updateNumericInput(
                    inputId = "n",
                    value = length(alphabet())
                )
            }
        })

        reactive({
            list(
                n = input$n,
                alphabet = get_alphabet(input$n, alphabet()),
                method = str_to_lower(input$method)
            )
        })
    })
}

get_alphabet <- function(n, alphabet) {
    if (is.null(alphabet)) {
        letters[1:n]
    } else {
        alphabet
    }
}
