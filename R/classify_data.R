library(shiny)
library(dplyr)

ui_classify_data <- function(id) {
    tagList(
        fileInput(
            inputId = NS(id, "file"),
            label = "What data file?",
            accept = ".csv",
            placeholder = "dengue.csv"
        ),
        selectInput(
            inputId = NS(id, "target"),
            label = "What target?",
            choices = NULL,
        ),
        selectInput(
            inputId = NS(id, "features"),
            label = "What features?",
            choices = NULL,
            multiple = TRUE
        )
    )
}

server_classify_data <- function(id) {
    moduleServer(id, function(input, output, session) {
        current_data <- reactiveVal(read.csv("./data/dengue.csv"))

        data <- reactive({
            if (isTruthy(input$file)) {
                current_data(read.csv(input$file$datapath))
            }

            current_data()
        })

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
            updateSelectInput(
                session,
                "features",
                choices = setdiff(cols(), input$target),
                selected = input$features
            )
        })

        reactive({
            select(data(), all_of(input$features))
        })
    })
}
