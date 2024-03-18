library(shiny)
library(dplyr)

ui_classify_data <- function(id) {
    tagList(
        fileInput(
            inputId = NS(id, "file"),
            label = "Upload the data file:",
            accept = ".csv",
            placeholder = "iris.csv"
        ),
        selectInput(
            inputId = NS(id, "target"),
            label = "Select the target attribute:",
            choices = NULL,
        ),
        selectInput(
            inputId = NS(id, "features"),
            label = "Select the features:",
            choices = NULL,
            multiple = TRUE
        )
    )
}

server_classify_data <- function(id) {
    moduleServer(id, function(input, output, session) {
        current_data <- reactiveVal(iris)

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
            select(data(), all_of(c(input$target, input$features)))
        })
    })
}
