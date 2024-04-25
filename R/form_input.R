library(shiny)

user_input_ui <- function(id) {
    tagList(
        fileInput(
            inputId = NS(id, "file"),
            label = "Upload your data.",
            accept = ".csv",
            placeholder = "dengue.csv"
        ),
        selectInput(
            inputId = NS(id, "attr"),
            label = "Select an attribute.",
            choices = NULL,
            selectize = FALSE,
        ),
        selectInput(
            inputId = NS(id, "time"),
            label = "Select the time column.",
            choices = NULL,
            selectize = FALSE,
        ),
    )
}

user_input_server <- function(id) {
    moduleServer(id, function(input, output, session) {

        current_data <- reactiveVal(read.csv("./data/dengue.csv"))

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
                time <- data()[[input$time]] |> as.Date()
            }

            list(time, input$time, attr, input$attr)
        })
    })
}
