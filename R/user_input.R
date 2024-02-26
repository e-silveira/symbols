library(shiny)

# - [x] Basic functionality.
# - [ ] Handle various file types.
# - [ ] Error handling.

user_input_ui <- function(id) {
    fileInput(
        inputId = NS(id, "file"),
        label = "Upload your data.",
        accept = ".csv",
    )
}

user_input_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        reactive({
            req(input$file)
            read.csv(input$file$datapath)
        })
    })
}
