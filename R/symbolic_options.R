library(shiny)

symbolic_options_ui <- function(id) {
    tagList(
        numericInput(
            inputId = NS(id, "alphabet_size"),
            label = "Specify an alphabet size.",
            value = 4,
        ),
        numericInput(
            inputId = NS(id, "compression_rate"),
            label = "Specify a compression rate.",
            value = 1,
        ),
        selectInput(
            inputId = NS(id, "method"),
            label = "Specify the method.",
            choices = c("sax", "dwsax", "qsax"),
        ),
    )
}

symbolic_options_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        reactive({
            list(
                alphabet_size = input$alphabet_size,
                crate = input$compression_rate,
                method = input$method
            )
        })
    })
}
