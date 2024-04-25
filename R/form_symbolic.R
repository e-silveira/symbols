library(shiny)
library(stringr)

ui_symbolic <- function(id) {
    tagList(
        numericInput(
            inputId = NS(id, "alpha"),
            label = "How many classes?",
            value = 4,
            min = 1,
            max = 26,
        ),
        helpText(paste(
            "By default, classes will be named",
            "after the letters of the alphabet.",
            "However, you may also provide custom class names.",
            "In such cases, the number of classes",
            "will correspond to the number of names you provide."
        )),
        numericInput(
            inputId = NS(id, "compr"),
            label = "What group size?",
            value = 1,
        ),
        helpText(paste(
            "The group size represents the quantity of observations",
            "you wish to aggregate through averaging."
        )),
        selectInput(
            inputId = NS(id, "method"),
            label = "What method?",
            choices = c("SAX", "dwSAX", "qSAX"),
        ),
        helpText(paste(
            "The SAX method calculates breakpoints",
            "assuming the normality of the data,",
            "while the dwSAX and qSAX methods determine breakpoints",
            "based on the distribution of the data."
        ))
    )
}

server_symbolic <- function(id) {
    moduleServer(id, function(input, output, session) {
        reactive({
            list(
                alpha = input$alpha,
                compr = input$compr,
                method = str_to_lower(input$method)
            )
        })
    })
}
