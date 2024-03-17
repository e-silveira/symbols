library(shiny)
library(rpart)
library(rpart.plot)

ui_classify_tree <- function(id) {
    tagList(
        selectInput(
            inputId = NS(id, "method"),
            label = "Select the splitting method:",
            choices = c("anova", "poisson", "class", "exp")
        ),
        numericInput(
            inputId = NS(id, "minsplit"),
            label = "Specify the minimum split size:",
            value = 20
        ),
        numericInput(
            inputId = NS(id, "minbucket"),
            label = "Specify the minimum bucket size:",
            value = round(20 / 3)
        ),
        numericInput(
            inputId = NS(id, "cp"),
            label = "Specify the complexity:",
            value = 0.01,
            step = 0.01
        ),
        numericInput(
            inputId = NS(id, "maxdepth"),
            label = "Specify the maximum depth of the tree:",
            value = 30
        )
    )
}

server_classify_tree <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        observeEvent(input$minsplit, {
            updateNumericInput(
                inputId = "minbucket",
                value = round(input$minsplit / 3)
            )
        })

        observeEvent(input$minbucket, {
            updateNumericInput(
                inputId = "minsplit",
                value = 3 * input$minbucket
            )
        })

        reactive({
            target <- colnames(data())[1]
            rpart(
                formula = paste0(target, " ~ ."),
                data = data(),
                method = input$method,
                control = rpart.control(
                    minsplit = input$minsplit,
                    minbucket = input$minbucket,
                    cp = input$cp,
                    maxdepth = input$maxdepth
                )
            ) |> rpart.plot()
        })
    })
}
