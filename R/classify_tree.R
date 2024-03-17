library(shiny)
library(rpart)
library(rpart.plot)

ui_classify_tree <- function(id) {
    tagList(
        selectInput(
            inputId = NS(id, "method"),
            label = "Select the splitting method:",
            choices = c("guess", "anova", "poisson", "class", "exp")
        ),
        checkboxInput(
            inputId = "na_action",
            label = "Delete missing observations."
        ),
        numericInput(
            inputId = "minsplit",
            label = "Specify the minimum split size:",
            value = 20
        ),
        numericInput(
            inputId = "minbucket",
            label = "Specify the minimum bucket size:",
            value = round(20 / 3)
        ),
        numericInput(
            inputId = "cp",
            label = "Specify the complexity:",
            value = 0.01,
            step = 0.01
        ),
        numericInput(
            inputId = "maxdepth",
            label = "Specify the maximum depth of the tree:",
            value = 30
        )
    )
}

server_classify_tree <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        reactive({
            target <- colnames(data())[1]
            rpart(
                formula = paste0(target, " ~ ."),
                data = data()
            ) |> rpart.plot()
        })
    })
}
