library(shiny)
library(rpart)
library(rpart.plot)

ui_classify_tree <- function(id) {
    tagList(
        selectInput(
            inputId = NS(id, "method"),
            label = "What method?",
            choices = c("guess", "anova", "poisson", "class", "exp")
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
