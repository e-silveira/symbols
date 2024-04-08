ui_input_boxplot <- function(id) {
    list(
        selectizeInput(
            inputId = NS(id, "main"),
            label = "Select the main attribute:",
            choices = NULL
        ),
        selectizeInput(
            inputId = NS(id, "grouping"),
            label = "Select the grouping attribute:",
            choices = NULL,
            options = list(plugins = "clear_button")
        )
    )
}

server_input_boxplot <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        observeEvent(data(), {
            updateSelectizeInput(
                inputId = "main",
                choices = select(data(), where(is.numeric)) |> colnames()
            )

            updateSelectizeInput(
                inputId = "grouping",
                choices = select(data(), where(is.factor)) |> colnames()
            )
        })

        reactive({
            p <- ggplot(data(), aes(y = .data[[input$main]]))

            if (isTruthy(input$grouping)) {
                p <- p + aes(x = .data[[input$grouping]])
            }

            p + geom_boxplot() + theme_minimal()
        })
    })
}
