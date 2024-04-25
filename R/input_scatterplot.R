ui_input_scatterplot <- function(id) {
    list(
        selectizeInput(
            inputId = NS(id, "main"),
            label = "Select the main attribute:",
            choices = NULL
        ),
        selectizeInput(
            inputId = NS(id, "secondary"),
            label = "Select the secondary attribute:",
            choices = NULL,
            options = list(plugins = "clear_button")
        ),
        selectizeInput(
            inputId = NS(id, "grouping"),
            label = "Select the grouping attribute:",
            choices = NULL,
            options = list(plugins = "clear_button")
        )
    )
}

server_input_scatterplot <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        observeEvent(data(), {
            updateSelectizeInput(
                inputId = "main",
                choices = select(data(), where(is.numeric)) |> colnames()
            )

            updateSelectizeInput(
                inputId = "secondary",
                choices = select(
                    data(),
                    where(function(x) is.numeric(x) || is.instant(x))
                ) |> colnames()
            )

            updateSelectizeInput(
                inputId = "grouping",
                choices = select(
                    data(),
                    where(function(x) is.factor(x) || is.character(x))
                ) |> colnames()
            )
        })

        reactive({
            p <- ggplot(data(), aes(y = .data[[input$main]]))

            if (isTruthy(input$secondary)) {
                p <- p + aes(x = .data[[input$secondary]])
            } else {
                p <- p + aes(x = seq_along(.data[[input$main]])) + labs(x = "")
            }

            if (isTruthy(input$grouping)) {
                p <- p + aes(colour = .data[[input$grouping]])
            }

            p + geom_point() + theme_minimal()
        })
    })
}
