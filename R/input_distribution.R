ui_input_distribution <- function(id) {
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

server_input_distribution <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        observeEvent(data(), {
            updateSelectizeInput(
                inputId = "main",
                choices = select(data(), where(is.numeric)) |> colnames()
            )

            updateSelectizeInput(
                inputId = "secondary",
                choices = select(data(), where(is.numeric)) |> colnames()
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
            p <- ggplot(data())

            if (isTruthy(input$secondary)) {
                p <- p +
                    geom_density_2d_filled(aes(
                        x = .data[[input$secondary]],
                        y = .data[[input$main]]
                    ))
            } else {
                p <- p + geom_density(aes(.data[[input$main]]))
            }

            if (isTruthy(input$grouping)) {
                p <- p + facet_wrap(vars(.data[[input$grouping]]))
            }

            p + theme_minimal()
        })
    })
}
