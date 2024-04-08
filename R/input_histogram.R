ui_input_histogram <- function(id) {
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
        ),
        numericInput(
            inputId = NS(id, "bins"),
            label = "Select the number of bins:",
            value = 30,
        )
    )
}

server_input_histogram <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        observeEvent(data(), {
            updateSelectizeInput(
                inputId = "main",
                choices = select(
                    data(),
                    where(function(x) {
                        is.numeric(x) || is.character(x) || is.factor(x)
                    })
                ) |> colnames()
            )

            updateSelectizeInput(
                inputId = "grouping",
                choices = select(
                    data(),
                    where(function(x) {
                        is.character(x) || is.factor(x)
                    })
                ) |> colnames()
            )
        })

        reactive({
            p <- ggplot(data(), aes(.data[[input$main]])) +
                geom_histogram(bins = input$bins)

            if (isTruthy(input$grouping)) {
                p <- p + facet_wrap(vars(.data[[input$grouping]]))
            }

            p +
                theme_minimal() +
                scale_color_viridis_d()
        })
    })
}
