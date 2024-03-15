library(shiny)

ui_classify <- function(id) {
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                tabPanel(
                    title = "Data",
                    ui_classify_data(NS(id, "data")),
                ),
                header = br(),
            ),
            actionButton(
                inputId = NS(id, "apply"),
                label = "Apply"
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    title = "Data",
                    dataTableOutput(
                        outputId = NS(id, "table")
                    )
                ),
                header = br()
            )
        )
    )
}

server_classify <- function(id) {
    moduleServer(id, function(input, output, session) {
        data <- server_classify_data("data")

        output$table <- renderDataTable(
            {
                data()
            },
            options = list(
                pageLength = 10,
                searching = FALSE,
                scrollX = TRUE
            )
        ) |> bindEvent(input$apply)
    })
}
