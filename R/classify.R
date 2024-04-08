library(shiny)
library(bslib)
library(DT)

ui_classify <- function(id) {
    layout_sidebar(
        sidebar = sidebar(
            width = "30%",
            tab_header("Classification"),
            div(
                accordion(
                    open = FALSE,
                    accordion_panel(
                        title = "Selection",
                        ui_classify_data(NS(id, "data")),
                    ),
                    accordion_panel(
                        title = "Tree",
                        ui_classify_tree(NS(id, "tree"))
                    ),
                    header = br(),
                )
            ),
            actionButton(
                inputId = NS(id, "apply"),
                label = "Apply",
            )
        ),
        navset_underline(
            nav_panel(
                title = "Table",
                DT::dataTableOutput(
                    outputId = NS(id, "table")
                )
            ),
            nav_panel(
                title = "Tree",
                plotOutput(
                    outputId = NS(id, "tree")
                )
            ),
            header = br()
        )
    )
}

server_classify <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        attr <- server_classify_data("data", data)
        tree <- server_classify_tree("tree", attr)

        output$table <- DT::renderDataTable(
            {
                attr()
            },
            options = list(
                paging = FALSE,
                searching = FALSE,
                scrollX = TRUE,
                scrollY = TRUE
            )
        ) |> bindEvent(input$apply)

        output$tree <- renderPlot(
            tree()
        ) |> bindEvent(input$apply)
    })
}
