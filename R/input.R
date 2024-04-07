library(shiny)
library(bslib)
library(data.table)
library(purrr)
library(dplyr)
library(vroom)
library(ggplot2)
library(ggpointdensity)


ui_scatterplot_selection <- function(id, data) {
    list(
        selectizeInput(
            inputId = NS(id, "main"),
            label = "Select the main attribute:",
            choices = select(data, where(is.numeric)) |> colnames()
        ),
        selectizeInput(
            inputId = NS(id, "secondary"),
            label = "Select the secondary attribute:",
            choices = select(
                data,
                where(function(x) is.numeric(x) || is.instant(x))
            ) |> colnames(),
            options = list(plugins = "clear_button")
        ),
        selectizeInput(
            inputId = NS(id, "grouping"),
            label = "Select the grouping attribute:",
            choices = select(
                data,
                where(function(x) is.factor(x) || is.character(x))
            ) |> colnames(),
            options = list(plugins = "clear_button")
        )
    )
}

ui_distribution_selection <- function(id, data) {
    list(
        selectizeInput(
            inputId = NS(id, "main"),
            label = "Select the main attribute:",
            choices = select(data, where(is.numeric)) |> colnames()
        ),
        selectizeInput(
            inputId = NS(id, "secondary"),
            label = "Select the secondary attribute:",
            choices = select(data, where(is.numeric)) |> colnames()
        ),
        selectizeInput(
            inputId = NS(id, "grouping"),
            label = "Select the grouping attribute:",
            choices = select(
                data,
                where(function(x) is.factor(x) || is.character(x))
            ) |> colnames(),
            options = list(plugins = "clear_button")
        )
    )
}

ui_input <- function(id) {
    layout_sidebar(
        border = TRUE,
        border_radius = TRUE,
        sidebar = sidebar(
            width = "33%",
            tab_header("Input and Visualization"),
            fileInput(
                inputId = NS(id, "file"),
                label = "Select a data file:",
                accept = ".csv",
                placeholder = "iris.csv"
            ),
            uiOutput(outputId = NS(id, "var_selection")),
        ),
        navset_underline(
            id = NS(id, "current_tab"),
            nav_panel(
                title = "Table",
                DT::dataTableOutput(outputId = NS(id, "table"))
            ),
            nav_panel(
                title = "Boxplot",
                plotOutput(outputId = NS(id, "boxplot"))
            ),
            nav_panel(
                title = "Scatterplot",
                plotOutput(outputId = NS(id, "scatterplot"))
            ),
            nav_panel(
                title = "Distribution",
                plotOutput(outputId = NS(id, "distribution"))
            )
        )
    )
}

server_input <- function(id) {
    moduleServer(id, function(input, output, session) {
        data <- reactiveVal(iris)
        selected_cols <- reactiveValues(
            boxplot = NULL,
            scatterplot = NULL,
            distribution = NULL
        )

        observeEvent(input$file, {
            data(fread(input$file$datapath) |> coerce_data_frame())
        })

        output$var_selection <- renderUI(
            switch(input$current_tab,
                "Table" = NULL,
                "Boxplot" = ui_boxplot_selection(id, data()),
                "Scatterplot" = ui_scatterplot_selection(id, data()),
                "Distribution" = ui_distribution_selection(id, data()),
            )
        ) |> bindEvent(input$current_tab)

        output$table <- DT::renderDataTable(
            data(),
            options = list(
                paging = FALSE, searching = FALSE,
                scrollX = TRUE, scrollY = TRUE
            )
        ) |> bindEvent(data())

        output$boxplot <- renderPlot({
            req(input$main)
            boxplot(data(), input$main, input$grouping)
        })

        output$scatterplot <- renderPlot({
            req(input$main)
            scatterplot(data(), input$main, input$secondary, input$grouping)
        })

        output$distribution <- renderPlot({
            req(input$main)
            distribution(data(), input$main, input$secondary, input$grouping)
        })

        data
    })
}

boxplot <- function(data, main, grouping) {
    print(paste(main, grouping))
    p <- ggplot(data, aes(y = .data[[main]]))

    if (isTruthy(grouping)) {
        p <- p + aes(x = .data[[grouping]])
    }

    p + geom_boxplot() + theme_minimal()
}

scatterplot <- function(data, main, secondary, grouping) {
    print(paste(main, secondary, grouping))
    p <- ggplot(data, aes(y = .data[[main]]))

    if (isTruthy(secondary)) {
        p <- p + aes(x = .data[[secondary]])
    } else {
        p <- p + aes(x = seq_along(.data[[main]])) + labs(x = "")
    }

    if (isTruthy(grouping)) {
        p <- p + aes(colour = .data[[grouping]])
    }

    p +
        geom_point() +
        theme_minimal() +
        scale_colour_viridis_d()
}

distribution <- function(data, main, secondary, grouping) {
}
