library(shiny)
library(bslib)
library(data.table)
library(purrr)
library(vroom)
library(ggplot2)

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
            selectizeInput(
                inputId = NS(id, "y"),
                label = "Select the main attribute:",
                choices = colnames(iris)
            ),
            selectizeInput(
                inputId = NS(id, "x"),
                label = "Select the secondary attribute:",
                choices = colnames(iris),
                options = list(plugins = "clear_button")
            ),
        ),
        navset_underline(
            nav_panel(
                title = "Table",
                DT::dataTableOutput(
                    outputId = NS(id, "table")
                )
            ),
            nav_panel(
                title = "Boxplot",
                plotOutput(
                    outputId = NS(id, "boxplot")
                )
            ),
            nav_panel(
                title = "Dotplot",
                plotOutput(
                    outputId = NS(id, "dotplot")
                )
            ),
            nav_panel(
                title = "Distribution",
                plotOutput(
                    outputId = NS(id, "distribution")
                )
            )
        )
    )
}

server_input <- function(id) {
    moduleServer(id, function(input, output, session) {
        data <- reactiveVal(iris)

        observeEvent(input$file, {
            data(fread(input$file$datapath))

            updateSelectizeInput(
                inputId = "x",
                choices = colnames(data())
            )

            updateSelectizeInput(
                inputId = "y",
                choices = colnames(data())
            )
        })

        output$table <- DT::renderDataTable(
            {
                data()
            },
            options = list(
                paging = FALSE,
                searching = FALSE,
                scrollX = TRUE,
                scrollY = TRUE
            )
        ) |> bindEvent(data())

        output$boxplot <- renderPlot({
            boxplot(data(), input$x, input$y) +
                theme_minimal() +
                scale_color_viridis_d()
        })

        output$dotplot <- renderPlot({
            dotplot(data(), input$x, input$y) +
                theme_minimal() +
                scale_color_viridis_d()
        })

        output$distribution <- renderPlot({
            distribution(data(), input$x, input$y) +
                theme_minimal() +
                scale_color_viridis_d()
        })

        reactive({
            data()
        })
    })
}

boxplot <- function(data, x, y) {
    if (isTruthy(x)) {
        df <- data.frame(x_ = data[[x]], y_ = data[[y]])
        ggplot(df) +
            geom_boxplot(aes(x = x_, y = y_)) +
            labs(x = x, y = y)
    } else {
        df <- data.frame(y_ = data[[y]])
        ggplot(df) +
            geom_boxplot(aes(y = y_)) +
            labs(y = y)
    }
}

dotplot <- function(data, x, y) {
    if (isTruthy(x)) {
        df <- data.frame(x_ = data[[x]], y_ = data[[y]])
        if (is.numeric(df$x)) {
            ggplot(df) +
                geom_point(aes(x = x_, y = y_)) +
                labs(x = x, y = y)
        } else {
            ggplot(df) +
                geom_point(aes(x = seq_along(y_), y = y_, colour = x_)) +
                labs(x = "", y = y, colour = x)
        }
    } else {
        df <- data.frame(y_ = data[[y]])
        ggplot(df) +
            geom_point(aes(x = seq_along(y_), y = y_)) +
            labs(x = "", y = y)
    }
}

distribution <- function(data, x, y) {
    if (isTruthy(x)) {
        df <- data.frame(x_ = data[[x]], y_ = data[[y]])
        if (is.numeric(df$x_)) {
            ggplot(df, aes(x = x_, y = y_)) +
                geom_density_2d_filled(alpha = 0.7) +
                geom_density_2d(linewidth = 0.25, colour = "black") +
                geom_point() +
                labs(x = x, y = y, fill = "Level")
        } else {
            ggplot(df, aes(x = y_)) +
                geom_density() +
                facet_wrap(vars(x_)) +
                labs(x = y, y = "Density")
        }
    } else {
        df <- data.frame(y_ = data[[y]])
        ggplot(df) +
            geom_density(aes(x = y_)) +
            labs(x = y, y = "Density")
    }
}
