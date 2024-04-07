
ui_input_boxplot <- function(id, data) {
    list(
        selectizeInput(
            inputId = NS(id, "main"),
            label = "Select the main attribute:",
            choices = select(data, where(is.numeric)) |> colnames()
        ),
        selectizeInput(
            inputId = NS(id, "grouping"),
            label = "Select the grouping attribute:",
            choices = select(data, where(is.factor)) |> colnames(),
            options = list(plugins = "clear_button")
        )
    )
}
