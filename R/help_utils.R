helper_ <- function(shiny_tag,
                    content,
                    icon = "circle-info",
                    type = "markdown",
                    colour = "#6B6B6B",
                    ...) {
    helper(
        shiny_tag,
        content = content,
        icon = icon,
        type = type,
        colour = colour,
        ...
    )
}
