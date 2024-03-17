helper_ <- function(shiny_tag,
                    content,
                    icon = "circle-info",
                    type = "markdown",
                    colour = "gray",
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
