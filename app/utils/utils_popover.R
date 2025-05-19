titleWithPopover <- function(title, popover_title, popover_body) {
  htmltools::span(
    class = "popover-title",
    title,
    shiny::icon(
      name = "circle-info",
      style = "cursor: pointer;",
      `data-bs-toggle` = "popover",
      `data-bs-trigger` = "hover",
      title = popover_title,
      `data-bs-content` = popover_body
    )
  )
}

# icon <- function(popover_title, popover_body){
#   shiny::icon(
#     name = "circle-info",
#     style = "cursor: pointer;",
#     `data-bs-toggle` = "popover",
#     `data-bs-trigger` = "hover",
#     title = popover_title,
#     `data-bs-content` = popover_body
#   )}