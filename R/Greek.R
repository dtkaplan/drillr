#' Greek letter questions
#'
#' `id` field so we can lump different sources together
#' @export
Greek <- function() {
  tibble::tribble(
    ~ answer,  ~ group, ~ question,
    "alpha",  "lower", "$$\\alpha$$",
    "beta",   "lower", "$$\\beta$$",
    "gamma",  "lower", "$$\\gamma$$",
    "Gamma",  "upper", "$$\\Gamma$$",
    "Delta",  "upper", "$$\\Delta$$",
    "Lambda", "upper", "$$\\Lambda$$"
  ) %>% mutate(id = "Greek")
}



