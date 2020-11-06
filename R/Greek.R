#' Greek letter questions
#'
#' `id` field so we can lump different sources together
#' @export
Greek <- function(direction = c("forward", "both", "backward")) {
  direction <- match.arg(direction)
  tibble::tribble(
    ~ answer,  ~ group, ~ question,
    "alpha",  "lower", "$$\\alpha$$",
    "beta",   "lower", "$$\\beta$$",
    "gamma",  "lower", "$$\\gamma$$",
    "delta",  "lower", "$$\\delta$$",
    "epsilon",  "lower", "$$\\epsilon$$",
    "theta",  "lower", "$$\\theta$$",
    "phi",  "lower", "$$\\phi$$",
    "psi",  "lower", "$$\\psi$$",
    "omega",  "lower", "$$\\omega$$",
    "kappa",  "lower", "$$\\kappa$$",
    "nu",  "lower", "$$\\nu$$",
    "mu",  "lower", "$$\\mu$$",
    "eta",  "lower", "$$\\eta$$",
    "rho",  "lower", "$$\\rho$$",
    "tau",  "lower", "$$\\tau$$",
    "lambda",  "lower", "$$\\lambda$$",
    "zeta",  "lower", "$$\\zeta$$",
    "xi",  "lower", "$$\\xi$$",
    "sigma",  "lower", "$$\\sigma$$",

    "Gamma",  "upper", "$$\\Gamma$$",
    "Delta",  "upper", "$$\\Delta$$",
    "Epsilon",  "upper", "$$E$$",
    "Theta",  "upper", "$$\\Theta$$",
    "Phi",  "upper", "$$\\Phi$$",
    "Omega",  "upper", "$$\\Omega$$",
    "Kappa",  "upper", "$$K$$",
    "Nu",  "upper", "$$N$$",
    "Mu",  "upper", "$$M$$",
    "Eta",  "upper", "$$E$$",
    "Rho",  "upper", "$$R$$",
    "Tau",  "upper", "$$T$$",
    "Lambda",  "upper", "$$\\Lambda$$",
    "Zeta",  "upper", "$$Z$$",
    "Xi",  "upper", "$$\\Xi$$",
    "Sigma",  "upper", "$$\\Sigma$$",
    "Psi",  "lower", "$$\\Psi$$",
  ) %>% mutate(id = "Greek",
               direction = direction,
               forward = "Name the Greek letter",
               backward = "Choose the letter named")
}

# What kind of thing is this, e.g. function, vector matrix, first derivative with respect
# to x, 2nd deriv, mixed  partial derivative.

