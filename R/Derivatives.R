#' First derivatives
#'
#' @export
Polynomials <- function() {
  tibble::tribble(
    ~ answer,  ~ group, ~ question,
    "$$\\partial_x\\,f(x) \\equiv 0$$" ,   "two",      "$$f(x) \\equiv b$$",
    "$$\\partial_x\\,f(x) \\equiv 0$$" ,   "two",      "$$f(x) \\equiv a$$",
    "$$\\partial_x\\,f(x) \\equiv 0$$" ,   "two",      "$$f(x) \\equiv a + b$$",
    "$$\\partial_x\\,f(x) \\equiv b$$" ,   "two",      "$$f(x) \\equiv a + b x$$",
    "$$\\partial_x\\,f(x) \\equiv a$$" ,   "two",      "$$f(x) \\equiv a x + b$$",
    "$$\\partial_x\\,f(x) \\equiv 2bx$$",   "two",      "$$f(x) \\equiv a + b x^2$$",
    "$$\\partial_x\\,f(x) \\equiv 2ax$$",   "two",      "$$f(x) \\equiv a x^2 + b$$",
    "$$\\partial_x\\,f(x) \\equiv b + 2 c x$$", "two", "$$f(x) \\equiv a + b x + c x^2$$",
    "$$\\partial_x\\,f(x) \\equiv b + c x$$", "two",   "$$f(x) \\equiv a + b x + c x^2 / 2$$",
    "$$\\partial_x\\,f(x) \\equiv 2b + 2c x$$", "two", "$$f(x) \\equiv a + 2b  x + c x^2$$",
    "$$\\partial_x\\,f(x) \\equiv 2b + cx$$", "two",   "$$f(x) \\equiv a + 2b x + c x^2/2$$",
    "$$\\partial_x\\,f(x) \\equiv a + b$$",  "two",    "$$f(x) \\equiv a x + b x$$",
    "$$\\partial_x\\,f(x) \\equiv ax + bx$$", "two",   "$$f(x) \\equiv a x^2/2 + b x^2/2$$",
    "$$\\partial_x\\,f(x) \\equiv b + 2cx + 3dx^2$$",   "three",     "$$f(x) \\equiv a + b x + c x^2 + d x^3$$",
    "$$\\partial_x\\,f(x) \\equiv a+b + 2cx + dx^2$$",  "three",     "$$f(x) \\equiv ax+bx+cx^2+dx^3/3$$",
    "$$\\partial_x\\,f(x) \\equiv bx + 2 c x + 2dx^2$$","three",     "$$f(x) \\equiv a +bx^2/2 + c x^2 + 2 d x^3/3$$",
    "$$\\partial_x\\,f(x) \\equiv 2ax + b + 3cx^2$$",  "three",     "$$f(x) \\equiv a x^2 + b  x + c x^3 + d$$",

    "$$\\partial_x\\,f(x) \\equiv d  $$",  "three",     "$$f(x) \\equiv dd10$$") %>%
    mutate(id = "polynomials",
           forward = "Select the derivative",
           backward = "Select the anti-derivative")

}
