#' First derivatives
#'
#' @export
Polynomials <- function(direction = c("forward", "both", "backward")) {
  direction <- match.arg(direction)
  tibble::tribble(
    ~ answer,  ~ group, ~ question,
    "$$\\partial_x\\,f(x) \\equiv 0$$" ,          "two",      "$$f(x) \\equiv b$$",
    "$$\\partial_x\\,f(x) \\equiv 0$$" ,          "two",      "$$f(x) \\equiv a$$",
    "$$\\partial_x\\,f(x) \\equiv 0$$" ,          "two",      "$$f(x) \\equiv a + b$$",
    "$$\\partial_x\\,f(x) \\equiv b$$" ,          "two",      "$$f(x) \\equiv a + b x$$",
    "$$\\partial_x\\,f(x) \\equiv a$$" ,          "two",      "$$f(x) \\equiv a x + b$$",
    "$$\\partial_x\\,f(x) \\equiv 2bx$$",         "two",      "$$f(x) \\equiv a + b x^2$$",
    "$$\\partial_x\\,f(x) \\equiv 2ax$$",         "two",      "$$f(x) \\equiv a x^2 + b$$",
    "$$\\partial_x\\,f(x) \\equiv b + 2 c x$$",   "two",      "$$f(x) \\equiv a + b x + c x^2$$",
    "$$\\partial_x\\,f(x) \\equiv b + c x$$",     "two",      "$$f(x) \\equiv a + b x + c x^2 / 2$$",
    "$$\\partial_x\\,f(x) \\equiv 2b + 2c x$$",   "two",      "$$f(x) \\equiv a + 2b  x + c x^2$$",
    "$$\\partial_x\\,f(x) \\equiv 2b + cx$$",     "two",      "$$f(x) \\equiv a + 2b x + c x^2/2$$",
    "$$\\partial_x\\,f(x) \\equiv a + b$$",       "two",      "$$f(x) \\equiv a x + b x$$",
    "$$\\partial_x\\,f(x) \\equiv ax + bx$$",     "two",      "$$f(x) \\equiv a x^2/2 + b x^2/2$$",
    "$$\\partial_x\\,f(x) \\equiv b + 2cx + 3dx^2$$",       "three",     "$$f(x) \\equiv a + b x + c x^2 + d x^3$$",
    "$$\\partial_x\\,f(x) \\equiv a+b + 2cx + dx^2$$",      "three",     "$$f(x) \\equiv ax+bx+cx^2+dx^3/3$$",
    "$$\\partial_x\\,f(x) \\equiv bx + 2 c x + 2dx^2$$",    "three",     "$$f(x) \\equiv a +bx^2/2 + c x^2 + 2 d x^3/3$$",
    "$$\\partial_x\\,f(x) \\equiv 2ax + b + 3cx^2$$",       "three",     "$$f(x) \\equiv a x^2 + b  x + c x^3 + d$$",
    "$$\\partial_x\\,f(x) \\equiv a + 3cx^2 - 2dx$$",       "three",     "$$f(x) \\equiv ax + b + cx^3 - dx^2$$",
    "$$\\partial_x\\,f(x) \\equiv ax^2 + b + 4cx +16dx^3$$","three",     "$$f(x) \\equiv ax^3/3 + bx + 2cx^2 + 4dx^4$$",
    "$$\\partial_x\\,f(x) \\equiv 4dx^3  $$",               "three",     "$$f(x) \\equiv 4b + dx^4$$",
    "$$\\partial_x\\,f(x) \\equiv c + 2cx$$",               "three",     "$$f(x) \\equiv cx + cx^2$$",
    "$$\\partial_x\\,f(x) \\equiv d  $$",                   "three",     "$$f(x) \\equiv cx + cx^2$$",
    "$$\\partial_x\\,f(x) \\equiv 2cbx $$",                 "three",     "$$f(x) \\equiv c(a + bx^2)$$") %>%
    mutate(id = "polynomials",
           direction = direction,
           forward = "Select the derivative",
           backward = "Select the anti-derivative")

}

#' @export
Exponentials <- function(direction = c("forward", "both", "backward")) {
  direction <- match.arg(direction)
  tibble::tribble(
    ~ answer,  ~ group, ~ question,
    "$$d_t\\, f(t) \\equiv k \\exp(kt )$$",            "exponential",   "$$f(t) \\equiv \\exp(kt)$$",
    "$$d_t\\, f(t) \\equiv -k \\exp(-kt )$$",          "exponential",   "$$f(t) \\equiv \\exp(-kt )$$",
    "$$d_t\\, f(t) \\equiv k\\exp(-kt )$$",            "exponential",   "$$f(t) \\equiv -\\exp(-kt )$$",
    "$$d_t\\, f(t) \\equiv - \\exp( kt)$$",            "exponential",   "$$f(t) \\equiv -\\exp( kt)$$",
    "$$d_t\\, f(t) \\equiv \\exp( t)$$",               "exponential",   "$$f(t) \\equiv \\exp( t)$$",
    "$$d_t\\, f(t) \\equiv k \\exp( t)$$",             "exponential",   "$$f(t) \\equiv k \\exp(t )$$",
    "$$d_t\\, f(t) \\equiv -k \\exp( t)$$",            "exponential",   "$$f(t) \\equiv -k \\exp(t)$$",
    "$$d_t\\, f(t) \\equiv -k \\exp(-t )$$",                "exponential",   "$$f(t) \\equiv k\\exp(-t )$$",
    "$$d_t\\, f(t) \\equiv k \\exp(-t )$$",            "exponential",   "$$f(t) \\equiv -k \\exp(-t )$$",
    "$$d_t\\, f(t) \\equiv (1/k) \\exp(t/k)$$",            "exponential",   "$$f(t) \\equiv \\exp(t/k )$$",
    "$$d_t\\, f(t) \\equiv (1/k^2)\\exp(t/k^2 )$$",            "exponential",   "$$f(t) \\equiv \\exp(t/k^2 )$$",
    "$$d_t\\, f(t) \\equiv -(1/k)\\exp(-t/k )$$",            "exponential",   "$$f(t) \\equiv \\exp(-t/k )$$",
    "$$d_t\\, f(t) \\equiv (1/k) \\exp(-t/k )$$",            "exponential",   "$$f(t) \\equiv -\\exp(-t/k )$$",

  ) %>%
    mutate(id = "exponentials",
           direction = direction,
           forward = "Select the derivative",
           backward = "Select the anti-derivative")
}

#' @export
Powers <- function(direction = c("forward", "both", "backward")) {
  direction <- match.arg(direction)
  tibble::tribble(
    ~ answer,  ~ group, ~ question,
    "$$d_t\\, f(t) \\equiv 3 x^2$$",            "powers",   "$$f(t) \\equiv x^3$$",
    "$$d_t\\, f(t) \\equiv -3 x^{-4}$$",            "powers",   "$$f(t) \\equiv x^{-3}$$",
    "$$d_t\\, f(t) \\equiv 2 x^{1}$$",            "powers",   "$$f(t) \\equiv x^2$$",
    "$$d_t\\, f(t) \\equiv 2 x^1$$",            "powers",   "$$f(t) \\equiv x^{-2}$$",
    "$$d_t\\, f(t) \\equiv -1 x^{-2}$$",            "powers",   "$$f(t) \\equiv x^1$$",
    "$$d_t\\, f(t) \\equiv \\ln(x)$$",             "powers",   "$$f(t) \\equiv x^{-1} $$",
    "$$d_t\\, f(t) \\equiv 0$$",             "powers",   "$$f(t) \\equiv x^0$$",
    "$$d_t\\, f(t) \\equiv 2 x^2$$",            "powers",   "$$f(t) \\equiv 2 x^3 / 3$$",
    "$$d_t\\, f(t) \\equiv 3 x$$",                "powers",   "$$f(t) \\equiv 3 x^2 /2$$",
    "$$d_t\\, f(t) \\equiv 0.5 x^{-0.5}$$",            "powers",   "$$f(t) \\equiv \\sqrt{x}$$",
    "$$d_t\\, f(t) \\equiv 1.5 x^{0.5}$$",        "powers",   "$$f(t) \\equiv x^{1.5} $$",
    "$$d_t\\, f(t) \\equiv x^{-2.5} $$",    "powers",   "$$f(t) \\equiv x^{-1.5}/1.5 $$",
    "$$d_t\\, f(t) \\equiv x^{1.5}$$",      "powers",   "$$f(t) \\equiv x^{2.5}/2.5$$",
    "$$d_t\\, f(t) \\equiv 5 x^{-3.5}$$",      "powers",   "$$f(t) \\equiv -2 x^{-2.5}$$",

  ) %>%
    mutate(id = "powers",
           direction = direction,
           forward = "Select the derivative",
           backward = "Select the anti-derivative")
}

#' @export
Sinusoids <- function(direction = c("forward", "both", "backward")) {
  direction <- match.arg(direction)
  tibble::tribble(
    ~ answer,  ~ group, ~ question,
    "$$d_t\\, f(t) \\equiv a\\sin(at)$$",          "trig",   "$$f(t) \\equiv \\sin(at)$$",
    "$$d_t\\, f(t) \\equiv a\\cos(t)$$",           "trig",   "$$f(t) \\equiv \\sin(t)$$",
    "$$d_t\\, f(t) \\equiv -a\\sin(at)$$",         "trig",   "$$f(t) \\equiv \\cos(at)$$",
    "$$d_t\\, f(t) \\equiv a\\sin(t)$$",           "trig",   "$$f(t) \\equiv a \\cos(t)$$",
    "$$d_t\\, f(t) \\equiv a\\cos(t)$$",           "trig",   "$$f(t) \\equiv a \\sin(t)$$",
    "$$d_t\\, f(t) \\equiv -a^2 \\sin(at)$$",      "trig",   "$$f(t) \\equiv -a \\sin(at)$$",
    "$$d_t\\, f(t) \\equiv -a\\sin(t)$$",          "trig",   "$$f(t) \\equiv -a \\cos(t)$$",
    "$$d_t\\, f(t) \\equiv -a\\cos(at)$$",         "trig",   "$$f(t) \\equiv -\\sin(at)$$",
    "$$d_t\\, f(t) \\equiv - \\sin(-t)$$",         "trig",   "$$f(t) \\equiv -\\cos(-t)$$",
    "$$d_t\\, f(t) \\equiv \\sin(-t)$$",           "trig",   "$$f(t) \\equiv \\cos(-t)$$",
    "$$d_t\\, f(t) \\equiv - \\cos(-t)$$",         "trig",   "$$f(t) \\equiv \\sin(-t)$$",
  ) %>%
    mutate(id = "trig",
           direction = direction,
           forward = "Select the derivative",
           backward = "Select the anti-derivative")
}

#' @export
Test_images <- function(direction = c("forward", "both", "backward")) {
  direction <- match.arg(direction)
  tibble::tribble(
    ~ answer, ~ group, ~ question,
    "<img src='https://upload.wikimedia.org/wikipedia/commons/8/83/Sir_Isaac_Newton_%281643-1727%29.jpg' width = 200>", "", "Leibniz",
    "Abraham Lincoln", "", "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/b/b6/Gilbert_Stuart_Williamstown_Portrait_of_George_Washington.jpg/440px-Gilbert_Stuart_Williamstown_Portrait_of_George_Washington.jpg' width=200>",
    "<img src='https://image.shutterstock.com/image-photo/no-limits-message-on-asphalt-260nw-198889661.jpg' width=200>", "", "Math 141Z"
  ) %>%
    mutate(id = "images",
           direction = direction,
           forward = "Which matches?",
           backward = "Which matches backward?")
}

#' @export
D_structure <- function(direction = c("forward", "both", "backward")) {
  direction <- match.arg(direction)
  tibble::tribble(
    ~ answer, ~ group, ~ question,
    "0", "all","$$\\partial_x \\exp(t)$$",
    "0", "all","$$\\partial_t \\ln(x)$$",
    "0", "all", "$$\\partial_x  7$$",
    "1", "all", "$$\\partial_x x$$",
    "1", "all", "$$\\partial_y y x^3$$",
    "1", "all", "$$\\partial_x \\exp(x) \\sin(\\omega t)$$",
    "fundamental", "all", "$$\\partial_x 7x$$",
    "fundamental", "all", "$$\\partial_y x \\exp(y)$$",
    "fundamental", "all","$$\\partial_t  \\exp(t)$$",
    "fundamental", "all", "$$\\partial_x \\exp(t)  \\sin(x)$$",
    "fundamental", "all","$$\\partial_t \\sin(t)$$",
    "fundamental", "all","$$\\partial_x x^n (n\\neq 0)",
    "fundamental", "all","$$\\partial_x \\ln(x)$$",
    "fundamental", "all", "$$\\partial_x \\mbox{sigmoid}(x, center, width)$$",
    "scaled fundamental", "all",  "$$\\partial_t \\exp(kt)$$",
    "scaled fundamental", "all", "$$\\partial_t \\sin(2\\pi t/P)$$",
    "scaled fundamental",  "all", "$$\\partial_x \\mbox{sigmoid}(4 x, center, width)$$",
    "scaled fundamental", "all", "$$\\partial_t \\sin(\\omega t)$$",
    "product", "all", "$$\\partial_x \\exp(x) \\sin(ax)$$",
    "product", "all", "$$\\partial_t \\sin(\\omega t)\\cos(\\alpha t)$$",
    "product", "all", "$$\\partial_x x^3  \\sin(k x)$$",
    "product", "all", "$$\\partial_x sin(\\omega x) \\ln(x)$$",
    "product", "all", "$$\\partial_x x \\exp(x)$$",
    "product", "all", "$$\\partial_x (x^2  + 1)  \\exp(kx)$$",
    "product", "all", "$$\\partial_t x \\cos(t) \\sin(\\omega t)$$",
    "quotient", "all", "$$\\partial_x sin(x)/cos(x)$$",
    "chain",  "all",  "$$\\partial_x \\sin(x^2)$$",
    "chain",  "all",  "$$\\partial_x \\ln(x^2 + x)$$",
    "linear combination", "all", "$$\\partial_x (x + 3)$$",
    "linear combination", "all", "$$\\partial_x (a_0 + a_1 x + a_2 x^2)$$",
    "linear combination", "all", "$$\\partial_x (2 x^2 - sin(\\omega x))$$",
    "$$g(x) \\partial_x f(x) + f(x) \\partial_x g(x)$$", "rule", "$$\\partial_x \\left[f(x) g(x)\\right]$$",
    "$$g(x) \\partial_x f(x) + \\partial_x g(x)$$", "rule", "$$\\partial_x \\left[f(x) + g(x)\\right]$$",
    "$$g(x) f'(g(x)) g'(x)$$", "rule", "$$\\partial_x f(g(x))$$",
    "$$g(x) a f'(a g(x)) g'(a x)$$", "rule", "$$\\partial_x f(a  g(x))$$",
    "$$g(x) g'(f(x)) f'(x)$$", "rule", "$$\\partial_x g(f(x))$$",
    "$$g(x) a f'(a x)$$", "rule", "$$\\partial_x f(a x + b) $$",
    "$$g(x) a \\partial_x f(x) + b \\partial_x g(x)$$", "rule", "$$\\partial_x \\left[a f(x) + b g(x)\\right]$$",
    "$$g(x) b \\partial_x f(x) + a \\partial_x g(x)$$", "rule", "$\\partial_x \\left[b f(x) + a g(x)\\right]$$",

    ) %>%
    mutate(id = "structure",
           direction = direction,
           forward = "Which structure matches the function?",
           backward = "Which  function  matches the structure?")
}


#' @export
#' @export
D_quiz <- function(direction = c("forward", "both", "backward")) {
  direction <- match.arg(direction)
  tibble::tribble(
    ~ answer, ~ group, ~ question,
    "$$7 \\cos(x) - 2/x$$", "quizx1",
         "$$\\partial_x   \\left[ 7 \\sin(x) - 2 \\ln(x)\\right]$$",
    "$$7 \\cos(7x) - 1/x$$", "quizx1",
         "$$\\partial_x   \\left[ \\sin(7 x) -  \\ln(2 x)\\right]$$",
    "$$7 \\cos(7x) - 1/x $$", "quizx1", "$$\\partial_x   \\left[ \\sin(7 x) -  \\ln(x/2)\\right]$$",
    "$$7 \\cos(x) - 2/x $$", "quizx1",  "$$\\partial_x \\left[7 \\sin(x) -  2 \\ln(x/2)\\right]$$",
    "$$-4 \\sqrt{6}  / x^5$$", "quizx2", "$$\\partial_x \\left[ \\sqrt{6} / x^4  \\right]$$",
    "$$4 \\sqrt{6}  / x^5$$", "quizx2", "$$\\partial_x \\left[ - \\sqrt{6} / x^4  \\right]$$",
    "$$4 \\sqrt{6} / x^4$$", "quizx2", "$$\\partial_x \\left[ -(4/3) \\sqrt{6} / x^3\\right]$$",
    "$$4 \\sqrt{6} / x^6$$", "quizx2",
         "$$\\partial_x \\left[ -(4/5) \\sqrt{6} / x^5\\right]$$",
     "$$\\log(4)\\log(3) 4^x$$" ,  "quizx3", "$$\\partial_x \\log(3) 4^x$$",
     "$$\\log(4)\\log(3) 3^x$$", "quizx3",  "$$\\partial_x \\log(4) 3^x$$",
     "$$3 \\log(4) x^2$$", "quizx3",    "$$\\partial_x \\log(4) x^3$$",
    "$$4 \\log(3) x^3$$", "quizx3",    "$$\\partial_x \\log(3) x^4$$",
    "$$- 75 \\sin(x)$$",  "quizx4",  "$$\\partial_x \\left[75 \\cos(x)  \\right]$$",
    "$$75 \\cos(x)$$",  "quizx4",  "$$\\partial_x \\left[75 \\sin(x)  \\right]$$",
    "$$75 \\sin(x)$$",  "quizx4",  "$$\\partial_x \\left[- 75 \\cos(x)\\right]$$",
    "$$-75 \\cos(x)$$",  "quizx4",  "$$\\partial_x \\left[- 75 \\sin(x)\\right]$$",
    "$$- 75 \\sin(75 x)$$",  "quizx4",  "$$\\partial_x \\left[\\cos(75 x)  \\right]$$",
    "$$75 \\cos(75 x)$$",  "quizx4",  "$$\\partial_x \\left[\\sin(75 x)  \\right]$$",
    "$$75 \\sin(75 x)$$",  "quizx4",  "$$\\partial_x \\left[-\\cos(75 x)\\right]$$",
    "$$-75 \\cos(75 x)$$",  "quizx4",  "$$\\partial_x \\left[-\\sin(75 x)\\right]$$",
    "$$2 - (1/9)/x^2  $$", "quizx5", "$$\\partial_x\\left[2 x + 1/(9x)\\right]$$",
    "$$2 + (1/9)/x^2  $$", "quizx5", "$$\\partial_x\\left[2 x - 1/(9x)\\right]$$",
    "$$2x$$", "quizx5", "$$\\partial_x\\left[x^2 + 1/(9k)\\right]$$",
    "$$2x + 9  $$", "quizx5", "$$\\partial_x\\left[x^2 + 9x\\right]$$",
    "$$\\log(2) 2^x - (1/9)/x^2$$", "quizx5", "$$\\partial_x\\left[2^x + 1/(9x)\\right]$$",
    "$$\\log(2) 2^x + (1/9)/x^2$$", "quizx5", "$$\\partial_x\\left[2^x - 1/(9x)\\right]$$",
    "$$2x  - (1/81)/x^2$$", "quizx5", "$$\\partial_x\\left[x^2 +  1/(81 x)\\right]$$",
    "$$2x - (1/3)/x^{3}  $$", "quizx5", "$$\\partial_x\\left[2 x + 1/(3x)^2\\right]$$",

  ) %>%
    mutate(id = "quiz_practice",
           direction = direction,
           forward = "Which is the derivative of the function?",
           backward = "Which is an anti-derivative of the function?")
}
