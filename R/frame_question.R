
#' Create a question with random distractors
#' @export
frame_question  <- function(items, ndistractor = 5, forward=TRUE,
                            k = sample(length(items), 1)) {
  base <- items[k,]
  distractors <- items[-k, ] %>%
    filter(group == base$group,
           id == base$id,
           answer  != base$answer) %>%
    sample_n(pmin(ndistractor, nrow(.)))

  res <- if (forward) {
    list(prompt =  base$question,
         right = base$answer,
         choices = sample(c(base$answer, distractors$answer))
    )
  } else {
    list(prompt =  base$answer,
         right = base$question,
         choices = sample(c(base$question, distractors$question))
    )
  }

  tmp <- res$choices # still character strings
  res$choices <- as.list(tmp == res$right)
  names(res$choices) <- tmp

  return(res)


}

hashbox <- function(id) {
  x = shiny::verbatimTextOutput(id, TRUE)
  x$attribs$style = "white-space: pre-wrap;"
  x
}
