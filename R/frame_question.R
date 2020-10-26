
#' Create a question with random distractors
#' @export
frame_question  <- function(items, ndistractor = 5, direction = "forward",
                            k = sample(length(items), 1)) {
  base <- items[k,]
  distractors <- items[-k, ] %>%
    filter(group == base$group, id == base$id) %>%
    sample_n(pmin(ndistractor, nrow(.)))

  forward_direction <-
    switch(base$direction,
           "both" = runif(1) > 0.5, # set at random
           "forward" = TRUE,
           "backward" = FALSE)

  res <- if (forward_direction) {
    list(prompt =  base$question,
         lead = base$forward,
         right = base$answer,
         choices = sample(c(base$answer, distractors$answer))
    )
  } else {
    list(prompt =  base$answer,
         lead = base$backward,
         right = base$question,
         choices = sample(c(base$question, distractors$question))
    )
  }

  tmp <- res$choices # still character strings
  res$choices <- as.list(tmp == res$right) # Logical vector: which choice is right

  # Process character strings in tmp, e.g. to latex, as image, ...
  res$right <- format_choices(res$right)
  res$prompt <- format_choices(res$prompt)
  names(res$choices) <- format_choices(tmp)


  return(res)


}

hashbox <- function(id) {
  x = shiny::verbatimTextOutput(id, TRUE)
  x$attribs$style = "white-space: pre-wrap;"
  x
}
