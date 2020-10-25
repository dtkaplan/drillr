library(drillr)
library(shiny)
library(shinyjs)
library(learnrhash)

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    withMathJax(),
    # Application title
    titlePanel("141Z Drill"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

            tags$div(htmlOutput("feedback"), style="font-size:15pt;"),
            tags$div(uiOutput("right_answer"), style="font-size:15pt;"),
            tags$hr(),
            actionButton("check_answer", "Check your answer"),
            actionButton("next_question", "Next ..."),
            tags$hr(),
            textOutput("score"),
            drillr:::hashbox("hash")

        ),
        mainPanel(
            tags$span(htmlOutput("prompt_lead"), style = "font-size: 15pt;"),
            tags$span(uiOutput("prompt"), style="font-size: 20pt;"),
            tags$div(uiOutput("choice_buttons"), style="font-size: 15pt;")

        )

    )
)


server <- function(input, output, session) {
    Items <- rbind(Polynomials(), Greek())  # set the question items
    prompt <- reactiveVal("Getting started.")
    this_question <- reactiveVal(NULL)
    current_feedback <- reactiveVal("")
    correct_answer <-  reactiveVal("")
    show_correct_answer <- reactiveVal("")
    n_asked <- reactiveVal(0)
    n_answered <- reactiveVal(0)
    n_correct <- reactiveVal(0)
    forward <- reactiveVal(TRUE)
    lead <- reactiveVal("What do you want me to do?")
    current_choices <- reactiveVal(NULL)
    sequence <- make_sequence(nrow(Items), 100)

    observeEvent(input$next_question,
                 {
                     n_asked(n_asked() + 1)
                     k <- sequence[n_asked()]

                     forward(rnorm(1) > 0)
                     this_question(
                         frame_question(Items,
                                    ndistractor = 5,
                                    forward=forward(), #NOTE NOTE NOTE
                                    k = k))
                     # Set the instructions for the prompot
                     lead(ifelse(forward(),
                                 Items[k,]$forward,
                                 Items[k,]$backward))


                     prompt(this_question()$prompt)
                     current_choices(this_question()$choices)
                     correct_answer(this_question()$right)
                     current_feedback("waiting for your answer ...")
                     disable("next_question")
                     enable("check_answer")
                 }, ignoreNULL  = FALSE )

    observeEvent(input$check_answer, {
        if (is.null(input$answer)) return()
        if (input$answer) {
            n_correct(n_correct() + 1)
            current_feedback("Right!")
            show_correct_answer("")
            disable("answer")
            disable("check_answer")
        } else {
            current_feedback("<span style=\"color: red;\">Sorry, it's ...</span>")
            show_correct_answer(correct_answer())
        }
        enable("next_question")
        n_answered(n_answered() + 1)

    })
    output$prompt_lead <- renderText(HTML(lead()))
    output$choice_buttons <- renderUI({
        if (is.null(current_choices())) return()

        withMathJax(radioButtons("answer",  " ",
                     selected = NA,
                     choices = current_choices()
                    ))
    })
    output$title <- renderUI(titlePanel("hello"))
    output$prompt <- renderUI(withMathJax(prompt()))
    output$score <- renderText(paste0("Score ", n_correct(), "/", n_answered()))
    output$feedback <- renderText(current_feedback())
    output$right_answer <- renderUI(withMathJax(show_correct_answer()))
    output$hash <- renderText({
        if (n_answered() == 0) return("Ready for first question.")
        learnrhash::encode_obj(
            tibble(
                lesson = "Greek",
                n_correct = n_correct(),
                n_answered = n_answered(),
                when = Sys.time()
            )
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
