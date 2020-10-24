library(drillr)
library(shiny)
library(shinyjs)
library(learnrhash)

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    withMathJax(),
    # Application title
    titlePanel("Greek Letters"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            htmlOutput("prompt_lead"),
            tags$div(uiOutput("prompt"), style="font-size: 30pt;"),
            uiOutput("feedback"),
            tags$hr(),
            actionButton("check_answer", "Check your answer"),
            actionButton("next_question", "Next ..."),
            tags$hr(),
            textOutput("score"),
            drillr:::hashbox("hash")

        ),
        mainPanel(
            tags$div(uiOutput("choice_buttons"), style="font-size: 20pt;")

        )

    )
)


server <- function(input, output, session) {
    Items <- Greek()  # set the question items
    prompt <- reactiveVal("Getting started.")
    this_question <- reactiveVal(NULL)
    current_feedback <- reactiveVal("")
    n_asked <- reactiveVal(0)
    n_answered <- reactiveVal(0)
    n_correct <- reactiveVal(0)
    current_choices <- reactiveVal(NULL)
    sequence <- make_sequence(nrow(Items), 100)

    observeEvent(input$next_question,
                 {
                     n_asked(n_asked() + 1)
                     k <- sequence[n_asked()]

                     this_question(
                         frame_question(Items,
                                    ndistractor = 5,
                                    forward=rnorm(1) > 0, #NOTE NOTE NOTE
                                    k = k))
                     prompt(this_question()$prompt)
                     current_choices(this_question()$choices)
                     current_feedback("waiting for your answer ...")
                     disable("next_question")
                     enable("check_answer")
                 }, ignoreNULL  = FALSE )
    observeEvent(input$check_answer, {
        if (is.null(input$answer)) return()
        if (input$answer) {
            n_correct(n_correct() + 1)
            current_feedback("Right!")
            disable("answer")
            disable("check_answer")
        } else {
            current_feedback("<span style=\"color: red;\">Sorry</span>")
        }
        enable("next_question")
        n_answered(n_answered() + 1)

    })
    output$prompt_lead <- renderText(HTML("Select the matching letter"))
    output$choice_buttons <- renderUI({
        if (is.null(current_choices())) return()

        radioButtons("answer",  "Select one.",
                     selected = NA,
                     choices = current_choices()
                    )
    })
    output$prompt <- renderUI(withMathJax(prompt()))
    output$score <- renderText(paste0("Score ", n_correct(), "/", n_answered()))
    output$feedback <- renderText(current_feedback())
    output$hash <- renderText({
        if (n_answered() == 0) return("Starting up.")
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
