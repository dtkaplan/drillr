library(drillr)
library(shiny)
library(shinyjs)
library(learnrhash)

quiz_choices <- list()

quiz_choices[["Quiz Derivatives"]] <- D_quiz("forward")
quiz_choices[["Derivative structure"]] <- D_structure("forward")
quiz_choices[["Derivs of Polynomials"]] <- Polynomials("forward")
quiz_choices[["Derivs of Exponentials"]] <- Exponentials("forward")
quiz_choices[["Derivs of Power-law"]] <- Powers("forward")
quiz_choices[["Derivs of Sinusoids"]] <- Sinusoids("forward")
quiz_choices[["Derivatives"]] <- rbind(Sinusoids("forward"), Powers("forward"),
                                       Exponentials("forward"), Polynomials("forward"))
quiz_choices[["Anti-derivs of Polynomials"]] <- Polynomials("backward")
quiz_choices[["Anti-derivs of Exponentials"]] <- Exponentials("backward")
quiz_choices[["Anti-derivs of Power-law"]] <- Powers("backward")
quiz_choices[["Anti-derivs of Sinusoids"]] <- Sinusoids("backward")
quiz_choices[["Anti-derivatives"]] <- rbind(Sinusoids("backward"), Powers("backward"),
                                       Exponentials("backward"), Polynomials("backward"))
quiz_choices[["Image test"]] <- Test_images("both")
quiz_choices[["Greek"]] <- Greek("both")

# Define UI for application that draws a histogram
ui <- tagList(
    useShinyjs(),
    withMathJax(),
    # Application title

    tagList(
        selectInput("quiz", "Choose quiz:", choices = names(quiz_choices)),
        tags$span(htmlOutput("prompt_lead"), style = "font-size: 12pt;"),
        tags$span(uiOutput("prompt"), style="font-size: 15pt;"),
        tags$div(uiOutput("choice_buttons"), style="font-size: 12pt;"),
        tags$hr(),
        tags$div(htmlOutput("feedback"), style="font-size:12pt;"),
        tags$div(uiOutput("right_answer"), style="font-size:15pt;"),
        tags$div(
            #actionButton("check_answer", "Check your answer"),
            actionButton("next_question", "Next ..."),
            tags$hr(),
            textOutput("score"),
            tags$div(
                drillr:::hashbox("hash"),
                style="font-size:6pt;"
            )
        )
    )

)


server <- function(input, output, session) {
    Items <- eventReactive(input$quiz, {
        correct_answer() # blank it out
        current_feedback() # blank it out
        show_correct_answer("") # blank it out
        n_asked(1)
        n_answered(0)
        n_correct(0)
        quiz_choices[[input$quiz]]
      })
    observeEvent(input$quiz, {
        new_question() # When quiz changes, redraw the question
      },
      priority = 0)
    prompt <- reactiveVal("Getting started.")
    this_question <- reactiveVal(NULL)
    current_feedback <- reactiveVal("")
    correct_answer <-  reactiveVal("")
    show_correct_answer <- reactiveVal("")
    n_asked <- reactiveVal(0)
    n_answered <- reactiveVal(0)
    n_correct <- reactiveVal(0)
    lead <- reactiveVal("What do you want me to do?")
    current_choices <- reactiveVal(NULL)
    sequence <- reactive({make_sequence(nrow(Items()), 100)})

    observeEvent(
        input$next_question, {
            correct_answer("")
            new_question()
        }, ignoreNULL  = FALSE )

    new_question <- reactive({
        n_asked(n_asked() + 1)
        k <- sequence()[n_asked()]
        this_question(
            frame_question(Items(),
                           ndistractor = 5,
                           k = k))
        # Set the instructions for the prompt
        lead(this_question()$lead)

        prompt(HTML(this_question()$prompt))
        current_choices(this_question()$choices)
        correct_answer(HTML(this_question()$right))
        current_feedback("")
        updateActionButton(session, "next_question",
                           label = "Waiting for your answer")
        disable("next_question")
        #enable("check_answer")
    })

    observeEvent(input$next_question, {
        show_correct_answer("")
    })

    # observeEvent(input$check_answer, {
    #     if (is.null(input$answer)) return()
    observeEvent(input$answer, {
        if (input$answer) {
            n_correct(n_correct() + 1)
            current_feedback("")
            show_correct_answer("")
            updateActionButton(session, "next_question",
                               label = "<span  style=\"color: green;\">Right!</span> Press to go to next question.")

        } else {
            current_feedback("<span style=\"color: red;\">Sorry, the correct answer is</span>")
            show_correct_answer(correct_answer())
            updateActionButton(session, "next_question",
                               label = "Next")
        }
        disable("answer")

        enable("next_question")

        n_answered(n_answered() + 1)

    })
    output$prompt_lead <- renderText(HTML(lead()))
    output$choice_buttons <- renderUI({
        if (is.null(current_choices())) return()

        Tmp <- withMathJax(radioButtons("answer",  " ",
                     selected = NA,
                     choices = current_choices()
                    ))

        # radioButtons() strips out internal "<" and ">", replacing them
        # with &lt; and &gt; respectively.
        # Undo this whereever it happens
        Tmp <- gsub("&lt;", "<", Tmp) %>%
          gsub("&gt;", ">", .) %>%
          HTML(.)

        3 -4
        Tmp
    })

    output$prompt <- renderUI(withMathJax(prompt()))
    output$score <- renderText(paste0("Score ", n_correct(), "/", n_answered()))
    output$feedback <- renderText(current_feedback())
    output$right_answer <- renderUI(withMathJax(show_correct_answer()))
    output$hash <- renderText({
        if (n_answered() == 0) return("Ready for first question.")
        learnrhash::encode_obj(
            tibble(
                lesson = input$quiz,
                n_correct = n_correct(),
                n_answered = n_answered(),
                when = Sys.time()
            )
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
