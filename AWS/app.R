library(shiny)
library(dplyr)
library(tuneR)  # required for AWS Polly

# Package installation instruction from cloudyR github
# install.packages("aws.iam", repos = c(cloudyr = "http://cloudyr.github.io/drat", 
#                                       getOption("repos")))
# install.packages("aws.comprehend", repos = c(cloudyr = "http://cloudyr.github.io/drat",
#                                             getOption("repos")))

library(aws.iam)  # AWS Identity and Access Management
library(aws.comprehend)
library(aws.polly)

ui <- fluidPage(

    # Application title
    titlePanel("cloudyR Examples"),
    
    sidebarLayout(
        sidebarPanel(
            textInput("awsKeyID", "AWS Access Key ID", value=""),
            passwordInput("awsSecretKey", "AWS Secret Acess Key", value=""),
            textInput("awsRegion", "AWS Default Region", value="us-east-1"),
            fluidRow(
                splitLayout(
                    cellWidths = c("2%", "30%", "50%"),
                    "",
                    actionButton("login", "Login"),
                    htmlOutput("loginMsgOutput")
                )
            ),
            br(), br(),
            selectInput("awsService", "AWS Service", 
                        choices=c("AWS Comprehend", "AWS Polly"),
                        selected="AWS Comprehend"
            ),
            uiOutput("awsFuncOutput"),
            uiOutput("awsPollyLangOutput"),
            uiOutput("awsPollyVoiceOutput"),
            textAreaInput("userInput", "Input text for evaluation", 
                          value="This is a French sentence."),
            actionButton("eval", "Evaluate"),
            width=4  # out of 12
        ),

        mainPanel(
            h3(strong("Results")),
            h3(textOutput("resultsTextOutput")),
            tableOutput("resultsTableOutput"),
            width=8  # out of 12
        )
    )
    
)
    

server <- function(input, output, session) {
    observeEvent(input$login, {
        updateTextInput(session, "awsKeyID")
        updateTextInput(session, "awsSecretKey")
        updateTextInput(session, "awsRegion")
        Sys.setenv("AWS_ACCESS_KEY_ID" = input$awsKeyID,
                   "AWS_SECRET_ACCESS_KEY" = input$awsSecretKey,
                   "AWS_DEFAULT_REGION" = input$awsRegion
        )
        tryCatch({
            keys <- aws.iam::list_keys()[[1]]
            if (keys$AccessKeyId == input$awsKeyID) {
                output$loginMsgOutput <- renderText(
                    paste("<font color=\"blue\">", "Login successful", "</font>")
                )
            }
            },
            warning = function(c) {
                output$loginMsgOutput <- renderText(
                    paste("<font color=\"red\">", "Unsuccessful login", "</font")
                )
            }
        )
    })
    
    observe({
        if (input$awsService == "AWS Comprehend") {
            output$awsFuncOutput <- renderUI({
                selectInput("awsFuncInput", "Function type",
                            choices=c("DetectDominantLanguage", "DetectSentiment"),
                            selected="DetectDominantLanguage")
            })
        }
        else if (input$awsService == "AWS Polly") {
            output$awsFuncOutput <- renderUI({
                selectInput("awsFuncInput", "Function type",
                            choices=c("SynthesizeSpeech"),
                            selected="SynthesizeSpeech")
            })
            if (input$awsFuncInput == "SynthesizeSpeech") {
                polly <- pollyHTTP(action="voices")[2]$Voices
                output$awsPollyLangOutput <- renderUI({
                    selectInput("awsPollyLangInput", "Select language",
                                choices=sort(unique(polly$LanguageName)),
                                selected="US English")
                })
                output$awsPollyVoiceOutput <- renderUI({
                    # if statement to prevent initial error about empty results
                    if (is.null(input$awsPollyLangInput)) {return(NULL)}
                    voices <- (polly %>% filter(LanguageName == input$awsPollyLangInput))$Id
                    selectInput("awsPollyVoiceInput", "Select voice",
                                choices=sort(unique(voices)),
                                selected=voices[1])
                })
            }
        }
    })
    

    observeEvent(input$eval, {
        output$resultsTextOutput <- NULL
        output$resultsTableOutput <- NULL
        updateTextAreaInput(session, "userInput")
        
        if (input$awsService == "AWS Comprehend") {
            if (input$awsFuncInput == "DetectDominantLanguage") {
                langResults <- detect_language(isolate(input$userInput))
                output$resultsTableOutput <- renderTable({
                    resultsTable <- matrix(c(langResults["LanguageCode"][,1],
                                             langResults["Score"][,1]), ncol=2)
                    colnames(resultsTable) <- c("Language", "Score")
                    resultsTable
                })
            }
            
            else if (input$awsFuncInput == "DetectSentiment") {
                senResults <- detect_sentiment(isolate(input$userInput))
                output$resultsTextOutput <- renderText({
                    paste("Sentiment is", toString(senResults["Sentiment"][,1]))
                })
                output$resultsTableOutput <- renderTable({
                    resultsTable <- matrix(c("Mixed", senResults["Mixed"][,1],
                                             "Negative", senResults["Negative"][,1],
                                             "Neutral", senResults["Neutral"][,1],
                                             "Positive", senResults["Positive"][,1]),
                                           ncol=2, byrow=TRUE)
                    colnames(resultsTable) <- c("Sentiment", "Score")
                    resultsTable
                })
            }
        }
        
        else if (input$awsService == "AWS Polly") {
            if (input$awsFuncInput == "SynthesizeSpeech") {
                play(synthesize(input$userInput, voice=input$awsPollyVoiceInput))
            }
        }
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
