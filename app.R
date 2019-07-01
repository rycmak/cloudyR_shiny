library(shiny)
library(dplyr)

# Package installation instruction from cloudyR github
# install.packages("aws.iam", repos = c(cloudyr = "http://cloudyr.github.io/drat", 
#                                       getOption("repos")))
# install.packages("aws.comprehend", repos = c(cloudyr = "http://cloudyr.github.io/drat",
#                                             getOption("repos")))

library(aws.iam)  # AWS Identity and Access Management
library(aws.comprehend)

ui <- fluidPage(

    # Application title
    titlePanel("cloudyR Examples"),
    
    sidebarLayout(
        sidebarPanel(
            textInput("awsKeyID", "AWS Access Key ID", value="Enter access key ID"),
            passwordInput("awsSecretKey", "AWS Secret Acess Key", value="Enter secret access key"),
            textInput("awsRegion", "AWS Default Region", value="us-east-1"),
            fluidRow(
                splitLayout(
                    cellWidths = c("5%", "30%", "50%"),
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
            selectInput("awsFunc", "Function type",
                        choices=c("detect_language", "detect_sentiment"),
                        selected="detect_language"),
            textAreaInput("userInput", "Input text for evaluation", 
                          value="This is a French sentence."),
            actionButton("eval", "Evaluate"),
            width=4  # out of 12
        ),

        mainPanel(
            h3(strong("Results")),
            h3(textOutput("resultsTextOutput")),
            tableOutput("resultsOutput"),
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
            

    observeEvent(input$eval, {
        updateTextAreaInput(session, "userInput")
        
        if (input$awsFunc == "detect_language") {
            print("in detect_language")
            langResults <- detect_language(isolate(input$userInput))
            output$resultsOutput <- renderTable({
                resultsTable <- matrix(c(langResults["LanguageCode"][,1],
                                         langResults["Score"][,1]), ncol=2)
                colnames(resultsTable) <- c("Language", "Score")
                resultsTable
            })
            print("detect_language")
        }

        else if (input$awsFunc == "detect_sentiment") {
            senResults <- detect_sentiment(isolate(input$userInput))
            output$resultsTextOutput <- renderText({
                paste("Sentiment is", toString(senResults["Sentiment"][,1]))
            })
            output$resultsOutput <- renderTable({
                resultsTable <- matrix(c("Mixed", senResults["Mixed"][,1],
                                         "Negative", senResults["Negative"][,1],
                                         "Neutral", senResults["Neutral"][,1],
                                         "Positive", senResults["Positive"][,1]),
                                       ncol=2, byrow=TRUE)
                colnames(resultsTable) <- c("Sentiment", "Score")
                resultsTable
            })
            #print("detect_sentiment")
        }
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
