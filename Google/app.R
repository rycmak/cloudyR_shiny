library(shiny)
library(googleAuthR)

# Install Google Cloud Vision for image analysis
# Authentication info: https://cloud.google.com/vision/docs/auth
pkgs <- c("googleCloudVisionR", "googleLanguageR")
for (pkg in pkgs) {
  if (!require(pkg, character.only=TRUE)) install.packages(pkg)
}

# Install some other packages that might be needed, and call their libraries
pkgs <- c("leaflet", "imager", "tuneR")
for (pkg in pkgs) {
  if (!require(pkg, character.only=TRUE)) install.packages(pkg)
  library(pkg, character.only=TRUE)
}


ui <- fluidPage(
  
  # Application title
  titlePanel("cloudyR Examples with Google Cloud"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("GoogleJSONFile", "Google Key",
                accept=".json", placeholder="Google service account JSON file"
      ),
      selectInput("GoogleService", "Google Service",
                  choices=c("Cloud Vision", "Cloud Text-to-Speech"),
                  selected="Cloud Vision"
      ),
      
      # following UI outputs if "Cloud Vision" is selected
      uiOutput("dataFile"),
      uiOutput("imageFeature"),
      uiOutput("maxNumResults"),
      
      # following UI outputs if "Cloud Text-to-Speech" is selected
      uiOutput("languageOutput"),
      uiOutput("genderOutput"),
      uiOutput("textOutput"),
      
      uiOutput("evalButton")  # "Evaluation" button
    ),
    
    mainPanel(
      h2(strong("Results")),
      textOutput("speechMsgOutput"),
      imageOutput("imageOutput"),
      br(), br(),
      h3(textOutput("resultsMsgOutput")),
      fluidRow(
        splitLayout(
          cellWidths=c("25%", "75%"),
          tableOutput("resultsTableOutput"),
          leafletOutput("mapOutput")
        )
      )
    )
  )
)
  
server <- function(input, output, session) {
  
  observeEvent(input$GoogleJSONFile$datapath, {
    # Authentication with Sys.setenv works on command line but not with Shiny:
    #Sys.setenv("GCV_AUTH_FILE"=toString(input$GoogleJSONFile$datapath))
    # Using googleAuthR library to authenticate instead:
    gar_auth_service(input$GoogleJSONFile$datapath)
    
    # Load Google Cloud libraries
    library("googleCloudVisionR")
    library("googleLanguageR")
  })
  
  observeEvent(input$GoogleService, {
    
    if (input$GoogleService == "Cloud Vision") {
      
      # clear inputs irrelevant to "Vision" service
      output$languageOutput <- NULL
      output$genderOutput <- NULL
      output$textOutput <- NULL
      output$speechMsgOutput <- NULL
      
      output$dataFile <- renderUI({
        fileInput("imageFile", "Image file", 
                  accept=c(".jpg", ".png")
        )
      })
      
      output$imageFeature <- renderUI({
        # feature can be one of: 'LABEL_DETECTION', 'TEXT_DETECTION',
        # 'DOCUMENT_TEXT_DETECTION', 'FACE_DETECTION', 'LOGO_DETECTION',
        # 'LANDMARK_DETECTION'
        selectInput("imageFeature", "Feature",
                    choices=c("Label detection", "Landmark detection", 
                              "Face detection", "OCR", "Multiple-object detection"),
                    selected="Label detection")
      })
      output$maxNumResults <- renderUI({
        numericInput("maxResults", "Max features (1 to 20)", 5, min=1, max=20, step=1)
      })
      
      output$evalButton <- renderUI({
        # only display "Evaluate" button when input image file has been set
        if (!is.null(input$imageFile)) {
          actionButton("eval", "Evaluate")
        }
      })
    }
    
    else if (input$GoogleService == "Cloud Text-to-Speech") {
      
      # clear inputs irrelevant to "Text-to-Speech" service
      output$dataFile <- NULL
      output$imageFeature <- NULL
      output$maxNumResults <- NULL
      output$imageOutput <- NULL
      output$resultsTableOutput <- NULL
      output$resultsMsgOutput <- NULL
      output$mapOutput <- NULL
      
      # get list of available languages
      langs <- gl_talk_languages()$languageCodes
      output$languageOutput <- renderUI({
        selectInput("languageInput", "Select language",
                    choices=c(sort(unique(langs))),
                    selected="en-AU")
      })
      
      # select voice gender
      output$genderOutput <- renderUI({
        selectInput("genderInput", "Select gender",
                    choices=c("FEMALE", "MALE", "NEUTRAL"),
                    selected="Female")
      })
      
      # user input text to be converted to speech
      output$textOutput <- renderUI({
        textAreaInput("textInput", "Enter text for speech synthesis")
      })
      
      output$evalButton <- renderUI({
        # only display "Evaluate" button when text input exists
        if (length(input$textInput) != 0) {
          actionButton("eval", "Evaluate")
        }
      })
    }
  })
    

  
  observeEvent(input$imageFile, {
    # if new image file selected, display image
    width <- session$clientData$output_imageOutput_width  # display width
    height <- session$clientData$output_imageOutput_height  # display height
    output$imageOutput <- renderImage(list(src=input$imageFile$datapath,
                                           height=height),
                                      delete=FALSE)
    # clear previous results table, map, and messages
    output$resultsTableOutput <- NULL
    output$mapOutput <- NULL
    output$resultsMsgOutput <- NULL
  })
  
  
  observeEvent(input$eval,{
    # if "Evaluation" button is pressed, clear previous results table, map, and messages
    output$resultsTableOutput <- NULL
    output$mapOutput <- NULL
    output$resultsMsgOutput <- NULL
    output$speechMsgOutput <- NULL
    
    if (input$GoogleService == "Cloud Vision") {

      width <- session$clientData$output_imageOutput_width  # set image display width
      height <- session$clientData$output_imageOutput_height  # set image display height
      
      if (input$imageFeature == "Label detection") {
        # compute labels and output to table
        labelResults <- gcv_get_image_annotations(imagePaths=input$imageFile$datapath,
                                                  feature="LABEL_DETECTION",
                                                  maxNumResults=input$maxResults)
        output$resultsTableOutput <- renderTable({
          resultsTable <- matrix(c(labelResults[["description"]],
                                   labelResults[["score"]]), ncol=2)
          colnames(resultsTable) <- c("Description", "Score")
          resultsTable
        })
      }

      else if (input$imageFeature == "Landmark detection") {
        # compute landmark(s) and output to table
        landmarkResults <- gcv_get_image_annotations(imagePaths=input$imageFile$datapath,
                                                     feature="LANDMARK_DETECTION",
                                                     maxNumResults=input$maxResults)
        
        # Valid results table should return 8 columns
        if (length(landmarkResults) != 8) {
          output$resultsMsgOutput <- renderText({
            "Sorry! Landmark not identified :-("
          })
        }
        else {
          output$resultsTableOutput <- renderTable({
            resultsTable <- matrix(c(landmarkResults[["description"]], 
                                     landmarkResults[["score"]]), ncol=2)
            colnames(resultsTable) <- c("Description", "Score")
            resultsTable
          })
          
          # output map of landmark location
          mapWidth <- session$clientData$output_mapOutput_width*3/5  # display width
          mapHeight <- session$clientData$output_mapOutput_height*3/5  # display height
          output$mapOutput <- renderLeaflet({
            m <- leaflet(height=mapHeight) %>%
              addTiles() %>%
              setView(lng=landmarkResults[["longitude"]][1],
                      lat=landmarkResults[["latitude"]][1], zoom=5) %>% 
              addMarkers(lng=landmarkResults[["longitude"]],
                         lat=landmarkResults[["latitude"]])
            m
          })
          
          # display image
          landmarkImg <- load.image(input$imageFile$datapath)
          jpeg(file="landmarkImg.jpg")  # saved in current working directory
          par(mar=c(0, 0, 0, 0))  # remove white space around image
          plot(landmarkImg, axes=FALSE, asp=width/height)
          
          # find coords of bounding box around identified landmark
          # and draw a green border around it, then display image
          xCoords <- strsplit(landmarkResults[["x"]][1], ",")
          xCoords <- c(as.double(xCoords[[1]][1]),
                       as.double(xCoords[[1]][2]),
                       as.double(xCoords[[1]][3]),
                       as.double(xCoords[[1]][4]))
          yCoords <- strsplit(landmarkResults[["y"]][1], ",")
          yCoords <- c(as.double(yCoords[[1]][1]),
                       as.double(yCoords[[1]][2]),
                       as.double(yCoords[[1]][3]),
                       as.double(yCoords[[1]][4]))
          polygon(xCoords, yCoords, border="green", "lwd"=3)
          dev.off()
          output$imageOutput <- renderImage(list(src="landmarkImg.jpg", width=width, height=height),
                                            deleteFile=FALSE)
        }
      }
    }
    
    else if (input$GoogleService == "Cloud Text-to-Speech") {
      gl_talk(input$textInput, output="output.wav",
              languageCode=input$languageInput, gender=input$genderInput)
      output$speechMsgOutput <- renderText({
        "Speech output to \"output.wav\""
      })
      soundObj <- readWave("output.wav")
      tuneR::play(soundObj)  # adding tuneR:: to not be confused with imager::play()
    }
  })
}



# Run the application
shinyApp(ui = ui, server = server)
