library(shiny)
library(xtable)
library(mlogit)
library(dplyr)
library(tidyr)
library(readxl)

ui <- fluidPage (
                headerPanel('Multionomial Logistic Regression'),
                sidebarPanel(
                              fileInput(inputId = "file", label = "Upload *.xlsx file"),
                              downloadButton('downloadData', 'Download'),
                              h6("To download the input file, please click on Open in Browser")
                            ),
                mainPanel(
                              tabsetPanel (type = "tabs",
                                           
                                           tabPanel("Data", tableOutput("exceldata")),
                                           tabPanel("Coefficient Estimates",tableOutput("coefficients")),
                                           tabPanel("Predicted Probabilities",tableOutput("probabilities")),
                                           tabPanel("Confusion Matrix",  h4("Confusion Matrix"),tableOutput("confusionmatrix"), h4("Confusion Matrix %"), tableOutput("confusionmatrixinpercentage"), verbatimTextOutput("hitrate"))
                                          )
                         )
                )

server <- function(input, output) 
{
  values <- reactiveValues()
  
  ChoiceData <- reactive({
                        if (is.null(input$file)) { return(NULL) }
                        else{
                              file.rename(input$file$datapath,paste(input$file$datapath, ".xlsx", sep=""))
                              ChoiceData <- read_excel(paste(input$file$datapath,".xlsx",sep=""), sheet = "ABB Survey Backup", skip = 2)
                              ChoiceData <- as.data.frame(ChoiceData)
                              values$ChoiceData <- ChoiceData[-nrow(ChoiceData),]
                              return(values$ChoiceData)
                            }
                      })
  
  output$exceldata <- renderTable( { ChoiceData() }, digits = 0, bordered = TRUE, spacing = "xs" )
  
  Coefficients <- reactive({
                             if (is.null(input$file)) { return(NULL) }
                             else{
                                   Electric.Company <- mlogit.data(values$ChoiceData, shape = "long", varying = 4:11, choice = "Choice", alt.var = "Alternatives", chid.var = "Customer")
                                   values$ml.Electric.Company <- mlogit(Choice ~ Price + `Energy Loss` + Maintenance + Warranty + `Spare Parts` + `Ease of Install` + `Prob Solver` + Quality, Electric.Company, reflevel = "Edison")
                                   values$coefficients <- summary(values$ml.Electric.Company)$CoefTable
                                   return(values$coefficients)
                                 }
                          })
  
  output$coefficients <- renderTable ( {Coefficients()}, bordered = TRUE, rownames = TRUE )
  
  EstimatedProbabilities <- reactive({
                                        if (is.null(input$file) | is.null(values$ml.Electric.Company)) { return(NULL) }
                                        else{
                                          
                                              predicted.probabilities <- as.data.frame(fitted(values$ml.Electric.Company, outcome = FALSE))

                                              max.probability <- apply(predicted.probabilities,1,which.max)

                                              for(i in 1:nrow(predicted.probabilities))
                                              {
                                                for(j in 1:ncol(predicted.probabilities))
                                                {
                                                  if (j == max.probability[i])
                                                  { predicted.probabilities[i,j] <- 1}
                                                  else
                                                  { predicted.probabilities[i,j] <- 0}
                                                }
                                              }

                                              predicted.probabilities$Customer <- NA
                                              for (i in 1:nrow(predicted.probabilities))
                                              {
                                                predicted.probabilities[i,]$Customer <- paste0("Customer ", i)
                                              }
                                              predicted.probabilities$Customer <- factor(predicted.probabilities$Customer)
                                              
                                              ObservedChoice <- values$ChoiceData[,1:3] %>% spread(Alternatives, Choice)
                                              ObservedChoice$Customer <- gsub("^\\s+|\\s+$", "", ObservedChoice$Customer)
                                              vals <- as.numeric(gsub("Customer ","", ObservedChoice$Customer))
                                              ObservedChoice <- ObservedChoice[order(vals),]
                                              
                                              estimatedchoiceprobabilities <- merge(predicted.probabilities, ObservedChoice, by = "Customer")
                                              # ##Customer 10 is shown after Customer 1, instead of Customer 2. Following code rearranges the Customer IDs
                                              vals<- as.numeric(gsub("Customer ","", estimatedchoiceprobabilities$Customer))
                                              estimatedchoiceprobabilities <- estimatedchoiceprobabilities[order(vals),]
                                              estimatedchoiceprobabilities <- estimatedchoiceprobabilities[, c(1,3,4,5,2,6,8,9,7)]
                                              colnames(estimatedchoiceprobabilities) <- c("Customer","Predicted ABB", "Predicted GE", "Predicted WestingHouse", "Predicted Edison", "Observed ABB", "Observed GE", "Observed WestingHouse", "Observed Edison")
                                              
                                              values$ObservedChoice <- ObservedChoice
                                              values$predicted.probabilities <- predicted.probabilities
                                              values$estimatedchoiceprobabilities <- estimatedchoiceprobabilities
                                              
                                              return(values$estimatedchoiceprobabilities)
                                           }
                                    })
                                          
  output$probabilities <- renderTable ( {EstimatedProbabilities()}, digits = 0, bordered = TRUE, spacing = "xs" )
  
  
  ConfusionMatrix <- reactive({
                                  if (is.null(input$file) | is.null(values$ml.Electric.Company)) { return(NULL) }
                                  else{
                                    
                                    predicted.probabilities <- values$predicted.probabilities
                                    predicted.probabilities$PredictedClass <- NA
                                    
                                    for(i in 1:nrow(predicted.probabilities))
                                    {
                                      if(predicted.probabilities[i,1] == 1)
                                      {
                                        predicted.probabilities[i,]$PredictedClass = "Edison"
                                      }
                                      else if(predicted.probabilities[i,2] == 1)
                                      {
                                        predicted.probabilities[i,]$PredictedClass = "ABB"
                                      }
                                      else if(predicted.probabilities[i,3] == 1)
                                      {
                                        predicted.probabilities[i,]$PredictedClass = "GE"
                                      }
                                      else if(predicted.probabilities[i,4] == 1)
                                      {
                                        predicted.probabilities[i,]$PredictedClass = "Westinghouse"
                                      }
                                    }
                                    
                                    ObservedChoice <- values$ObservedChoice 
                                    
                                    ObservedChoice$ObservedClass <- NA
                                    for(i in 1:nrow(ObservedChoice))
                                    {
                                      if(ObservedChoice[i,2] == 1)
                                      {
                                        ObservedChoice[i,]$ObservedClass = "ABB"
                                      }
                                      else if(ObservedChoice[i,3] == 1)
                                      {
                                        ObservedChoice[i,]$ObservedClass = "Edison"
                                      }
                                      else if(ObservedChoice[i,4] == 1)
                                      {
                                        ObservedChoice[i,]$ObservedClass = "GE"
                                      }
                                      else if(ObservedChoice[i,5] == 1)
                                      {
                                        ObservedChoice[i,]$ObservedClass = "Westinghouse"
                                      }
                                    }
                                    
                                    confusion.matrix <- table(predicted.probabilities$PredictedClass,ObservedChoice$ObservedClass)
                                    confusion.matrix.format <- as.data.frame.matrix(confusion.matrix)
                                    values$confusionmatrix <- confusion.matrix
                                    return(confusion.matrix.format)
                                  }
                                })
  
  
  output$confusionmatrix <- renderTable ( {ConfusionMatrix()}, digits = 0, rownames = TRUE )
  
  ConfusionMatrixinPercentage <- reactive({
                                            if (is.null(input$file) | is.null(values$ml.Electric.Company)) { return(NULL) }
                                            else{
                                                  ConfusionMatrix.in.Percentage <- round(prop.table(values$confusionmatrix,2) * 100,2)
                                                  ConfusionMatrix.in.Percentage <- as.data.frame.matrix(ConfusionMatrix.in.Percentage)
                                                  return(ConfusionMatrix.in.Percentage)
                                                }
                                         })
            
  
  output$confusionmatrixinpercentage <- renderTable ( {ConfusionMatrixinPercentage()}, digits = 0, rownames = TRUE )
  
  HitRate <- reactive({
                        if (is.null(input$file) | is.null(values$ml.Electric.Company)) { return(NULL) }
                        else{
                              Correct.Predictions <- values$confusionmatrix[1,1] + values$confusionmatrix[2,2] + values$confusionmatrix[3,3] + values$confusionmatrix[4,4]  
                              hitrate <- round(Correct.Predictions / length(values$ObservedChoice$Customer) * 100, 2)
                              hitrate <- paste0("The Hit rate (percent of total cases correctly classified) is : ",hitrate, " %" )
                              return(hitrate)
                            }
                     })
  
  
  output$hitrate <- renderText({HitRate()})
  
  output$downloadData <- downloadHandler(filename = 'mlogit.xlsx', content = function(file){file.copy('mlogit.xlsx', file)})
  
}

shinyApp(ui = ui, server = server)