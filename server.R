library(tidyverse)
library(shiny)
library(caret)

# Read data 
datacomplete = read_csv("./datacomplete.csv") %>%
  mutate_at(c("admitted", "ethnicity_race", "asthma", "diabetes"), as.factor) %>%
  select(admitted, age, bmi_value, systolic_bp_value, ethnicity_race, asthma, diabetes)

# Build random forest model in caret
myFolds = createFolds(datacomplete$admitted, k = 5)
rfControl = trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, 
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

model = train(
  admitted ~., 
  data = datacomplete,
  metric = "ROC",
  method = "ranger",
  trControl = rfControl
)

server = function(input, output) {
            
            # Input Data
            datasetInput = reactive({  
              
              df = data.frame(
                Name = c("age", "bmi_value", "systolic_bp_value", "ethnicity_race", "asthma", "diabetes"),
                Value = as.character(c(input$age, input$bmi_value, input$systolic_bp_value, input$ethnicity_race, input$asthma,
                                       input$diabetes)),
                stringsAsFactors = FALSE)
              
              admitted = "admitted"
              df = rbind(admitted, df)
              input = transpose(df)
              write.table(input, "input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
              
              test = read_csv("input.csv") %>%
                mutate(admitted = factor(admitted, levels = c("yes", "no")),
                       ethnicity_race = factor(ethnicity_race, levels = c("american indian", "asian", "caucasian", "black",
                                                                          "latino", "multiple")),
                       asthma = factor(asthma, levels = c("0", "1")),
                       diabetes = factor(diabetes, levels = c("0", "1"))
                )
              
              Output = predict(model, test, type="prob")
              print(Output)
              
            })
            
            # Status/Output Text Box
            output$contents = renderPrint({
              if (input$submitbutton > 0) { 
                isolate("Calculation complete. Interpret results with caution.") 
              } else {
                return("Server is ready for calculation.")
              }
            })
            
            # Prediction results table
            output$tabledata = renderTable({
              if (input$submitbutton > 0) { 
                isolate(datasetInput()) 
              } 
            })
            
            }