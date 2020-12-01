# Import libraries
library(data.table)
library(randomForest)
library(tidyverse)

# Read data 
datacomplete = read_csv("./datacomplete.csv") %>%
  mutate_at(c("admitted", "ethnicity_race", "asthma", "diabetes"), as.factor) %>%
  select(admitted, age, bmi_value, systolic_bp_value, ethnicity_race, asthma, diabetes)

####################################
# User interface                   #
####################################

ui = fluidPage(headerPanel('COVID-19 Pediatric Hospitalization Risk Predictor'),
               
               # Input values
               sidebarPanel(
                 HTML("<h3>Select patient parameters</h3>"),
                 
                 sliderInput("age", "Age:",
                             min = 0, max = 22,
                             value = 15),
                 
                 sliderInput("bmi_value", "Body Mass Index:",
                             min = 10, max = 85,
                             value = 22),
                 
                 sliderInput("systolic_bp_value", "Systolic blood pressure:",
                             min = 60, max = 190,
                             value = 120),
                 
                 selectInput("ethnicity_race", label = "Ethnicity:", 
                             choices = list("African American" = "black", "Asian" = "asian", "Caucasian" = "caucasian",
                                            "Hispanic" = "latino", "Native American" = "american indian",
                                            "Multiple" = "multiple"), 
                             selected = "African American"),
                 
                 selectInput("asthma", label = "Asthma:", 
                             choices = list("Yes" = "1", "No" = "0"), 
                             selected = "1"),
                 
                 selectInput("diabetes", label = "Diabetes:", 
                             choices = list("Yes" = "1", "No" = "0"), 
                             selected = "1"),
                 
                 actionButton("submitbutton", "Predict", class = "btn btn-primary")
               ),
               
               mainPanel(
                 tags$label(h3('Predicted Probability of Hospitalization')), # Status/Output Text Box
                 verbatimTextOutput('contents'),
                 tableOutput('tabledata') # Prediction results table
                 
               )
)