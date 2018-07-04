#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Data Science CapStone Project"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        h3("App that predicts the next word using the previous 2."),
        h4("We have used the Katz's Back-off Model and some variants of the Good-Turning Smoothing in order to predict the next word."),
        h4("Just introduce your text in the dialogue box and submit and wait to the answer to come, we will show you the 3 most interesting next words."),
        h4("Due to the low power of the site, it will take 15 secs to predict your next word, sorry for the inconvinience")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(type = "tabs", 
                    tabPanel("Predict Me!", br(),textInput("text1","Can i have a chance to predict your next word?"),
                             submitButton("Press me for fun"),
                             verbatimTextOutput("Guess1"),
                             verbatimTextOutput("Guess2"),
                             verbatimTextOutput("Guess3")),
                    tabPanel("GitHub Code",br(),uiOutput("tab1"),uiOutput("tab2"))
                    
        )
       
    )
  )
))
