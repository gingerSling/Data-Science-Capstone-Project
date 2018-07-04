#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) { 
    url1 <- a("GitHub repository", href="https://www.google.com/")
    output$tab1 <- renderUI({
        tagList("The code is available at my personal ", url1)
    }) 
    url2 <- a("mail", href="portiamo95@hotmail.com")
    output$tab2 <- renderUI({
        tagList("You can contact me for any questions at my mail ", url2)
    })
    combo_output <- reactive({
        if(input$text1==""){
            a<-"Waiting for you to type some words!!"
            b<-"Waiting for you to type some words!!"
            c<-"Waiting for you to type some words!!"
            combo <- list(a = a, b = b,c=c)
            combo
        }
        else{
        res<-prediction2(input$text1)
        a <- strsplit(res[1,1],split=" ")[[1]][3]
        b <- strsplit(res[5,1],split=" ")[[1]][3]
        c <- strsplit(res[10,1],split=" ")[[1]][3]
        combo <- list(a = a, b = b,c=c)
        combo
    }
    })
    output$Guess1 <- renderText({
        #res<-prediction2(input$text1)
        #return(as.character(res[c(1,5,10),1]))
        combo<-combo_output()
        return(as.character(combo$a))
    
  })
    output$Guess2 <- renderText({
        #res<-prediction2(input$text1)
        #return(as.character(res[c(1,5,10),1]))
        combo<-combo_output()
        return(as.character(combo$b))
        
    })
    output$Guess3 <- renderText({
        #res<-prediction2(input$text1)
        #return(as.character(res[c(1,5,10),1]))
        combo<-combo_output()
        return(as.character(combo$c))
        
    })
  
})
