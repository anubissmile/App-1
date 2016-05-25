#setwd("C:/Users/anubissmile/Desktop/R_test-shiny/App-1")

midterm <- read.csv("csv/mid-term-score.csv", header = TRUE, sep = ",")
# midterm <- read.csv("C:/Users/anubis/Desktop/R_test-shiny/mid-term-score2.csv", header = TRUE, sep = ",")


userMatch <- function(ID,pwd){
  r <- subset(midterm,id == ID & password == pwd)
  if(nrow(r) != 0){
    return (TRUE)
  }else{ return (FALSE) }
}

genGradeSet <- function(std_code,fin){
  id <- midterm[std_code,1]
  name <- midterm[std_code,2]
  sc <- midterm[std_code,4]
  qz <- midterm[std_code,5]
  cls <- midterm[std_code,6]
  total1 <- sc+qz+cls
  if(fin != 0){
    sc <- c(rep(paste("(A)Mid-term score"," ", sc),sc) 
            ,rep(paste("(B)Quiz score"," ", qz),qz)
            ,rep(paste("(C)Class score"," ", cls),cls)
            ,rep(paste("(D)Final score",fin),fin))
  }else{
    sc <- c(rep(paste("Mid-term score"," ", sc),sc) 
            ,rep(paste("Quiz score"," ", qz),qz)
            ,rep(paste("Class score"," ",cls),cls))
  }
  return (data.frame(id,name,sc))
  # return (TRUE)
}

genGradeSet2 <- function(std_code){
  id <- midterm[std_code,1]
  name <- midterm[std_code,2]
  sc <- midterm[std_code,4]
  qz <- midterm[std_code,5]
  cls <- midterm[std_code,6]
  total2 <- sc+qz+cls
  sc <- c(rep(paste("Mid-term score"," ", sc),sc) 
          ,rep(paste("Quiz score"," ", qz),qz)
          ,rep(paste("Class score"," ",cls),cls))
  return (data.frame(id,name,sc))
  # return (TRUE)
}

# a <- subset(midterm, name %in% c("Wesarut","Tawatchai"))

getPointer <- function(sid){
  
  if(sid != "ID"){
    i <- 1
    repeat{
      if(midterm$id[i] == sid){
        break
      }
      i <- i + 1
    }
    return (i)
  }
  
}


library(shiny)
library(ggplot2)
# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output) {
  
  selectf <- reactive({
    switch(input$fieldset,
           "mid" = midterm$score,
           "quiz" = midterm$quiz,
           "class" = midterm$class,
           "Total" = midterm$score + midterm$quiz + midterm$class)
  })
  
  output$ScorePlot <- renderPlot({
    
    if(input$stdID1 != 'ID' && input$stdID1 != "" && userMatch(input$stdID1,input$pwd)){
      i <- 0
      colColor <- c(1,2)
      colorCompare <- c("#009933","#0066CC")
      colorplain <- "#999966"
      info <- c("Please input your id","Please input other id for comparision.")
      
#       if(input$stdID1 == ""){
#         output$summary <- renderText(info[1])
#       }
      
#       if(input$stdID2 == ""){
#         output$summary2 <- renderText(info[2])
#       }
      
      repeat{
        i <- i + 1
        if(i <= length(midterm$score)){
          ##############
          if(i == getPointer(input$stdID1) && typeof(getPointer(input$stdID1)) == 'double'){
            colColor[i] <- colorCompare[1]
            info[1] <- paste(midterm$id[i], midterm$name[i], "\nclass score : ",
                             midterm$class[i], " points\nquiz : ",
                             midterm$quiz[i], "points\nsmid-term : ",
                             midterm$score[i], " points")
          
#           else if(i == getPointer(input$stdID2) && typeof(getPointer(input$stdID2)) == 'double'){
#             colColor[i] <- colorCompare[2]
#             info[2] <- paste(midterm$id[i], midterm$name[i], "\nclass score : ",
#                              midterm$class[i], " points\nquiz : ",
#                              midterm$quiz[i], "points\nsmid-term : ",
#                              midterm$score[i], " points")
          }else{
            colColor[i] <- colorplain
            
          }
          ##############
        }else{
          break
        }
        
      }
      
#       output$summary <- renderText(info[1])
#       output$summary2 <- renderText(info[2])
      
      barplot(selectf(), main = "SCORE CALCULATE",
              ylab = "Scores",
              xlab = "Students",
              names.arg = selectf(),
              col=colColor,
              border = 'black',
              ylim = c(0,100))
    }
    
    
    
  })
  
  #Output Table Summarize of Gradeset
  
  # output$tabOut <- renderTable({
  #   if(input$stdID1 != 'ID' && input$stdID1 != "" && userMatch(input$stdID1,input$pwd)){
  #     if(input$finalterm != ""){
  #       fin <- input$finalterm
  #     }else{ fin <- 0 }
  #     gradeset <- genGradeSet(getPointer(input$stdID1),fin)
  #     ss1 <- subset(midterm,id == input$stdID1)
  #     total1 <- (ss1$score+ss1$quiz+ss1$class)
  #     gradeset
  #   }
  # })
  
  output$diversity <- renderPlot({ 
    # Grouped Bar Plot
    
      # output$Test <- renderText(getPointer(input$stdID1))
      
    if(input$stdID1 != 'ID' && input$stdID1 != "" && userMatch(input$stdID1,input$pwd)){
      if(input$finalterm != ""){
        fin <- input$finalterm
      }else{ fin <- 0 }
      gradeset <- genGradeSet(getPointer(input$stdID1),fin)
      ss1 <- subset(midterm,id == input$stdID1)
      total1 <- (ss1$score+ss1$quiz+ss1$class)
      
      
#       if(input$stdID2 != 'ID' && input$stdID1 != ""){
#         gradeset2 <- genGradeSet2(getPointer(input$stdID2))
#         gradeset <- rbind(gradeset,gradeset2)
#         rm(gradeset2)
#         ss2 <- subset(midterm,id == input$stdID2)
#         total2 <- (ss2$score+ss2$quiz+ss2$class)
#       }
      
#       k <- ggplot(gradeset, aes(factor(name), fill = factor(sc)))
#       k + geom_bar() + coord_flip()
      colnames(gradeset)[3] <- "Score"
      # Bar layer plot
      ggplot(gradeset, aes(name, fill=Score)) + geom_bar(width=0.3)
      
      # #Pie plot
      # ggplot(gradeset, aes(x=name, fill=Score)) +
      #   xlab(paste("Name : ", gradeset$name)) + 
      #   ylab("Score Type") + 
      #   ggtitle("Ratio for each score type") +
      #   geom_bar(width=1) + 
      #   coord_polar(theta = "x")

      
      
    }
    
  })
  
  output$summary <- renderText({
    if(input$stdID1 != 'ID' && input$stdID1 != "" && userMatch(input$stdID1,input$pwd)){
      r <- subset(midterm, id == input$stdID1 & password == input$pwd)
      paste(r$id," ",r$name)

    }
  })
  
  output$meanClass <- renderText({
    if(input$stdID1 != 'ID' && input$stdID1 != "" && 
       userMatch(input$stdID1,input$pwd)){
      r <- subset(midterm, id == input$stdID1 & password == input$pwd)
      paste("class room score (10 Points): ", r$class,
            "\nMean of Classroom score : ", round(mean(midterm$class), digits=2) , 
            "\nMax of classroom score : ", max(midterm$class),
            "\nMin of classroom score : ", min(midterm$class),
            "\nStandard Deviation : ", round(sd(midterm$class),digits=2))
      
    }
  })
  
  output$meanQuiz <- renderText({
    if(input$stdID1 != 'ID' && input$stdID1 != "" && userMatch(input$stdID1,input$pwd)){
      r <- subset(midterm, id == input$stdID1 & password == input$pwd)
      paste("quiz score (30 Points): ",r$quiz,
            "\nMean of quiz score : ", round(mean(midterm$quiz),digits=2), 
            "\nMax of quiz score : ", max(midterm$quiz),
            "\nMin of quiz score : ", min(midterm$quiz),
            "\nStandard Deviation : ", round(sd(midterm$quiz),digits=2))
      
    }
  })
  
  output$meanMid <- renderText({
    if(input$stdID1 != 'ID' && input$stdID1 != "" && userMatch(input$stdID1,input$pwd)){
      r <- subset(midterm, id == input$stdID1 & password == input$pwd)
      paste("Mid-term score (20 Points): ",r$score,
            "\nMean of Mid-term score : ", round(mean(midterm$score),digits=2), 
            "\nMax of Mid-term score : ", max(midterm$score),
            "\nMin of Mid-term score : ", min(midterm$score),
            "\nStandard Deviation : ", round(sd(midterm$score),digits=2))
      
    }
  })
  
  output$greatSummary <- renderText({
    if(input$stdID1 != 'ID' && input$stdID1 != "" && userMatch(input$stdID1,input$pwd) && input$finalterm != ""){
      ss <- subset(midterm, id == input$stdID1)
      # output$header <- renderText(paste(ss$id, " ", ss$name, "\n"))
      if(input$finalterm != ""){
        total <- (ss$score + ss$quiz + ss$class + as.numeric(input$finalterm))
      }else{
        total <- (ss$score + ss$quiz + ss$class)
      }
      
      # gtotal <- total + as.numeric(input$finalterm)
      
      if(total >= 80){
        #A
        paste("Total Score ", (total), "You will get A\n")
      }else if(total >= 75){
        #B+
        paste("Total Score ", (total), "You will get B+\n")
      }else if(total >= 70){
        #B
        paste("Total Score ", (total), "You will get B\n")
      }else if(total >= 65){
        #C+
        paste("Total Score ", (total), "You will get C+\n")
      }else if(total >= 60){
        #C
        paste("Total Score ", (total), "You will get C\n")
      }else if(total >= 55){
        #D+
        paste("Total Score ", (total), "You will get D+\n")
      }else{
        #D
        paste("Total Score ", (total), "You will get D\n")
      }
      
      
    }
  })
  
  
  
})
