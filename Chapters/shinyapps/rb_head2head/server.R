library("shiny")
library("ggplot2")
library('reshape2')

rb_counts <- as.data.frame(table(rb_app$full_name))
rb_counts <- rb_counts[order(-rb_counts[,2]),]

resample_yardmeans <- function(yards,samplesize)
{
  yardmeans = numeric(10000)
  for (i in 1:10000) {
    yardmeans[i] = mean(sample(yards, size=samplesize, replace=T))
  }
  yardmeans
}

playercomp_resample <- function(player1, player2, samplesize)
{
  yardmeansdf <- data.frame(matrix(ncol = 2, nrow = 10000))
  colnames(yardmeansdf) <- c(player1, player2)
  yardmeansdf[,1] <- resample_yardmeans(rb_app[rb_app$full_name==player1,]$rushing_yds,samplesize)
  yardmeansdf[,2] <- resample_yardmeans(rb_app[rb_app$full_name==player2,]$rushing_yds,samplesize)
  yardmeansdf
}

plot_footrace <- function(df,df_title)
{
  qplot(value, data = data.frame(melt(df)), geom="density",fill=factor(variable),size=I(1.5),alpha=I(0.3),xlab="Yards Per Carry", ylab="Density", main=df_title)+scale_fill_discrete("Player")+coord_cartesian(xlim=c(0, 8),ylim=c(0,1.8))+geom_vline(xintercept = mean(df[[1]]),size=1.5)+geom_text(aes(mean(df[[1]]),y=1.5,label = round(mean(df[[1]]),digits=2), hjust = -0.19))+geom_vline(xintercept = mean(df[[2]]),size=1.5)+geom_text(aes(mean(df[[2]]),y=1.6,label = round(mean(df[[2]]),digits=2), hjust = -0.19))
}

shinyServer(function(input, output) {
  output$playerControls1 <- renderUI({
    if(input$sortplayers == FALSE) playerchoices=levels(unique(rb_app$full_name))
    if(input$sortplayers == TRUE) playerchoices=unlist(lapply(rb_counts$Var1, as.character))
    selectInput("playertoplot1",label=h3("Player 1"), choices=playerchoices,selected="Chris Ivory")
  })
  output$playerControls2 <- renderUI({
    if(input$sortplayers == FALSE) playerchoices=levels(unique(rb_app$full_name))
    if(input$sortplayers == TRUE) playerchoices=unlist(lapply(rb_counts$Var1, as.character))
    selectInput("playertoplot2",label=h3("Player 2"), choices=playerchoices, selected="Shonn Greene")
  })
  simdata <- eventReactive(input$recalc, {
    suppressMessages(playercomp_resample(input$playertoplot1,input$playertoplot2,input$samplenum))
  })
  samplenumupdate <- eventReactive(input$recalc, {
    input$samplenum
  })
  output$carrieskde <- renderPlot({
    suppressMessages(plot_footrace(simdata(), paste(samplenumupdate(), "carries each")))
  })
  output$resultstext <- renderUI({
    input$recalc
    isolate(player1text<-input$playertoplot1)
    isolate(player2text<-input$playertoplot2)
    str1 <- paste(player1text,"wins:",length(simdata()[which(simdata()[,1]>simdata()[,2]),][[1]])/100,"%")
    str2 <- paste(player2text,"wins:",length(simdata()[which(simdata()[,2]>simdata()[,1]),][[1]])/100,"%")
    str3 <- paste("Tie:",length(simdata()[which(simdata()[,2]==simdata()[,1]),][[1]])/100,"%")
    str4 <- "---"
    HTML(paste(str1, str2, str3, str4, "Competition outcomes (in YPC):", sep = '<br/>'))
  })
  output$fivenumtable = renderTable({
    fivenum_results<-as.data.frame(list(round(fivenum(simdata()[[1]]),2),round(fivenum(simdata()[[2]]),2)))
    row.names(fivenum_results)<-c("min","Q1","median","Q3","max")
    input$recalc
    isolate(colnames(fivenum_results)<-c(input$playertoplot1, input$playertoplot2))
    fivenum_results
  })
})