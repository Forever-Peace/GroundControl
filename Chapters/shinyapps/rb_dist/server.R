library("shiny")
library("ggplot2")

rb_counts <- as.data.frame(table(rb_app$full_name))
rb_counts <- rb_counts[order(-rb_counts[,2]),]

shinyServer(function(input, output) {
  output$yearControls <- renderUI({
    selectInput("yeartoplot",label=h3("Year"),choices=unique(rb_app[rb_app$full_name==input$playertoplot,]$year),selected="2015")
  })
  output$playerControls <- renderUI({
    if(input$sortplayers == FALSE) playerchoices=levels(unique(rb_app$full_name))
    if(input$sortplayers == TRUE) playerchoices=unlist(lapply(rb_counts$Var1, as.character))
    selectInput("playertoplot",label=h3("Player"), choices=playerchoices)
  })
  output$textcarries <- renderText({ 
    if(input$plotallyears == TRUE) numcarries <- nrow(rb_app[rb_app$full_name == input$playertoplot,])
    if(input$plotallyears == FALSE) numcarries <- nrow(rb_app[rb_app$full_name == input$playertoplot & rb_app$year == input$yeartoplot,])
    paste("Number of carries : ", numcarries)
  })
  output$carrieskde <- renderPlot({
    density<-ggplot(rb_app, aes(rushing_yds)) + geom_density(fill='blue',alpha = 0.2,adjust=1.85)+ coord_cartesian(xlim=c(-10, 20), ylim=c(0,.16))+ylab ("Probability")+xlab("Rushing Yards")
    if(input$plotallyears == TRUE) carriesplot <- density+geom_density(mapping = aes(rushing_yds), data = rb_app[rb_app$full_name==input$playertoplot,],fill='red',alpha = 0.2)
    if(input$plotallyears == FALSE) carriesplot <- density+geom_density(mapping = aes(rushing_yds), data = rb_app[rb_app$full_name==input$playertoplot & rb_app$year == input$yeartoplot,],fill='red',alpha = 0.2)
    print(carriesplot)
  })
  output$cumcarries <- renderPlot({
    cum_density<- ggplot(rb_app, aes(rushing_yds))+stat_ecdf(alpha = 0.4,color="blue",size=1.4) + coord_cartesian(xlim=c(-10, 20)) + ylab("Proportion of Run Death") + xlab ("Rushing Yards") + scale_y_reverse(breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+scale_x_continuous(breaks=seq(-10, 20, 5))
    if(input$plotallyears == TRUE) cumcarriesplot <- cum_density+stat_ecdf(mapping = aes(rushing_yds), data = rb_app[rb_app$full_name==input$playertoplot,],color='red',alpha = 0.4,size=1.4)
    if(input$plotallyears == FALSE) cumcarriesplot <- cum_density+stat_ecdf(mapping = aes(rushing_yds), data = rb_app[rb_app$full_name==input$playertoplot & rb_app$year == input$yeartoplot,],color='red',alpha = 0.4,size=1.4)
    print(cumcarriesplot)
  })
})