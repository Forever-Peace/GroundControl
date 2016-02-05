library("shiny")
library("ggplot2")

rb_counts <- as.data.frame(table(rb_app$full_name))
rb_counts <- rb_counts[order(-rb_counts[,2]),]

shinyServer(function(input, output) {
  output$playerControls <- renderUI({
    if(input$sortplayers == FALSE) playerchoices=levels(unique(rb_app$full_name))
    if(input$sortplayers == TRUE) playerchoices=unlist(lapply(rb_counts$Var1, as.character))
    selectInput("playertoplot",label=h3("Player"), choices=playerchoices,selected="Frank Gore")
  })
  output$yearControls <- renderUI({
    selectInput("yeartoplot",label=h3("Year"),choices=unique(rb_app[rb_app$full_name==input$playertoplot,]$year))
  })
  playerdataall <- reactive({
    rb_app[rb_app$yearteamweek %in% rb_app[rb_app$full_name==input$playertoplot,]$yearteamweek,]
  })
  playerdatayears <- reactive({
    rb_app[rb_app$yearteamweek %in% rb_app[rb_app$full_name==input$playertoplot&rb_app$year==input$yeartoplot,]$yearteamweek,]
  })
  output$textcarries1 <- renderText({ 
    if(input$plotallyears==TRUE) playerdata <- playerdataall()
    if(input$plotallyears==FALSE) playerdata <- playerdatayears()
    numcarries <- nrow(playerdata[playerdata$full_name == input$playertoplot,])
    numcarriestext <- as.character(input$playertoplot)
    paste("Number of carries (", numcarriestext, "): ", numcarries)
  })
  output$textcarries2 <- renderText({ 
    if(input$plotallyears==TRUE) playerdata <- playerdataall()
    if(input$plotallyears==FALSE) playerdata <- playerdatayears()
    numcarries <- nrow(playerdata[playerdata$full_name != input$playertoplot,])
    numcarriestext <- "all other players"
    paste("Number of carries (", numcarriestext, "): ", numcarries)
  })
  output$playercomparisons <- renderText({ 
    if(input$plotallyears==TRUE) playerdata <- playerdataall()
    if(input$plotallyears==FALSE) playerdata <- playerdatayears()
    paste(unique(playerdata[playerdata$full_name!=input$playertoplot,]$full_name),",")
  })
  output$carrieskde <- renderPlot(function() {
    if(input$plotallyears==TRUE) playerdata <- playerdataall()
    if(input$plotallyears==FALSE) playerdata <- playerdatayears()
    playerdata$playergroup <- "Others"
    playerdata[playerdata$full_name==input$playertoplot,]$playergroup <- input$playertoplot
    carriesplot <- ggplot(playerdata, aes(rushing_yds,fill=playergroup))+
      geom_density(aes(fill=playergroup),alpha = 0.2,adjust=1.85) + coord_cartesian(xlim=c(-10, 20), ylim=c(0,.16)) + ylab("Probability")+xlab ("Rushing Yards")+
      theme(legend.title = element_text(size=16, face="bold"))+scale_fill_discrete(name="Player")+
      labs(title="Comparison of Run Distributions")+theme(plot.title = element_text(size = rel(2)))+
      theme(axis.title=element_text(size="16"))
    print(carriesplot)
  })
  output$cumcarries <- renderPlot(function() {
    if(input$plotallyears==TRUE) playerdata <- playerdataall()
    if(input$plotallyears==FALSE) playerdata <- playerdatayears()
    playerdata$playergroup <- "Others"
    playerdata[playerdata$full_name==input$playertoplot,]$playergroup <- input$playertoplot
    cumcarriesplot <- ggplot(playerdata, aes(rushing_yds,fill=playergroup))+
      stat_ecdf(aes(color=playergroup),alpha = 0.8,size=1.4) + coord_cartesian(xlim=c(-10, 20)) + ylab("Proportion of Run Death")+xlab ("Rushing Yards")+ scale_y_reverse(breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
      theme(legend.title = element_text(size=16, face="bold"))+scale_color_discrete(name="Player")+
      labs(title="Comparison of Cumulative Distributions")+theme(plot.title = element_text(size = rel(2)))+
      theme(axis.title=element_text(size="16"))
    print(cumcarriesplot)
  })
  output$runshare <- renderPlot(function() {
    if(input$plotallyears==TRUE) playerdata <- playerdataall()
    if(input$plotallyears==FALSE) playerdata <- playerdatayears()
    playerdata$playergroup <- "Others"
    playerdata[playerdata$full_name==input$playertoplot,]$playergroup <- input$playertoplot
    runshareplot <- ggplot(playerdata, aes(x = factor(1), fill = factor(playergroup))) + geom_bar(width = 1)+ coord_polar(theta = "y") +
      ylab("")+xlab ("")+theme(legend.title = element_text(size=16, face="bold"))+scale_fill_discrete(name="Player")+
      labs(title="Proportion of Run Share in Active Games")+theme(plot.title = element_text(size = rel(2)))+
      theme(axis.title=element_text(size="16"))
    print(runshareplot)
  })
})