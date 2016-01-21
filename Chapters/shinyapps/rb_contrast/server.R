library("shiny")
library("ggplot2")

rb_counts <- as.data.frame(table(rb_app$full_name))
rb_counts <- rb_counts[order(-rb_counts[,2]),]

shinyServer(function(input, output) {
  output$yearControls1 <- renderUI({
    selectInput("yeartoplot1",label=h3("Year 1"),choices=unique(rb_app[rb_app$full_name==input$playertoplot1,]$year))
  })
  output$yearControls2 <- renderUI({
    year2choices <- unique(rb_app[rb_app$full_name==input$playertoplot1,]$year)
    year2choices <- year2choices[which(year2choices!=input$yeartoplot1)] 
    selectInput("yeartoplot2",label=h3("Year 2"),choices=year2choices)
  })
  output$playerControls1 <- renderUI({
    if(input$sortplayers == FALSE) playerchoices=levels(unique(rb_app$full_name))
    if(input$sortplayers == TRUE) playerchoices=unlist(lapply(rb_counts$Var1, as.character))
    selectInput("playertoplot1",label=h3("Player 1"), choices=playerchoices,selected="Frank Gore")
  })
  output$playerControls2 <- renderUI({
    if(input$sortplayers == FALSE) playerchoices=levels(unique(rb_app$full_name))
    if(input$sortplayers == TRUE) playerchoices=unlist(lapply(rb_counts$Var1, as.character))
    selectInput("playertoplot2",label=h3("Player 2"), choices=playerchoices, selected="Matt Forte")
  })
  output$textcarries1 <- renderText({ 
    if(input$compchoice == "Players") {
      numcarries <- nrow(rb_app[rb_app$full_name == input$playertoplot1,])
      numcarriestext <- as.character(input$playertoplot1)
      }
    if(input$compchoice == "Years") {
      numcarries <- nrow(rb_app[rb_app$full_name == input$playertoplot1 & rb_app$year == input$yeartoplot1,])
      numcarriestext <- as.character(input$yeartoplot1)
    }
    paste("Number of carries (", numcarriestext, "): ", numcarries)
  })
  output$textcarries2 <- renderText({ 
    if(input$compchoice == "Players") {
      numcarries <- nrow(rb_app[rb_app$full_name == input$playertoplot2,])
      numcarriestext <- as.character(input$playertoplot2)
    }
    if(input$compchoice == "Years") {
      numcarries <- nrow(rb_app[rb_app$full_name == input$playertoplot1 & rb_app$year == input$yeartoplot2,])
      numcarriestext <- as.character(input$yeartoplot2)
    }
    paste("Number of carries (", numcarriestext, "): ", numcarries)
  })
  output$carrieskde <- renderPlot(function() {
    if(input$compchoice == "Players") {
      carriesplot <- ggplot(rbind(rb_app[rb_app$full_name==input$playertoplot1,],rb_app[rb_app$full_name==input$playertoplot2,]), aes(rushing_yds,fill=full_name))+
        geom_density(aes(fill=full_name),alpha = 0.2,adjust=1.85) + coord_cartesian(xlim=c(-10, 20), ylim=c(0,.16)) + ylab("Probability")+xlab ("Rushing Yards")+
        theme(legend.title = element_text(size=16, face="bold"))+scale_fill_discrete(name="Player")+
        labs(title="Comparison of Run Distributions")+theme(plot.title = element_text(size = rel(2)))+
        theme(axis.title=element_text(size="16"))
    }
    if(input$compchoice == "Years") {
      carriesplot <- ggplot(rbind(rb_app[rb_app$full_name==input$playertoplot1&rb_app$year==input$yeartoplot1,],rb_app[rb_app$full_name==input$playertoplot1&rb_app$year==input$yeartoplot2,]), aes(rushing_yds,fill=as.factor(year)))+
        geom_density(aes(fill=as.factor(year)),alpha = 0.2,adjust=1.85) + coord_cartesian(xlim=c(-10, 20), ylim=c(0,.16)) + ylab("Probability")+xlab ("Rushing Yards")+
        theme(legend.title = element_text(size=16, face="bold"))+scale_fill_discrete(name="Year")+
        labs(title="Comparison of Run Distributions")+theme(plot.title = element_text(size = rel(2)))+
        theme(axis.title=element_text(size="16"))
    }
    print(carriesplot)
  })
  output$cumcarries <- renderPlot(function() {
    if(input$compchoice == "Players") {
    cumcarriesplot <- ggplot(rbind(rb_app[rb_app$full_name==input$playertoplot1,],rb_app[rb_app$full_name==input$playertoplot2,]), aes(rushing_yds,color=full_name))+
      stat_ecdf(aes(color=full_name),alpha = 0.8,size=1.4) + coord_cartesian(xlim=c(-10, 20)) + ylab("Proportion of Run Death")+xlab ("Rushing Yards")+ scale_y_reverse(breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
      theme(legend.title = element_text(size=16, face="bold"))+scale_color_discrete(name="Player")+
      labs(title="Comparison of Cumulative Distributions")+theme(plot.title = element_text(size = rel(2)))+
      theme(axis.title=element_text(size="16"))
    }
    if(input$compchoice == "Years") {
      cumcarriesplot <- ggplot(rbind(rb_app[rb_app$full_name==input$playertoplot1&rb_app$year==input$yeartoplot1,],rb_app[rb_app$full_name==input$playertoplot1&rb_app$year==input$yeartoplot2,]), aes(rushing_yds,color=year))+
        stat_ecdf(aes(color=as.factor(year)),alpha = 0.8,size=1.4) + coord_cartesian(xlim=c(-10, 20)) + ylab("Proportion of Run Death")+xlab ("Rushing Yards")+ scale_y_reverse(breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
        theme(legend.title = element_text(size=16, face="bold"))+scale_color_discrete(name="Year")+
        labs(title="Comparison of Cumulative Distributions")+theme(plot.title = element_text(size = rel(2)))+
        theme(axis.title=element_text(size="16"))
    }
    print(cumcarriesplot)
  })
})