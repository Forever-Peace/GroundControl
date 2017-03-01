library("shiny")
library("ggplot2")

rb_counts <- unique(transform(rb_app, nameweekyear_atts=ave(nameweekyear_atts, full_name, FUN=sum))[c("full_name","nameweekyear_atts")])
rb_counts <- rb_counts[order(-rb_counts[,2]),]

probvsleague <- function(player1)
{
  ggplot(rb_app, aes(x=nameweekyear_atts, y=surp))+geom_point(color="gray70")+
    geom_line(aes(x=nameweekyear_atts, y=surp_avg), size=1.75, color="dodgerblue3")+
    geom_point(data=rb_app[rb_app$full_name==player1,],color="coral3",size=4)+
    stat_smooth(data=rb_app[rb_app$full_name==player1,], method="loess", size=1.75, span = 0.65, color="coral3", se=FALSE)+
    ylab ("Game Surprisal (in bits)")+xlab ("Game Rushing Attempts")+
    labs(title=paste0("Surprising Games: ",player1))+theme(plot.title = element_text(size = rel(2)))+
    theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))
}

probvsleague_year <- function(player1,year)
{
  ggplot(rb_app, aes(x=nameweekyear_atts, y=surp))+geom_point(color="gray80")+
    geom_line(aes(x=nameweekyear_atts, y=surp_avg), size=1.75, color="dodgerblue3")+
    geom_point(data=rb_app[rb_app$full_name==player1,],color="coral3",size=4)+
    geom_point(data=rb_app[rb_app$full_name==player1&rb_app$year==year,],color="darkslategray4",shape=18,size=6.5)+
    stat_smooth(data=rb_app[rb_app$full_name==player1,], method="loess", size=1.75, span = 0.65, color="coral3", se=FALSE)+
    ylab ("Game Surprisal (in bits)")+xlab ("Game Rushing Attempts")+
    labs(title=paste0("Surprising Games: ",player1," ",year))+theme(plot.title = element_text(size = rel(2)))+
    theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))
}

probvsleague_den <- function(player1)
{
  ggplot(rb_app[rb_app$nameweekyear_atts>2,], aes(surp_marg))+
    geom_density(fill='blue',alpha = 0.2, adjust=1.15)+coord_cartesian(xlim=c(-15, 15), ylim=c(0,0.25))+
    geom_rug(data=rb_app[rb_app$full_name==player1,],col="coral3",alpha=0.6)+
    geom_density(data=rb_app[rb_app$full_name==player1,],fill="coral3", alpha=0.2)+
    ylab ("Relative Probability")+xlab ("Marginal Surprisal in a Game")+
    labs(title=paste0("Surprising Games: ",player1))+theme(plot.title = element_text(size = rel(2)))+
    theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))
}

shinyServer(function(input, output) {
  output$yearControls <- renderUI({
    selectInput("yeartoplot",label=h3("Year"),choices=unique(rb_app[rb_app$full_name==input$playertoplot,]$year),selected="2016")
  })
  output$playerControls <- renderUI({
    if(input$sortplayers == FALSE) playerchoices=levels(unique(rb_app$full_name))
    if(input$sortplayers == TRUE) playerchoices=unlist(lapply(rb_counts$full_name, as.character))
    selectInput("playertoplot",label=h3("Player"), choices=playerchoices)
  })
  output$gamesurp <- renderPlot({
    if(input$markyears == FALSE) carriesplot <- probvsleague(input$playertoplot)
    if(input$markyears == TRUE) carriesplot <- probvsleague_year(input$playertoplot, input$yeartoplot)
    print(carriesplot)
  })
  output$surpmarg <- renderPlot({
    densurpplot <- probvsleague_den(input$playertoplot)
    print(densurpplot)
  })
})