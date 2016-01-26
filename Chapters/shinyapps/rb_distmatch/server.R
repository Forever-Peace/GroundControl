library("shiny")
library("ggplot2")
library("reshape2")
library("FNN")

rb_counts <- as.data.frame(table(rb_app$full_name))
rbs_50cars <- lapply(unique(rb_counts[rb_counts$Freq>50,]$Var1), as.character)
rb_counts <- rb_counts[order(-rb_counts[,2]),]
rbs_50cars_ordered <- lapply(unique(rb_counts[rb_counts$Freq>50,]$Var1), as.character)

df_rb_rundist <- dcast(rb_app, full_name ~ rushing_yds, length)
df_rb_rundist$carries <- rowSums(df_rb_rundist[,-1], na.rm = TRUE)
df_rb_rundist <- df_rb_rundist[df_rb_rundist$carries > 50,]
row.names(df_rb_rundist) <- df_rb_rundist$full_name
df_rb_rundist$carries <- NULL
df_rb_rundist_table <- prop.table(as.table(as.matrix(df_rb_rundist[,-1])),1)
df_rb_rundist <- as.data.frame.matrix(df_rb_rundist_table) 
df_rb_rundist_cum <- data.frame(t(apply(df_rb_rundist,1,cumsum)))

players_knn_wide <- get.knn(df_rb_rundist_cum[which( colnames(df_rb_rundist_cum)=="X.3" ):which( colnames(df_rb_rundist_cum)=="X15" )],k=180,algorithm="brute")
players_knn_wide_index <- players_knn_wide["nn.index"][[1]]

playercomp_cumden <- function(df,player1,player2) #Comparison plots
{
  p <- ggplot(rbind(df[df$full_name==player1,],df[df$full_name==player2,]), 
              aes(rushing_yds,color=full_name))
  p + stat_ecdf(aes(color=full_name),alpha = 0.8,size=1.4) + coord_cartesian(xlim=c(-10, 20)) + ylab("Proportion of Run Death")+xlab ("Rushing Yards")+ scale_y_reverse(breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
    theme(legend.title = element_text(size=16, face="bold"))+scale_color_discrete(name="Player")+
    labs(title="Comparison of Cumulative Distributions")+theme(plot.title = element_text(size = rel(2)))+
    theme(axis.title=element_text(size="16"))
}

playercomp_knn <- function(df_rb_rundist_cum,knn_index,player,match_num) #Finding and plotting comparisons for stated players
{
  playerindex <- match(player,rownames(df_rb_rundist_cum))
  knn_matches <- knn_index[playerindex,] #matches for player
  knn_best_match <- row.names(df_rb_rundist_cum[knn_matches[match_num],]) #best match
  playercomp_cumden(df_rb,knn_best_match,player)
}

playercomp_knn_list <- function(df_rb_rundist_cum,knn_index,player,match_num) #Finding and plotting comparisons for stated players
{
  playerindex <- match(player,rownames(df_rb_rundist_cum))
  knn_matches <- knn_index[playerindex,] #matches for player
  knn_best_match <- row.names(df_rb_rundist_cum[knn_matches[1:match_num],]) #best match
  knn_best_match
}

#Shiny App
shinyServer(function(input, output) {
  output$playerControls <- renderUI({
    if(input$sortplayers == FALSE) playerchoices=unlist(rbs_50cars)
    if(input$sortplayers == TRUE) playerchoices=unlist(rbs_50cars_ordered)
    selectInput("playertoplot",label=h3("Player to match"), choices=playerchoices)
  })
  output$player2Controls <- renderUI({
    playermatches <- row.names(df_rb_rundist_cum[players_knn_wide_index[match(input$playertoplot,rownames(df_rb_rundist_cum)),],])
    selectInput("playermatch",label=h3("List of matches (in order)"),choices=playermatches)
  })
  output$textcarries1 <- renderText({ 
    numcarries <- nrow(rb_app[rb_app$full_name == input$playertoplot,])
    numcarriestext <- as.character(input$playertoplot)
    paste("Number of carries (", numcarriestext, "): ", numcarries)
  })
  output$textcarries2 <- renderText({ 
    numcarries <- nrow(rb_app[rb_app$full_name == input$playermatch,])
    numcarriestext <- as.character(input$playermatch)
    paste("Number of carries (", numcarriestext, "): ", numcarries)
  })
  output$top10matches <- renderText({
    paste(playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,input$playertoplot,10),",")
  })
  output$cumcarries <- renderPlot(function() {
    cumcarriesplot <- playercomp_cumden(rb_app,input$playertoplot,input$playermatch)
    print(cumcarriesplot)
  })
})