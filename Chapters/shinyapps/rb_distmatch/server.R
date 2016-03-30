library("shiny")
library("ggplot2")
library("reshape2")
library("FNN")

rb_counts <- as.data.frame(table(rb_app$full_name))
rbs_50cars <- lapply(unique(rb_counts[rb_counts$Freq>=50,]$Var1), as.character)
rbs_100cars <- lapply(unique(rb_counts[rb_counts$Freq>=100,]$Var1), as.character)
rbs_150cars <- lapply(unique(rb_counts[rb_counts$Freq>=150,]$Var1), as.character)
rbs_200cars <- lapply(unique(rb_counts[rb_counts$Freq>=200,]$Var1), as.character)
rb_counts <- rb_counts[order(-rb_counts[,2]),]

rbs_50cars_ordered <- lapply(unique(rb_counts[rb_counts$Freq>=50,]$Var1), as.character)
rbs_100cars_ordered <- lapply(unique(rb_counts[rb_counts$Freq>=100,]$Var1), as.character)
rbs_150cars_ordered <- lapply(unique(rb_counts[rb_counts$Freq>=150,]$Var1), as.character)
rbs_200cars_ordered <- lapply(unique(rb_counts[rb_counts$Freq>=200,]$Var1), as.character)

df_rb_rundist <- dcast(rb_app, full_name ~ rushing_yds, length)
df_rb_rundist$carries <- rowSums(df_rb_rundist[,-1], na.rm = TRUE)

df_rb_rundist50 <- df_rb_rundist[df_rb_rundist$carries >= 50,]
row.names(df_rb_rundist50) <- df_rb_rundist50$full_name
df_rb_rundist50$carries <- NULL
df_rb_rundist_table <- prop.table(as.table(as.matrix(df_rb_rundist50[,-1])),1)
df_rb_rundist50 <- as.data.frame.matrix(df_rb_rundist_table) 
df_rb_rundist50_cum <- data.frame(t(apply(df_rb_rundist50,1,cumsum)))

df_rb_rundist100 <- df_rb_rundist[df_rb_rundist$carries >= 100,]
row.names(df_rb_rundist100) <- df_rb_rundist100$full_name
df_rb_rundist100$carries <- NULL
df_rb_rundist_table <- prop.table(as.table(as.matrix(df_rb_rundist100[,-1])),1)
df_rb_rundist100 <- as.data.frame.matrix(df_rb_rundist_table) 
df_rb_rundist100_cum <- data.frame(t(apply(df_rb_rundist100,1,cumsum)))

df_rb_rundist150 <- df_rb_rundist[df_rb_rundist$carries >= 150,]
row.names(df_rb_rundist150) <- df_rb_rundist150$full_name
df_rb_rundist150$carries <- NULL
df_rb_rundist_table <- prop.table(as.table(as.matrix(df_rb_rundist150[,-1])),1)
df_rb_rundist150 <- as.data.frame.matrix(df_rb_rundist_table) 
df_rb_rundist150_cum <- data.frame(t(apply(df_rb_rundist150,1,cumsum)))

df_rb_rundist200 <- df_rb_rundist[df_rb_rundist$carries >= 200,]
row.names(df_rb_rundist200) <- df_rb_rundist200$full_name
df_rb_rundist200$carries <- NULL
df_rb_rundist_table <- prop.table(as.table(as.matrix(df_rb_rundist200[,-1])),1)
df_rb_rundist200 <- as.data.frame.matrix(df_rb_rundist_table) 
df_rb_rundist200_cum <- data.frame(t(apply(df_rb_rundist200,1,cumsum)))

players_knn_wide_50 <- get.knn(df_rb_rundist50_cum[which( colnames(df_rb_rundist50_cum)=="X.3" ):which( colnames(df_rb_rundist50_cum)=="X15" )],k=180,algorithm="brute")
players_knn_wide_50_index <- players_knn_wide_50["nn.index"][[1]]

players_knn_wide_100 <- get.knn(df_rb_rundist100_cum[which( colnames(df_rb_rundist100_cum)=="X.3" ):which( colnames(df_rb_rundist100_cum)=="X15" )],k=150,algorithm="brute")
players_knn_wide_100_index <- players_knn_wide_100["nn.index"][[1]]

players_knn_wide_150 <- get.knn(df_rb_rundist150_cum[which( colnames(df_rb_rundist150_cum)=="X.3" ):which( colnames(df_rb_rundist150_cum)=="X15" )],k=120,algorithm="brute")
players_knn_wide_150_index <- players_knn_wide_150["nn.index"][[1]]

players_knn_wide_200 <- get.knn(df_rb_rundist200_cum[which( colnames(df_rb_rundist200_cum)=="X.3" ):which( colnames(df_rb_rundist200_cum)=="X15" )],k=98,algorithm="brute")
players_knn_wide_200_index <- players_knn_wide_200["nn.index"][[1]]

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
    rbs_cars <- switch(input$mincarries,
           "50" = rbs_50cars,
           "100" = rbs_100cars,
           "150" = rbs_150cars,
           "200" = rbs_200cars)
    rbs_cars_ordered <- switch(input$mincarries,
                       "50" = rbs_50cars_ordered,
                       "100" = rbs_100cars_ordered,
                       "150" = rbs_150cars_ordered,
                       "200" = rbs_200cars_ordered)
    if(input$sortplayers == FALSE) playerchoices=unlist(rbs_cars)
    if(input$sortplayers == TRUE) playerchoices=unlist(rbs_cars_ordered)
    selectInput("playertoplot",label=h3("Player to match"), choices=playerchoices)
  })
  output$player2Controls <- renderUI({
    df_rb_rundist_cum <- switch(input$mincarries,
                       "50" = df_rb_rundist50_cum,
                       "100" = df_rb_rundist100_cum,
                       "150" = df_rb_rundist150_cum,
                       "200" = df_rb_rundist200_cum)
    players_knn_wide_index <- switch(input$mincarries,
                                "50" = players_knn_wide_50_index,
                                "100" = players_knn_wide_100_index,
                                "150" = players_knn_wide_150_index,
                                "200" = players_knn_wide_200_index)
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
    df_rb_rundist_cum <- switch(input$mincarries,
                                "50" = df_rb_rundist50_cum,
                                "100" = df_rb_rundist100_cum,
                                "150" = df_rb_rundist150_cum,
                                "200" = df_rb_rundist200_cum)
    players_knn_wide_index <- switch(input$mincarries,
                                     "50" = players_knn_wide_50_index,
                                     "100" = players_knn_wide_100_index,
                                     "150" = players_knn_wide_150_index,
                                     "200" = players_knn_wide_200_index)
    paste(playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,input$playertoplot,10),",")
  })
  output$cumcarries <- renderPlot({
    cumcarriesplot <- playercomp_cumden(rb_app,input$playertoplot,input$playermatch)
    print(cumcarriesplot)
  })
})