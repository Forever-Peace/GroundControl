df = read.csv("rushing_data_stack.csv")

df_rb = df[df$FantPos=='RB',]

df_rb$week <- factor(df_rb$week)
df_rb$down <- factor(df_rb$down)
df_rb$year <- factor(df_rb$year)
df_rb$FantPos <- factor(df_rb$FantPos)
df_rb$full_name <- factor(df_rb$full_name)

library('ggplot2')
library('reshape2')
library("dplyr")

#Run distribution calculations
df_rb_rundist <- dcast(df_rb, full_name ~ rushing_yds, length)
df_rb_rundist$carries <- rowSums(df_rb_rundist[,-1], na.rm = TRUE)
df_rb_rundist <- df_rb_rundist[df_rb_rundist$carries > 50,]

row.names(df_rb_rundist) <- df_rb_rundist$full_name
df_rb_rundist$carries <- NULL
df_rb_rundist_table <- prop.table(as.table(as.matrix(df_rb_rundist[,-1])),1)
df_rb_rundist <- as.data.frame.matrix(df_rb_rundist_table) 
df_rb_rundist_cum <- data.frame(t(apply(df_rb_rundist,1,cumsum)))

#Playermatching
library("FNN")
players_knn <- get.knn(df_rb_rundist_cum[which( colnames(df_rb_rundist_cum)=="X0" ):which( colnames(df_rb_rundist_cum)=="X8" )],k=180,algorithm="brute")
players_knn_index <- players_knn["nn.index"][[1]]
players_knn_index[7,] #matches for 7th player in database
row.names(df_rb_rundist_cum[players_knn_index[7,1],]) #best match
row.names(df_rb_rundist_cum[players_knn_index[7,180],]) #worst match

players_knn_wide <- get.knn(df_rb_rundist_cum[which( colnames(df_rb_rundist_cum)=="X.3" ):which( colnames(df_rb_rundist_cum)=="X15" )],k=180,algorithm="brute")
players_knn_wide_index <- players_knn_wide["nn.index"][[1]]
players_knn_wide_index[7,] #matches for 7th player in database
row.names(df_rb_rundist_cum[players_knn_wide_index[7,1],]) #best match
row.names(df_rb_rundist_cum[players_knn_wide_index[7,180],]) #worst match

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

#Archetype players
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"LeSean McCoy",10)
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"LeSean McCoy",1)
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"LeSean McCoy",2) #AP, show.
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"LeSean McCoy",3)
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"LeSean McCoy",4)
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"LeSean McCoy",5)
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"LeSean McCoy",6) #Hyde not as good at home runs, but JStew is close.

playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Frank Gore",10)
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Frank Gore",1) #Forte
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Frank Gore",2) #Hightower
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Frank Gore",3) #Morris, show
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Frank Gore",4) #Ivory

playercomp_knn_list(df_rb_rundist_cum,players_knn_index,"Shonn Greene",10) #note: not wide.
playercomp_knn(df_rb_rundist_cum,players_knn_index,"Shonn Greene",1) #Jennings, show.
playercomp_knn(df_rb_rundist_cum,players_knn_index,"Shonn Greene",2)
playercomp_knn(df_rb_rundist_cum,players_knn_index,"Shonn Greene",3)
playercomp_knn(df_rb_rundist_cum,players_knn_index,"Shonn Greene",4)

playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Alfred Blue",10)
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Alfred Blue",1)
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Alfred Blue",2)
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Alfred Blue",3) #Richardson, show.
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Alfred Blue",4)

playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Danny Woodhead",10)
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Danny Woodhead",1) #Jonathan Grimes, show.
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Danny Woodhead",2) 
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Danny Woodhead",3)
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Danny Woodhead",4)
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Danny Woodhead",5) #Pierre Thomas
#All five of the first five comps are 5'9 or 5'10, like Woodhead.

playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Le'Veon Bell",10)
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Le'Veon Bell",1) #Fred Jackson, show
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Le'Veon Bell",2) #Willis McGahee
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Le'Veon Bell",3) #Gio
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Le'Veon Bell",4) #MJD

#Backwards Matching: Find young matches for known quantities.
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Le'Veon Bell",3) #Gio, show.

playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Jamaal Charles",10) #CJA certainly LOOKED like the next world-beater at end of 2014.
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Jamaal Charles",1) #Karlos, show.
playercomp_knn(df_rb_rundist_cum,players_knn_wide_index,"Jamaal Charles",5) #Rawls, show.

playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Marshawn Lynch",10) #no young
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Matt Forte",10) #no young

playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"DeMarco Murray",10) #Ridley/Miller/Morris/McKinnon/David Johnson

playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Trent Richardson",10)

#Forwards Matching: Take a young player and find older matches. Listing top 10.
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Thomas Rawls",10) #Jamaal Charles, but better. Karlos Williams.
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Karlos Williams",10) #Jamaal Charles. Thomas Rawls.
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"David Johnson",10) #Lamar Miller, DeMarco Murray, Stevan Ridley
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"T.J. Yeldon",10) #Frank Gore, CJ2K, Ronnie Hillman
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Todd Gurley",10) #Leon Washington, James Starks, Joseph Randle
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Tevin Coleman",10) #Knowshon Moreno, Marshawn Lynch, Ryan Mathews
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Ameer Abdullah",10) #DeMarco Murray and Marshawn Lynch, but worse at 10-20 yard range.
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Jeremy Langford",10) #Peyton Hillis, Rashad Jennings (but a little worse at 4-10 yard range)
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Matt Jones",10) #Tre Mason, Bobby Rainey
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Melvin Gordon",10) #Shaun Draughn and Chris Polk, but worse. 
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Javorius Allen",10) #Joique Bell, Donald Brown, Cadillac Williams
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Duke Johnson",10) #Jacquizz Rodgers, Cedric Benson
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"David Cobb",10) #Alfred Blue and Trent Richardson, but WAY worse.


playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Carlos Hyde",10) #Jonathan Stewart, but better.
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Jerick McKinnon",10) #DeMarco Murray, but better. 
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Jeremy Hill",10) #Maurice Jones-Drew and Willis McGahee, but better at short yardage. Late-career LaDainian Tomlinson.
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Giovani Bernard",10) #Felix Jones, Fred Jackson, LaGarrette Blount.
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Charcandrick West",10) #Jacquizz Rodgers, Steven Jackson, Doug Martin, Tim Hightower.
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Bishop Sankey",10) #Shonn Greene. Rashad Jennings but worse at 10+ yards.
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Devonta Freeman",10) #Brandon Bolden. CJ Spiller but WAY worse from 0-5 yards.
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Latavius Murray",10) #Darren McFadden, Steven Jackson, 2010 Brandon Jackson.
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Alfred Blue",10) #poopoo
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Terrance West",10) #Willis McGahee. Bilal Powell but worse.
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Antonio Andrews",10) #Jackie Battle, but slightly worse.
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Isaiah Crowell",10) #Not really anybody. Javon Ringer or Ryan Torain maybe? Tre Mason, kind of.
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Tre Mason",10) #Also not really anybody. Dujuan Harris? The algorithm places him as far from Jamaal Charles as possible (match 178 out of 180)
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Charles Sims",10) #Not really anybody. Dexter McCluster?
playercomp_knn_list(df_rb_rundist_cum,players_knn_wide_index,"Andre Williams",10) #Literally the worst run distribution I have ever seen. LaRod Stephens-Howling, but worse. 2010 Correll Buckhalter (where he averaged 2.5 YPC) is match 6, and 32-year-old Buckhalter comes out looking like the far better player.



