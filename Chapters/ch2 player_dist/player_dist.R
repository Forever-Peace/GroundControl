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

#Player vs. Total
den_all <- function(df,player1)
{
  density<-ggplot(df_rb, aes(rushing_yds)) + geom_density(fill='blue',alpha = 0.2,adjust=1.85)+ coord_cartesian(xlim=c(-10, 20))+ylab ("Probability")+xlab ("Rushing Yards") +
    labs(title="Run Distribution")+theme(plot.title = element_text(size = rel(2)))+
    theme(axis.title=element_text(size="16"))+
    scale_y_continuous(breaks=seq(0, 0.15, 0.05))+scale_x_continuous(breaks=seq(-10, 20, 5))
  density + geom_density(mapping = aes(rushing_yds), data = df[df$full_name==player1,],fill='red',alpha = 0.2) +theme(legend.title = element_text(size=16, face="bold"))+ggtitle(player1)
}

den_all(df_rb,"Adrian Peterson")

#Grinders
den_all(df_rb,"Frank Gore")
den_all(df_rb,"Alfred Morris")
den_all(df_rb,"Maurice Jones-Drew")
den_all(df_rb,"LeGarrette Blount")
den_all(df_rb,"Mark Ingram")
den_all(df_rb,"Eddie Lacy")

#Home Run Hitters
den_all(df_rb,"LeSean McCoy")
den_all(df_rb,"Adrian Peterson")
den_all(df_rb,"Justin Forsett")
den_all(df_rb,"Reggie Bush")
den_all(df_rb,"Christine Michael")

#Short-Yardage Specialists
den_all(df_rb,"Shonn Greene")
den_all(df_rb,"John Kuhn")
den_all(df_rb,"Rashad Jennings")
den_all(df_rb,"Peyton Hillis")
den_all(df_rb,"Daniel Thomas")
den_all(df_rb,"BenJarvus Green-Ellis")
den_all(df_rb,"Jonathan Grimes")

#Crap
den_all(df_rb,"LaRod Stephens-Howling")
den_all(df_rb,"Trent Richardson")
den_all(df_rb,"Joique Bell")
den_all(df_rb,"Bernard Pierce")
den_all(df_rb,"Alfred Blue")
den_all(df_rb,"Isaiah Crowell")
den_all(df_rb,"Tre Mason")

#Pass-catching backs
den_all(df_rb,"Danny Woodhead")
den_all(df_rb,"Pierre Thomas")
den_all(df_rb,"Shane Vereen")
den_all(df_rb,"Darren Sproles")
den_all(df_rb,"Roy Helu")

#Game Breaking Talents
den_all(df_rb,"DeMarco Murray")
den_all(df_rb,"Le'Veon Bell")
den_all(df_rb,"LaDainian Tomlinson")
den_all(df_rb,"Jamaal Charles")
den_all(df_rb,"Jerick McKinnon")
den_all(df_rb,"Thomas Rawls")

#
#
#Player vs. Total cumulative
den_all_cum <- function(df,player1)
{
  density<-ggplot(df_rb, aes(rushing_yds)) + stat_ecdf(color='blue',alpha = 0.4,size=1.4)+ coord_cartesian(xlim=c(-10, 20))+ylab ("Proportion of Run Death")+xlab ("Rushing Yards") +
    labs(title="Run Distribution")+theme(plot.title = element_text(size = rel(2)))+
    theme(axis.title=element_text(size="16"))+
    scale_y_reverse(breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+scale_x_continuous(breaks=seq(-10, 20, 5))
  density + stat_ecdf(mapping = aes(rushing_yds), data = df[df$full_name==player1,],color='red',alpha = 0.4,size=1.4) +theme(legend.title = element_text(size=16, face="bold"))+ggtitle(player1)
}

den_all_cum(df_rb,"Adrian Peterson")

#Grinders
den_all_cum(df_rb,"Frank Gore")
den_all_cum(df_rb,"Alfred Morris")
den_all_cum(df_rb,"Mark Ingram")
den_all_cum(df_rb,"Maurice Jones-Drew")
den_all_cum(df_rb,"LeGarrette Blount")
den_all_cum(df_rb,"Eddie Lacy")

#Home Run Hitters
den_all_cum(df_rb,"LeSean McCoy")
den_all_cum(df_rb,"Adrian Peterson")
den_all_cum(df_rb,"Justin Forsett")
den_all_cum(df_rb,"Reggie Bush")
den_all_cum(df_rb,"Christine Michael")

#Short-Yardage Specialists
den_all_cum(df_rb,"John Kuhn")
den_all_cum(df_rb,"Shonn Greene")
den_all_cum(df_rb,"Peyton Hillis")
den_all_cum(df_rb,"Daniel Thomas")
den_all_cum(df_rb,"BenJarvus Green-Ellis")
den_all_cum(df_rb,"Jonathan Grimes")
den_all_cum(df_rb,"Rashad Jennings")

#Crap
den_all_cum(df_rb,"Alfred Blue")
den_all_cum(df_rb,"Trent Richardson")
den_all_cum(df_rb,"Bernard Pierce")
den_all_cum(df_rb,"LaRod Stephens-Howling")
den_all_cum(df_rb,"Joique Bell")
den_all_cum(df_rb,"Isaiah Crowell")
den_all_cum(df_rb,"Tre Mason")

#Pass-catching backs
den_all_cum(df_rb,"Danny Woodhead")
den_all_cum(df_rb,"Pierre Thomas")
den_all_cum(df_rb,"Darren Sproles")
den_all_cum(df_rb,"Shane Vereen")
den_all_cum(df_rb,"Roy Helu")

#Game Breaking Talents
den_all_cum(df_rb,"DeMarco Murray")
den_all_cum(df_rb,"Le'Veon Bell")
den_all_cum(df_rb,"LaDainian Tomlinson")
den_all_cum(df_rb,"Jamaal Charles")
den_all_cum(df_rb,"Jerick McKinnon")
den_all_cum(df_rb,"Thomas Rawls")

#
#
#Cumulative Player Comparisons
playercomp_cumden <- function(df,player1,player2)
{
  p <- ggplot(rbind(df[df$full_name==player1,],df[df$full_name==player2,]), 
              aes(rushing_yds,color=full_name))
  p + stat_ecdf(aes(color=full_name),alpha = 0.8,size=1.4) + coord_cartesian(xlim=c(-10, 20)) + ylab("Proportion of Run Death")+xlab ("Rushing Yards")+ scale_y_reverse(breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
    theme(legend.title = element_text(size=16, face="bold"))+scale_color_discrete(name="Player")+
    labs(title="Comparison of Cumulative Distributions")+theme(plot.title = element_text(size = rel(2)))+
    theme(axis.title=element_text(size="16"))
}


#Grinder vs various
playercomp_cumden(df_rb,'Frank Gore','LeSean McCoy')
playercomp_cumden(df_rb,'Frank Gore','John Kuhn')
playercomp_cumden(df_rb,'Frank Gore','Trent Richardson') #Include
playercomp_cumden(df_rb,'Frank Gore','Danny Woodhead')
playercomp_cumden(df_rb,'Frank Gore','Jamaal Charles') #Include

#Home-run hitters vs various
playercomp_cumden(df_rb,'Frank Gore','LeSean McCoy')
playercomp_cumden(df_rb,'LeSean McCoy','John Kuhn')
playercomp_cumden(df_rb,'LeSean McCoy','Trent Richardson')
playercomp_cumden(df_rb,'LeSean McCoy','Danny Woodhead')
playercomp_cumden(df_rb,'LeSean McCoy','Jamaal Charles') #Include

#Short-Yardage Specialists vs various
playercomp_cumden(df_rb,'Frank Gore','John Kuhn')
playercomp_cumden(df_rb,'LeSean McCoy','John Kuhn') #Include
playercomp_cumden(df_rb,'John Kuhn','Trent Richardson')
playercomp_cumden(df_rb,'John Kuhn','Danny Woodhead')
playercomp_cumden(df_rb,'John Kuhn','Jamaal Charles')

#Crap vs. various
playercomp_cumden(df_rb,'Frank Gore','Trent Richardson')
playercomp_cumden(df_rb,'LeSean McCoy','Trent Richardson')
playercomp_cumden(df_rb,'John Kuhn','Trent Richardson')
playercomp_cumden(df_rb,'Trent Richardson','Danny Woodhead')
playercomp_cumden(df_rb,'Trent Richardson','Jamaal Charles') #Include

#Pass-catching backs vs. various
playercomp_cumden(df_rb,'Frank Gore','Danny Woodhead')
playercomp_cumden(df_rb,'LeSean McCoy','Danny Woodhead')
playercomp_cumden(df_rb,'John Kuhn','Danny Woodhead')
playercomp_cumden(df_rb,'Trent Richardson','Danny Woodhead')
playercomp_cumden(df_rb,'Danny Woodhead','Jamaal Charles') #Include




