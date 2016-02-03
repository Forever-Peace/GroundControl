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

#2015 Run proportions
#calc
df_rb_rundist2015 <- dcast(df_rb[df_rb$year==2015,], full_name ~ rushing_yds, length)
df_rb_rundist2015$carries <- rowSums(df_rb_rundist2015[,-1], na.rm = TRUE)
df_rb_rundist2015 <- df_rb_rundist2015[df_rb_rundist2015$carries > 50,]

row.names(df_rb_rundist2015) <- df_rb_rundist2015$full_name
df_rb_rundist2015$carries <- NULL
df_rb_rundist_table <- prop.table(as.table(as.matrix(df_rb_rundist2015[,-1])),1)
df_rb_rundist2015 <- as.data.frame.matrix(df_rb_rundist_table) 
df_rb_rundist_cum2015 <- data.frame(t(apply(df_rb_rundist2015,1,cumsum)))

#Positive yardage
a<-1-df_rb_rundist_cum2015[,"X0",drop=FALSE]
round(a[order(-a$X0),"X0",drop=FALSE],3)

#Gained at least 3 yards
a<-1-df_rb_rundist_cum2015[,"X2",drop=FALSE]
round(a[order(-a$X2),"X2",drop=FALSE],3)

#Gained at least 4 yards
a<-1-df_rb_rundist_cum2015[,"X3",drop=FALSE]
round(a[order(-a$X3),"X3",drop=FALSE],3)

#Gained at least 5 yards
a<-1-df_rb_rundist_cum2015[,"X4",drop=FALSE]
round(a[order(-a$X4),"X4",drop=FALSE],3)

#Gained at least 10 yards
a<-1-df_rb_rundist_cum2015[,"X9",drop=FALSE]
round(a[order(-a$X9),"X9",drop=FALSE],3)

#Gained at least 20 yards
a<-1-df_rb_rundist_cum2015[,"X19",drop=FALSE]
round(a[order(-a$X19),"X19",drop=FALSE],3)


#Career Run proportions
#calc (100 carry min)
df_rb_rundist <- dcast(df_rb, full_name ~ rushing_yds, length)
df_rb_rundist$carries <- rowSums(df_rb_rundist[,-1], na.rm = TRUE)
df_rb_rundist <- df_rb_rundist[df_rb_rundist$carries > 99,]

row.names(df_rb_rundist) <- df_rb_rundist$full_name
df_rb_rundist$carries <- NULL
df_rb_rundist_table <- prop.table(as.table(as.matrix(df_rb_rundist[,-1])),1)
df_rb_rundist <- as.data.frame.matrix(df_rb_rundist_table) 
df_rb_rundist_cum <- data.frame(t(apply(df_rb_rundist,1,cumsum)))

#Positive yardage
a<-1-df_rb_rundist_cum[,"X0",drop=FALSE]
round(a[order(-a$X0),"X0",drop=FALSE],3)

#Gained at least 3 yards
a<-1-df_rb_rundist_cum[,"X2",drop=FALSE]
round(a[order(-a$X2),"X2",drop=FALSE],3)

#Gained at least 4 yards
a<-1-df_rb_rundist_cum[,"X3",drop=FALSE]
round(a[order(-a$X3),"X3",drop=FALSE],3)

#Gained at least 5 yards
a<-1-df_rb_rundist_cum[,"X4",drop=FALSE]
round(a[order(-a$X4),"X4",drop=FALSE],3)

#Gained at least 10 yards
a<-1-df_rb_rundist_cum[,"X9",drop=FALSE]
round(a[order(-a$X9),"X9",drop=FALSE],3)

#Gained at least 20 yards
a<-1-df_rb_rundist_cum[,"X19",drop=FALSE]
round(a[order(-a$X19),"X19",drop=FALSE],3)

