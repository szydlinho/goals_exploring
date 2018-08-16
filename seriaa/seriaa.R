library(ggplot2)
library(reshape2)
library(gridExtra)
library(ggExtra)

setwd("G:/data_football/seriaa/")
dane <- read.csv("seriaa_raw.csv", header=T)
head(dane)
length(colnames(dane))
dane[1,30:31] <- "ND"
summary(dane)

sezony <- 1:18

H <- c()
A <- c()
D <- c()

for (i in 1:length(sezony)) {
  H[i] <- sum((dane$FTR=="H") & (dane$Sezon==i))
  A[i] <- sum((dane$FTR=="A") & (dane$Sezon==i))
  D[i] <- sum((dane$FTR=="D") & (dane$Sezon==i))
  
}
had <- list(H, A, D)
df <- as.data.frame(had)
colnames(df) <- c("H", "A", "D")
df$Sezon <- sezony
df <- df[,c(4,1,2,3)]
df$Suma <- df$H+df$A+df$D
df$H <- df$H/df$Suma
df$A <- df$A/df$Suma
df$D <- df$D/df$Suma
df[,c(2,3,4)] <- round(df[,c(2,3,4)], 5)
df <- df[,c(1,2,3,4)] 

d <- melt(df, id.vars="Sezon")

ggplot(data = d, aes(x=Sezon, y=value, color=variable)) + 
  geom_point(size=3) + stat_smooth(se=F, method="auto")+
  labs(title="Wyniki koñcowe- Seria A XXI w.", x="Sezon", y="Procent wyników meczów") +
  scale_color_manual(labels = c("Gospodarz", "Goœæ", "Remis"), values = c("blue", "green", "red")) +
  theme_bw() +
  guides(color=guide_legend("Wynik koñcowy")) +
  scale_x_continuous(limits=c(1,17), breaks=c(0:17))


ggplot(data = d, aes(x=Sezon, y=value, fill=variable)) + geom_bar(stat = "identity") +
  labs(title="Wyniki koñcowe - Seria A XXI w.", x="Sezon", y="Procent wyst¹pieñ wyników meczów") +
  scale_fill_discrete(name="Wynik koñcowy", breaks=c("H", "A", "D"),
                      labels = c("Gospodarz", "Goœæ", "Remis"))+
  scale_x_continuous(limits=c(0,18), breaks=c(0:18))  + geom_hline(yintercept=0.5, linetype="dashed", color = "white")

ggsave("Wyniki koñcowe - Seria A XXI.png", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 15, height = 10, units = c("cm"),
       dpi = 400)

n_homewins <-  sum(dane$FTR=="H")
n_draw <- sum(dane$FTR=="D")
n_away <- sum(dane$FTR=="A")
win_rate <- round((n_homewins)/(nrow(dane))*100, 2)
draw_rate <- round((n_draw/nrow(dane))*100, 2)
away_rate <- round((n_away/nrow(dane))*100, 2)
value <- c(n_homewins, n_draw, n_away)
etykiety <- c(paste("Goœæ ", away_rate, "%"), paste("Remis", draw_rate, "%"), paste("Gospodarz", win_rate, "%"))

###Rozk³ad czêstoœci wyników meczów 

ggplot(dane,aes(x = factor(1), fill = as.factor(FTR))) +
  geom_bar(width=1) + coord_polar(theta = "y")+ 
  ggtitle("Czêstoœæ wyniku koñcowego - Seria A")+ 
  scale_fill_discrete(name="Wynik Koñcowy", breaks=c("A", "D", "H"),
 labels=etykiety) + theme(axis.title.x=element_blank(), axis.title.y = element_blank(),
                          axis.ticks.x=element_blank(), axis.ticks.y = element_blank())

ggsave("Czêstoœæ wyniku koñcowego - SeriaA.png", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 15, height = 10, units = c("cm"),
       dpi = 400)

df2 <- dane[,c("FTHG", "FTAG")]
df2 <- melt(df2)
library(plyr)

counts <- ddply(df2, .(df2$variable, df$value), nrow)
library(data.table)
dt <- data.table(df2)
dt <- dt[, list(Freq =.N), by=list(variable,value)]
df2 <- as.data.frame(dt)

ggplot(data = df2, aes(x=value, y=Freq, fill=variable))+
  geom_histogram(stat="identity",position="dodge") +
  labs(title="Liczebnoœæ goli w meczu - Seria A XXI w.", x="Iloœæ goli", y="LIczebnoœæ wyst¹pieñ")+ 
  scale_fill_discrete(name="Gole", breaks=c("FTHG", "FTAG"),
                      labels=c("Bramki Gospodarzy", "Bramki Goœci"))  +
  scale_x_continuous(limits=c(-1,9), breaks=c(0,1,2,3,4,5,6,7,8,9))
ggsave("Liczebnoœæ goli w meczu - SeriaA XXI.png", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 15, height = 10, units = c("cm"),
       dpi = 400)
###Rozk³ad bramek strzelonych przez gospodarza i goœcia

p11 <- ggplot(dane, aes(x=FTHG, fill=FTR)) + 
  geom_histogram(binwidth=0.5, position="dodge")+ 
  scale_x_continuous(limits=c(-1,9), breaks=c(0,1,2,3,4,5,6,7,8,9)) +  
  theme(legend.position="none", axis.title.y = element_blank()) +
  xlab("Bramki Gospodarzy") 
  
p22<- ggplot(dane, aes(x=FTAG, fill=FTR)) + geom_histogram(binwidth=0.5, position="dodge")+ 
  scale_x_continuous(limits=c(-1,9), breaks=c(0,1,2,3,4,5,6,7,8,9))+
  xlab("Bramki Goœci") + theme(axis.title.y = element_blank(), legend.position = "bottom")+ 
  scale_fill_discrete(name="Wynik Koñcowy", breaks=c("A", "D", "H"), labels=c("Goœæ", "Remis", "Gospodarz")) 
p12 <- grid.arrange(p11, p22, nrow= 2,  top = "Rozk³ad bramek wg. wyniku - Seria A")

ggsave("Rozk³ad bramek wg. wyniku - SeriaA.png", plot = p12, device = NULL, path = NULL,
       scale = 1, width = 18, height = 15, units = c("cm"),
       dpi = 400)


g <- ggplot(dane, aes(FTHG, FTAG)) + geom_count(aes(y = (..count..)/sum(..count..)),col="green", position = "identity", alpha=0.5) +
  scale_size_area(max_size = 13)+ 
  xlab("Bramki strzelone przez gospodarzy") + 
  ylab("Bramki strzelone przez dru¿ynê goœci")+
  scale_x_continuous(limits=c(-1,9), breaks=c(0,1,2,3,4,5,6,7,8,9)) + 
  scale_y_continuous(limits=c(-1,9), breaks=c(0,1,2,3,4,5,6,7,8)) + 
  labs(title="Rozk³ad czêstoœci wyników meczów Seria A")
ggMarginal(g, type = "boxplot", fill="transparent", size=10)

ggsave("Rozk³ad czêstoœci wyników meczów Seria A.png", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 15, height = 10, units = c("cm"),
       dpi = 400)

attach(dane)
mytable <- table(FTHG,FTAG) 
mytable 

x <- melt(mytable)
names(x) <- c("FTHG","FTAG","value")

ggplot(x, aes(FTHG, FTAG)) +
  geom_point(aes(size = value), alpha=0.6, color="darkgreen", show_guide=FALSE) +
  geom_text(aes(label = value), color="white") +
  scale_size(range = c(0,50)) +
  theme_bw()

margin.table(mytable, 1) 
margin.table(mytable, 2) 


xx <- round(prop.table(mytable) * 100,3)
xx
y <- melt(xx)


ggplot(y, aes(FTHG, FTAG)) +
  geom_point(aes(size = value), alpha=0.6, color="lightgreen", show.legend=FALSE) +
  geom_text(aes(label = value), color="black")  +
  scale_size(range = c(0,50)) +
  theme_classic() + labs(title="Rozk³ad czêstoœci wyników meczów - Seria A XXI w.", x="Iloœæ goli Gospodarzy", y="Iloœæ Goli Goœci")+
  scale_x_continuous(limits=c(0,9), breaks=c(0,1,2,3,4,5,6,7,8,9)) + 
  scale_y_continuous(limits=c(0,9), breaks=c(0,1,2,3,4,5,6,7,8))  

ggsave("Rozk³ad czêstoœci wyników meczów - Seria A XXI.png", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 15, height = 10, units = c("cm"),
       dpi = 400)

prop.table(mytable, 1) 
prop.table(mytable, 2) 


gp <- dane[, c("FTHG", "FTAG")]
gp$pkt_gosp <- 0
gp$pkt_gosc <- 0

for (i in 1:length(gp$FTHG)){
  if (gp[i,"FTHG"] > gp[i,"FTAG"]){
    gp[i,"pkt_gosp"] = 3
    gp[i,"pkt_gosc"] = 0
  }
  else if (gp[i,"FTHG"] < gp[i,"FTAG"]){
    gp[i,"pkt_gosp"] = 0
    gp[i,"pkt_gosc"] = 3    
  }
  else{
    gp[i,"pkt_gosp"] = 1
    gp[i,"pkt_gosc"] = 1  
  }
}


a <- gp[,c(1,3)]
b <- gp[,c(2,4)]
names(a) <- c("gole", "punkty")
names(b) <- c("gole", "punkty")
gpp <- rbind(a,b)


#ggplot(gpp, aes(x=gole, y=punkty)) + 
#  geom_point()  + geom_smooth(method = "loess")


attach(gpp)
mytable1 <- table(gole,punkty) 
mytable1 


xx2 <- round(prop.table(mytable1) * 100,3)
xx2
y2 <- melt(xx2)

ggplot(y2, aes(gole, punkty)) +
  geom_point(aes(size = value), alpha=0.6, color="darkgreen", show.legend=FALSE) +
  geom_text(aes(label = value), color="white") +
  scale_size(range = c(0,50)) +
  theme_dark() + labs(title="Gole ~ Punkty; Seria A", x="Gole", y="Punkty")+
  scale_x_continuous(limits=c(-1,9), breaks=c(0,1,2,3,4,5,6,7,8,9)) + 
  scale_y_continuous(limits=c(-1,4), breaks=c(0,1,3))  


ggsave("Gole Punkty - SeriaA XXI.png", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 15, height = 10, units = c("cm"),
       dpi = 400)
 # scale_x_continuous(limits=c(0,9), breaks=c(0,1,2,3,4,5,6,7,8,9)) + 
#  scale_y_continuous(limits=c(0,9), breaks=c(0,1,2,3,4,5,6,7,8))  