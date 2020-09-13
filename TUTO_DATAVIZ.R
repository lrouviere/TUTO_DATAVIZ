## ----include=FALSE----------------------------------------
# automatically create a bib database for R packages
knitr::write_bib(c(
  .packages(), 'bookdown', 'knitr', 'rmarkdown'
), 'packages.bib')

## ---- eval=FALSE, echo=TRUE-------------------------------
#  demo(graphics)

## ---------------------------------------------------------
x <- seq(-2*pi,2*pi,by=0.05)
y <- sin(x)
plot(x,y) #points (par défaut)
plot(x,y,type="l") #représentation sous forme de ligne

## ---------------------------------------------------------
ozone <- read.table("data/ozone.txt")
summary(ozone)

## ---------------------------------------------------------
plot(ozone[,"T12"],ozone[,"maxO3"])

## ---------------------------------------------------------
plot(maxO3~T12,data=ozone)

## ---------------------------------------------------------
plot(ozone[,"T12"],ozone[,"maxO3"],xlab="T12",ylab="maxO3")

## ---------------------------------------------------------
hist(ozone$maxO3,main="Histogram")
barplot(table(ozone$vent)/nrow(ozone),col="blue")
boxplot(maxO3~vent,data=ozone)

## ---------------------------------------------------------
library(rAmCharts)
amHist(ozone$maxO3)
amPlot(ozone,col=c("T9","T12"))
amBoxplot(maxO3~vent,data=ozone)

## ----test-a,teacher=correct-------------------------------
x <- seq(0,2*pi,length=1000)
plot(x,sin(x),type="l")
title("Plot of the sine function")

## ----teacher=correct--------------------------------------
x <- seq(-4,4,by=0.01)
plot(x,dnorm(x),type="l")
abline(v=0,lty=2)
lines(x,dt(x,5),col=2)
lines(x,dt(x,30),col=3)
legend("topleft",legend=c("Gaussian","Student(5)","Student(30)"),
   col=1:3,lty=1)

## ----teacher=correct--------------------------------------
spots <- read.table("data/taches_solaires.csv",sep=";",header=TRUE,dec=",")

## ----teacher=correct--------------------------------------
library(tidyverse)
period <- cut_interval(spots$annee,n=8)

## ----eval=correct,echo=TRUE-------------------------------
mycolors <- c("yellow", "magenta", "orange", "cyan",
          "grey", "red", "green", "blue")

## ----teacher=correct--------------------------------------
levels(period) <- mycolors

## ----teacher=correct--------------------------------------
coordx <- seq(along=spots[,1])

## ----teacher=correct--------------------------------------
plot(coordx,spots[,1],xlab="Temps",ylab="Nombre de taches",
 col=period,type="p",pch="+")

## ----teacher=correct--------------------------------------
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(maxO3~T12,data=ozone)
hist(ozone$T12)
boxplot(ozone$maxO3)

## ---------------------------------------------------------
library(tidyverse)
set.seed(1234)
diamonds2 <- diamonds[sample(nrow(diamonds),5000),] 
summary(diamonds2)
help(diamonds)

## ---------------------------------------------------------
plot(price~carat,data=diamonds2)

## ---------------------------------------------------------
ggplot(diamonds2) #nothing
ggplot(diamonds2)+aes(x=carat,y=price) #nothing
ggplot(diamonds2)+aes(x=carat,y=price)+geom_point() #OK

## ----teacher=correct--------------------------------------
ggplot(diamonds2)+aes(x=carat)+geom_histogram()
ggplot(diamonds2)+aes(x=carat)+geom_histogram(bins=10)
ggplot(diamonds2)+aes(x=cut)+geom_bar()

## ----eval=FALSE-------------------------------------------
#  ggplot(diamonds2)+aes(x=carat,y=price)

## ----eval=FALSE-------------------------------------------
#  ggplot(diamonds2)+aes(x=carat,y=price,color=cut)

## ---------------------------------------------------------
ggplot(diamonds2)+aes(x=carat,y=price,color=cut)+geom_point()

## ----teacher=correct--------------------------------------
ggplot(diamonds2)+aes(x=cut)+geom_bar(fill="blue")

## ----teacher=correct--------------------------------------
ggplot(diamonds2)+aes(x=cut,fill=cut)+geom_bar()

## ----teacher=correct--------------------------------------
ggplot(diamonds2)+aes(x=cut)+geom_bar(fill=c("blue","red","green","yellow","black"))

## ---------------------------------------------------------
D <- data.frame(X=seq(-2*pi,2*pi,by=0.01))
ggplot(D)+aes(x=X,y=sin(X))+geom_line()

## ---------------------------------------------------------
ggplot(diamonds2)+aes(x=price)+geom_histogram(bins=40)

## ---------------------------------------------------------
ggplot(diamonds2)+aes(x=price,y=..density..)+geom_histogram(bins=40)

## ----eval=FALSE,echo=TRUE---------------------------------
#  ggplot(diamonds2)+aes(x=price,y=..density..)+stat_bin()

## ----teacher=correct--------------------------------------
X <- data.frame(X1=c("red","blue","green","black"),prob=c(0.3,0.2,0.4,0.1))
ggplot(X)+aes(x=X1,y=prob,fill=X1)+geom_bar(stat="identity")+labs(fill="Color")+xlab("")

## ----teacher=correct--------------------------------------
ggplot(diamonds2)+aes(x=carat,y=price)+geom_smooth(method="loess")
ggplot(diamonds2)+aes(x=carat,y=price)+stat_smooth(method="loess")

## ----teacher=correct--------------------------------------
ggplot(diamonds2)+aes(x=carat,y=price)+geom_smooth(method="loess",linetype="dotted")
ggplot(diamonds2)+aes(x=carat,y=price)+stat_smooth(method="loess",geom="point")

## ---------------------------------------------------------
ggplot(diamonds2)+aes(x=carat,y=price,color=cut)+geom_point()+
  scale_color_manual(values=c("Fair"="black","Good"="yellow",
                              "Very Good"="blue","Premium"="red","Ideal"="green"))

## ---------------------------------------------------------
p1 <- ggplot(diamonds2)+aes(x=cut)+geom_bar(aes(fill=cut))
p1

## ---------------------------------------------------------
p1+scale_fill_brewer(palette="Purples")

## ---------------------------------------------------------
p2 <- ggplot(diamonds2)+aes(x=carat,y=price)+geom_point(aes(color=depth))
p2

## ---------------------------------------------------------
p2+scale_color_gradient(low="red",high="yellow")

## ---------------------------------------------------------
p2+scale_x_continuous(breaks=seq(0.5,3,by=0.5))+scale_y_continuous(name="prix")+scale_color_gradient("Profondeur")

## ---------------------------------------------------------
ggplot(diamonds2)+aes(x=carat,y=price,group=cut)+
  geom_smooth(method="loess")

## ---------------------------------------------------------
ggplot(diamonds2)+aes(x=carat,y=price)+
  geom_smooth(method="loess")+facet_wrap(~cut)
ggplot(diamonds2)+aes(x=carat,y=price)+
  geom_smooth(method="loess")+facet_wrap(~cut,nrow=1)

## ---------------------------------------------------------
ggplot(diamonds2)+aes(x=carat,y=price)+geom_point()+
  geom_smooth(method="lm")+facet_grid(color~cut)
ggplot(diamonds2)+aes(x=carat,y=price)+geom_point()+
  geom_smooth(method="lm")+facet_wrap(color~cut)

## ----echo=TRUE,eval=FALSE---------------------------------
#  ggplot()+aes()+geom_()+scale_()

## ---------------------------------------------------------
ggplot(diamonds2)+aes(x=carat,y=price)+geom_point()
ggplot(diamonds2,aes(x=carat,y=price))+geom_point()
ggplot(diamonds2)+geom_point(aes(x=carat,y=price))

## ---------------------------------------------------------
X <- seq(-2*pi,2*pi,by=0.001)
Y1 <- cos(X)
Y2 <- sin(X)
donnees1 <- data.frame(X,Y1)
donnees2 <- data.frame(X,Y2)
ggplot(donnees1)+geom_line(aes(x=X,y=Y1))+
  geom_line(data=donnees2,aes(x=X,y=Y2),color="red")

## ---------------------------------------------------------
p <- ggplot(diamonds2)+aes(x=carat,y=price,color=cut)+geom_point()
p+theme_bw()
p+theme_classic()
p+theme_grey()
p+theme_bw()

## ----teacher=correct--------------------------------------
X <- seq(-2*pi,2*pi,by=0.001)
Y1 <- cos(X)
Y2 <- sin(X)
donnees1 <- data.frame(X,Y1)
donnees2 <- data.frame(X,Y2)
ggplot(donnees1)+geom_line(aes(x=X,y=Y1))+
  geom_line(data=donnees2,aes(x=X,y=Y2),color="red")

## ----teacher=correct--------------------------------------
donnees <- data.frame(X,Y1,Y2)
ggplot(donnees)+aes(x=X,y=Y1)+geom_line()+
  geom_line(aes(y=Y2),color="red")
#ou pour la légende
ggplot(donnees)+aes(x=X,y=Y1)+geom_line(aes(color="cos"))+
  geom_line(aes(y=Y2,color="sin"))+labs(color="Fonction")

## ----teacher=correct--------------------------------------
df <- data.frame(X,cos=Y1,sin=Y2)
df1 <- df %>% pivot_longer(cols=c(cos,sin),
                       names_to = "Fonction",
                       values_to = "value")
#or
df1 <- df %>% pivot_longer(cols=-X,
                       names_to = "Fonction",
                       values_to = "value")
ggplot(df1)+aes(x=X,y=value,color=Fonction)+geom_line()

## ----teacher=correct--------------------------------------
ggplot(df1)+aes(x=X,y=value)+geom_line()+facet_wrap(~Fonction)

## ----teacher=correct--------------------------------------
library(gridExtra)
p1 <- ggplot(donnees1)+aes(x=X,y=Y1)+geom_line()
p2 <- ggplot(donnees2)+aes(x=X,y=Y2)+geom_line()
grid.arrange(p1,p2,nrow=1)

## ---------------------------------------------------------
data(mtcars)
summary(mtcars)

## ----teacher=correct--------------------------------------
ggplot(mtcars)+aes(x=mpg)+geom_histogram()
ggplot(mtcars)+aes(x=mpg)+geom_histogram(bins=10)

## ----teacher=correct--------------------------------------
ggplot(mtcars)+aes(x=mpg,y=..density..)+geom_histogram(bins=10)

## ----teacher=correct--------------------------------------
ggplot(mtcars)+aes(x=cyl)+geom_bar()

## ----teacher=correct--------------------------------------
ggplot(mtcars)+aes(x=disp,y=mpg,color=cyl)+geom_point()
ggplot(mtcars)+aes(x=disp,y=mpg,color=as.factor(cyl))+geom_point()+labs(color="cyl")

## ----teacher=correct--------------------------------------
ggplot(mtcars)+aes(x=disp,y=mpg,color=as.factor(cyl))+geom_point()+
  geom_smooth(method="lm")+labs(color="cyl")

## ----teacher=correct--------------------------------------
n <- 100
X <- runif(n)
eps <- rnorm(n,sd=0.2)
Y <- 3+X+eps
D <- data.frame(X,Y)

## ----teacher=correct--------------------------------------
model <- lm(Y~.,data=D)
co <- coef(model)
D$fit <- predict(model)
co <- coef(lm(Y~.,data=D))
ggplot(D)+aes(x=X,y=Y)+geom_point()+
  geom_abline(slope=co[2],intercept=co[1],color="blue")

## ----teacher=correct--------------------------------------
ggplot(D)+aes(x=X,y=Y)+geom_point()+geom_smooth(method="lm")

## ----teacher=correct--------------------------------------
ggplot(D)+aes(x=X,y=Y)+geom_point()+geom_smooth(method="lm")+
  geom_segment(aes(xend=X,yend=fit))

## ----echo=FALSE,eval=TRUE---------------------------------
ggplot(data=diamonds) + geom_boxplot(aes(x=cut,y=carat,fill=cut)) 
ggplot(data=diamonds) + geom_boxplot(aes(x=cut,y=carat,fill=cut))+coord_flip()
ggplot(data=diamonds) + geom_density(aes(x=carat,y=..density..)) +  facet_grid(cut~.)

## ----echo=correct,eval=FALSE------------------------------
#  ggplot(data=diamonds) + geom_boxplot(aes(x=cut,y=carat,fill=cut))
#  ggplot(data=diamonds) + geom_boxplot(aes(x=cut,y=carat,fill=cut))+coord_flip()
#  ggplot(data=diamonds) + geom_density(aes(x=carat,y=..density..)) +  facet_grid(cut~.)

## ----echo=correct,eval=TRUE-------------------------------
Q1 <- diamonds %>% group_by(cut) %>% 
  summarize(q1=quantile(carat,c(0.25)),q2=quantile(carat,c(0.5)),
        q3=quantile(carat,c(0.75)))
quantildf <- Q1%>% gather(key="alpha",value="quantiles",-cut)
ggplot(data=diamonds) + geom_density(aes(x=carat,y=..density..))+
  facet_grid(cut~.) +
  geom_vline(data=quantildf,aes(xintercept=quantiles),col=alpha("black",1/2))

## ----echo=FALSE,eval=TRUE---------------------------------
library(ggstance)
ggplot(data=diamonds) +
  geom_boxploth(data=diamonds,aes(y=-0.5,x=carat,fill=cut)) +
  geom_density(aes(x=carat,y=..density..)) +  facet_grid(cut~.) +
  geom_vline(data=quantildf,aes(xintercept=quantiles),col=alpha("black",1/2))+
  ylab("")

## ----echo=correct,eval=FALSE------------------------------
#  library(ggstance)
#  ggplot(data=diamonds) +
#    geom_boxploth(data=diamonds,aes(y=-0.5,x=carat,fill=cut)) +
#    geom_density(aes(x=carat,y=..density..)) +  facet_grid(cut~.) +
#    geom_vline(data=quantildf,aes(xintercept=quantiles),col=alpha("black",1/2))+
#    ylab("")

