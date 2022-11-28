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

## ----teacher=correct--------------------------------------
x <- seq(0,2*pi,length=1000)
plot(x,sin(x),type="l")

## ----echo=correct,eval=FALSE------------------------------
#  title("Plot of the sine function")

## ----echo=FALSE,eval=correct------------------------------
plot(x,sin(x),type="l")
title("Plot of the sine function")

## ----teacher=correct--------------------------------------
x <- seq(-4,4,by=0.01)
plot(x,dnorm(x),type="l")

## ----echo=correct,eval=FALSE------------------------------
#  abline(v=0,lty=2)

## ----echo=FALSE,eval=correct------------------------------
plot(x,dnorm(x),type="l")
abline(v=0,lty=2)

## ----echo=correct,eval=FALSE------------------------------
#  lines(x,dt(x,5),col=2)
#  lines(x,dt(x,30),col=3)

## ----echo=FALSE,eval=correct------------------------------
plot(x,dnorm(x),type="l")
abline(v=0,lty=2)
lines(x,dt(x,5),col=2)
lines(x,dt(x,30),col=3)

## ----echo=correct,eval=FALSE------------------------------
#  legend("topleft",legend=c("Normal","Student(5)","Student(30)"),
#     col=1:3,lty=1)

## ----echo=FALSE,eval=correct------------------------------
plot(x,dnorm(x),type="l")
abline(v=0,lty=2)
lines(x,dt(x,5),col=2)
lines(x,dt(x,30),col=3)
legend("topleft",legend=c("Normal","Student(5)","Student(30)"),
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

## ----teacher=correct--------------------------------------
ggplot(diamonds2)+aes(x=carat)+geom_histogram(bins=10)

## ----teacher=correct--------------------------------------
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
ggplot(diamonds)+aes(x=cut,fill=cut)+geom_bar(show.legend = FALSE)+
  scale_fill_manual(values=c("Fair"="blue","Good"="red",
                             "Very Good"="green","Premium"="yellow",
                             "Ideal"="black"))


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
ggplot(X)+aes(x=X1,y=prob,fill=X1)+geom_bar(stat="identity",show.legend = FALSE)+
  labs(fill="Color")+xlab("")
#or
ggplot(X)+aes(x=X1,weight=prob)+geom_bar()

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
ggplot(donnees1)+aes(x=X)+geom_line(aes(y=Y1))+
  geom_line(data=donnees2,aes(y=Y2),color="red")

## ----teacher=correct--------------------------------------
donnees <- data.frame(X,Y1,Y2)
ggplot(donnees)+aes(x=X,y=Y1)+geom_line()+
  geom_line(aes(y=Y2),color="red")
#or for the legend
ggplot(donnees)+aes(x=X,y=Y1)+geom_line(aes(color="cos"))+
  geom_line(aes(y=Y2,color="sin"))+labs(color="Fonction")

## ----teacher=correct--------------------------------------
df <- data.frame(X,cos=Y1,sin=Y2)
df1 <- df |> pivot_longer(cols=c(cos,sin),
                       names_to = "Fonction",
                       values_to = "value")
#or
df1 <- df |> pivot_longer(cols=-X,
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
#  ggplot(data=diamonds) + geom_boxplot(aes(x=carat,y=cut,fill=cut))
#  ggplot(data=diamonds) + geom_density(aes(x=carat,y=..density..)) +
#    facet_grid(cut~.)

## ----echo=correct,eval=TRUE-------------------------------
Q1 <- diamonds |> group_by(cut) |> 
  summarize(q1=quantile(carat,c(0.25)),q2=quantile(carat,c(0.5)),
        q3=quantile(carat,c(0.75)))
quantildf <- Q1 |> pivot_longer(-cut,names_to="alpha",values_to="quantiles")
ggplot(data=diamonds) + geom_density(aes(x=carat,y=..density..)) +
  facet_grid(cut~.) +
  geom_vline(data=quantildf,aes(xintercept=quantiles),col=alpha("black",1/2))

## ----teacher=correct--------------------------------------
ggplot(data=diamonds) + aes(x=carat)+ 
  geom_density() +
  stat_boxplot(aes(xintercept=c(..xlower..,..xmiddle..,
                                ..xupper..)),geom="vline") + 
  facet_grid(cut~.) 

## ----teacher=correct--------------------------------------
diamonds |> ggplot(aes(x=carat)) +
  geom_density() +
  stat_summary(mapping=aes(y=1,xintercept=after_stat(x)),fun="quantile",
               fun.args = list(prob=c(0.25,0.5,0.75)),
               geom="vline",orientation="y") + 
  facet_grid(cut~.) 

## ----echo=FALSE,eval=TRUE---------------------------------
ggplot(data=diamonds) +
  geom_boxplot(data=diamonds,aes(y=-0.5,x=carat,fill=cut)) +
  geom_density(aes(x=carat,y=..density..)) +  facet_grid(cut~.) +
  geom_vline(data=quantildf,aes(xintercept=quantiles),col=alpha("black",1/2)) +
  ylab("")

## ----echo=correct,eval=FALSE------------------------------
#  ggplot(data=diamonds) +
#    geom_boxplot(data=diamonds,aes(y=-0.5,x=carat,fill=cut)) +
#    geom_density(aes(x=carat,y=..density..)) +  facet_grid(cut~.) +
#    geom_vline(data=quantildf,aes(xintercept=quantiles),col=alpha("black",1/2)) +
#    ylab("")

## ----teacher=correct--------------------------------------
ggplot(data=diamonds) + aes(x=carat)+
  geom_boxplot(data=diamonds,aes(y=-0.5,x=carat,fill=cut)) +
  geom_density() +
  stat_boxplot(aes(xintercept=c(..xlower..,..xmiddle..,
                            ..xupper..)),geom="vline") + 
  facet_grid(cut~.)+ylab("")

## ----teacher=correct--------------------------------------
diamonds |> ggplot(aes(x=carat)) +
  geom_boxplot(data=diamonds,aes(y=-0.5,x=carat,fill=cut)) +
  geom_density() +
  stat_summary(mapping=aes(y=1,xintercept=after_stat(x)),fun="quantile",
               fun.args = list(prob=c(0.25,0.5,0.75)),
               geom="vline",orientation="y") + 
  facet_grid(cut~.) + ylab("")

## ----echo=FALSE,eval=FALSE--------------------------------
#  library(ggstance)
#  ggplot(data=diamonds) +
#    geom_boxploth(data=diamonds,aes(y=-0.5,x=carat,fill=cut)) +
#    geom_density(aes(x=carat,y=..density..)) +  facet_grid(cut~.) +
#    geom_vline(data=quantildf,aes(xintercept=quantiles),col=alpha("black",1/2)) +
#    ylab("")

## ----echo=FALSE-------------------------------------------
cache_carto <- FALSE
#correct <- FALSE
#cor <- FALSE

## ----message=FALSE, warning=FALSE,cache=cache_carto-------
library(ggmap)
us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
ggmap(map)

## ----message=FALSE, warning=FALSE,cache=cache_carto-------
europe <- c(left = -12, bottom = 35, right = 30, top = 63)
get_stamenmap(europe, zoom = 5,"toner-lite") |> ggmap()

## ----message=FALSE, warning=FALSE,cache=cache_carto-------
get_stamenmap(europe, zoom = 5,"toner-background") |> ggmap()

## ----message=FALSE, warning=FALSE,cache=cache_carto-------
fr <- c(left = -6, bottom = 41, right = 10, top = 52)
get_stamenmap(fr, zoom = 5,"toner-lite") |> ggmap()

## ----message=FALSE, warning=FALSE-------------------------
if (!(require(jsonlite))) install.packages("jsonlite")
mygeocode <- function(adresses){
# adresses est un vecteur contenant toutes les adresses sous forme de chaine de caracteres
  nominatim_osm <- function(address = NULL){
    ## details: http://wiki.openstreetmap.org/wiki/Nominatim
    ## fonction nominatim_osm proposée par D.Kisler
    if(suppressWarnings(is.null(address)))  return(data.frame())
    tryCatch(
      d <- jsonlite::fromJSON(
        gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
             'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
      ), error = function(c) return(data.frame())
    )
    if(length(d) == 0) return(data.frame())
    return(c(as.numeric(d$lon), as.numeric(d$lat)))
  }
  tableau <- t(sapply(adresses,nominatim_osm))
  colnames(tableau) <- c("lon","lat")
  return(tableau)
}


## ---- message=FALSE, warning=FALSE,cahe=cache_carto-------
mygeocode("the white house")
mygeocode("Paris")
mygeocode("Rennes")

## ----teacher=correct,cache=cache_carto--------------------
V <- c("Paris","Lyon","Marseille")
A <- mygeocode(V)
A <- A |> as_tibble() |> mutate(Villes=V)
fr <- c(left = -6, bottom = 41, right = 10, top = 52)
fond <- get_stamenmap(fr, zoom = 5,"toner-lite") 
ggmap(fond)+geom_point(data=A,aes(x=lon,y=lat),color="red")

## ----echo=FALSE,eval=FALSE--------------------------------
#  #pour aller plus vite
#  df <- read_csv("data/villes_fr.csv")
#  df$Commune <- as.character(df$Commune)
#  df$Commune[10] <- "Lille"
#  coord <- mygeocode(as.character(df$Commune)) |> as_tibble()
#  write_csv(coord,path="coord_exo1_ggmap.csv")

## ----teacher=correct--------------------------------------
df <- read_csv("data/villes_fr.csv")
df$Commune <- as.character(df$Commune)

## ----teacher=correct--------------------------------------
df$Commune[10]    
df$Commune[10] <- "Lille"

## ----echo=correct,eval=FALSE------------------------------
#  coord <- mygeocode(as.character(df$Commune)) |> as_tibble()
#  df1 <- bind_cols(df,coord)
#  ggmap(fond)+geom_point(data=df1,aes(x=lon,y=lat),color="red")
#  ggmap(fond)+geom_point(data=df1,aes(x=lon,y=lat,size=`2014`),color="red")

## ----echo=FALSE,eval=correct------------------------------
coord <- read_csv("data_comp_tuto/coord_exo1_ggmap.csv")
df1 <- bind_cols(df,coord)
ggmap(fond)+geom_point(data=df1,aes(x=lon,y=lat),color="red")
ggmap(fond)+geom_point(data=df1,aes(x=lon,y=lat,size=`2014`),color="red")

## ---------------------------------------------------------
library(sf)
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
class(nc)
nc

## ----cache=cache_carto------------------------------------
plot(st_geometry(nc))

## ---------------------------------------------------------
ggplot(nc)+geom_sf()

## ---------------------------------------------------------
ggplot(nc[1:3,]) + geom_sf(aes(fill = AREA)) + geom_sf_label(aes(label = NAME))

## ----echo=FALSE,eval=FALSE--------------------------------
#  #Pour aller plus vite
#  coord.ville.nc <- mygeocode(paste(as.character(nc$NAME),"NC")) |> as.data.frame()
#  write_csv(coord.ville.nc,path="data_comp_tuto/coord_ville_nc.csv")

## ----echo=FALSE-------------------------------------------
coord.ville.nc <- read_csv("data_comp_tuto/coord_ville_nc.csv")
#coord.ville.nc <- as.data.frame(coord.ville.nc)
names(coord.ville.nc) <- c("lon","lat")

## ----eval=FALSE,echo=TRUE---------------------------------
#  coord.ville.nc <- mygeocode(paste(as.character(nc$NAME),"NC"))
#  coord.ville.nc <- as.data.frame(coord.ville.nc)
#  names(coord.ville.nc) <- c("lon","lat")

## ---------------------------------------------------------
coord.ville1.nc <- coord.ville.nc |>  
  filter(lon<=-77 & lon>=-85 & lat>=33 & lat<=37) |> 
  as.matrix() |> st_multipoint()  |> st_geometry() |> st_cast(to="POINT")
coord.ville1.nc

## ---------------------------------------------------------
st_crs(coord.ville1.nc) <- 4326 

## ---------------------------------------------------------
ggplot(nc)+geom_sf()+geom_sf(data=coord.ville1.nc)

## ---------------------------------------------------------
nc2 <- nc |> mutate(centre=st_centroid(nc)$geometry)
ggplot(nc2)+geom_sf()+geom_sf(aes(geometry=centre))

## ---------------------------------------------------------
dpt <- read_sf("data/dpt")
ggplot(dpt) + geom_sf()

## ----teacher=correct--------------------------------------
coord.ville1 <- tibble(df1[,14:15]) |> 
  as.matrix() |> st_multipoint() |> st_geometry()
coord.ville2 <- st_cast(coord.ville1, to = "POINT")
coord.ville1
coord.ville2

## ----teacher=correct--------------------------------------
st_geometry(df1) <- coord.ville2
st_crs(df1) <- 4326
ggplot(dpt)+geom_sf(fill="white")+
  geom_sf(data=df1,aes(size=`2014`),color="red")+theme_void()

## ----teacher=correct--------------------------------------
chomage <- read_delim("data/tauxchomage.csv",delim=";")

## ----teacher=correct--------------------------------------
dpt <- read_sf("data/dpt")
dpt2 <- inner_join(dpt,chomage,by="CODE_DEPT")

## ----teacher=correct--------------------------------------
dpt3 <- dpt2 |> select(A2006=TCHOMB1T06,A2011=TCHOMB1T11,geometry) |> as_tibble() |>
  pivot_longer(-geometry,names_to="Annee",values_to="TxChomage") |> st_as_sf()

## ----echo=FALSE,eval=FALSE--------------------------------
#  dpt3 <- dpt2 |> select(A2006=TCHOMB1T06,A2011=TCHOMB1T11,geometry) |>
#    gather(key="Annee",value="TxChomage",-geometry)
#  #  pivot_longer(-geometry,names_to="Annee",values_to="TxChomage")

## ----teacher=correct--------------------------------------
ggplot(dpt3) + aes(fill = TxChomage)+geom_sf() +
  facet_wrap(~Annee, nrow = 1)+
  scale_fill_gradient(low="white",high="brown")+theme_bw()

## ----echo=FALSE-------------------------------------------
correct <- FALSE
cor <- correct

## ----echo=FALSE,eval=FALSE--------------------------------
#  #Pour éviter les changements
#  donnees <- read_delim("https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/synop.2022112415.csv",delim=";",col_types = cols(t=col_double()))
#  station <- read_delim("https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.csv",delim=";")
#  write_csv(donnees,path="donnees_temp_fr.csv")
#  write_csv(station,path="station_temp_fr.csv")

## ----echo=FALSE,eval=TRUE---------------------------------
donnees <- read_csv("data/donnees_temp_fr.csv")
station <- read_csv("data/station_temp_fr.csv")
donnees$t <- donnees$t-273.15 #Celcius
temp <- donnees |> select(numer_sta,t)
names(temp)[1] <- c("ID")
D <- inner_join(temp, station, by = c("ID"))

## ---- echo=correct,eval=FALSE-----------------------------
#  donnees <- read_delim("https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/synop.2020052415.csv",delim=";",col_types = cols(t=col_double()))
#  station <- read_delim("https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.csv",delim=";")
#  donnees$t <- donnees$t-273.15 #on passe en degrés celcius
#  temp <- donnees |> select(numer_sta,t)
#  names(temp)[1] <- c("ID")
#  D <- inner_join(temp, station, by = c("ID"))

## ----echo=TRUE,eval=correct-------------------------------
#  station2 <- station1 |> select(Longitude,Latitude) |>
#    as.matrix() |> st_multipoint() |> st_geometry()
#  st_crs(station2) <- 4326
#  station2 <- st_cast(station2, to = "POINT")

## ----echo=TRUE,eval=correct-------------------------------
#  centro <- st_centroid(dpt$geometry)
#  centro <- st_transform(centro,crs=4326)

## ----echo=TRUE,eval=correct-------------------------------
#  DD <- st_distance(df,centro)

## ---------------------------------------------------------
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world) +
geom_sf(aes(fill = pop_est)) +
scale_fill_viridis_c(option = "plasma", trans = "sqrt")+theme_void()

## ---------------------------------------------------------
ggplot(data = world) +
geom_sf() +
coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

## ---------------------------------------------------------
regions <- read_sf("data/regions-20180101-shp/")

## ---------------------------------------------------------
format(object.size(regions),units="Mb")

## ---------------------------------------------------------
library(rmapshaper)
regions1 <- ms_simplify(regions)
format(object.size(regions1),units="Mb")
ggplot(regions1)+geom_sf()+
  coord_sf(xlim = c(-5.5,10),ylim=c(41,51))+theme_void()

## ----message=FALSE, warning=FALSE-------------------------
library(leaflet)
leaflet() |> addTiles()

## ----echo=FALSE,eval=FALSE--------------------------------
#  Paris <- c(2.351462,48.8567)

## ---------------------------------------------------------
Paris <- mygeocode("paris")
m2 <- leaflet() |> setView(lng = Paris[1], lat = Paris[2], zoom = 12) |> 
  addTiles()
m2 |> addProviderTiles("Stamen.Toner")
m2 |> addProviderTiles("Wikimedia")
m2 |> addProviderTiles("Esri.NatGeoWorldMap")
m2 |>
  addProviderTiles("Stamen.Watercolor") |>
  addProviderTiles("Stamen.TonerHybrid")


## ---------------------------------------------------------
data(quakes)
leaflet(data = quakes[1:20,]) |> addTiles() |>
  addMarkers(~long, ~lat, popup = ~as.character(mag))

## ---------------------------------------------------------
content <- paste(sep = "<br/>",
  "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
  "606 5th Ave. S",
  "Seattle, WA 98138"
)

leaflet() |> addTiles() |>
  addPopups(-122.327298, 47.597131, content,
    options = popupOptions(closeButton = FALSE)
  )

## ----echo=FALSE,eval=FALSE--------------------------------
#  #Pour éviter les problèmes de changement
#  sta.Paris <- read_delim("https://opendata.paris.fr/explore/dataset/velib-disponibilite-en-temps-reel/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true",delim=";")
#  write_csv(sta.Paris,path="sta.Paris.csv")

## ----echo=FALSE,eval=TRUE---------------------------------
sta.Paris <- read_csv("data/sta.Paris.csv")

## ---- echo=correct,eval=FALSE-----------------------------
#  lien <- "https://opendata.paris.fr/explore/dataset/velib-disponibilite-en-temps-reel/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true"
#  sta.Paris <- read_delim(lien,delim=";")

## ---- echo=correct,eval=TRUE------------------------------
sta.Paris1 <- sta.Paris |> 
  separate(`Coordonnées géographiques`,into=c("lat","lon"),sep=",") |> 
  mutate(lat=as.numeric(lat),lon=as.numeric(lon))

## ---------------------------------------------------------
ColorPal1 <- colorNumeric(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7",
                                               space = "Lab"), domain = c(0,1))
ColorPal2 <- colorNumeric(scales::seq_gradient_pal(low = "red", high = "black", 
                                               space = "Lab"), domain = c(0,1))

## ----echo=correct,eval=TRUE-------------------------------
nom.station <- "Jussieu - Fossés Saint-Bernard"
local.station <- function(nom.station){
  df <- sta.Paris1 |> filter(`Nom station`==nom.station)
  leaflet(data = sta.Paris1) |> setView(lng=df$lon,lat=df$lat,zoom=15) |> 
addTiles() |> 
addCircleMarkers(~ lon, ~ lat,stroke = FALSE, fillOpacity = 0.7,
                popup = ~ paste(as.character(`Nom station`),", Vélos dispos :",
                                as.character(`Nombre total vélos disponibles`),
                                sep="")) |>
addMarkers(lng=df$lon,lat=df$lat,
           popup = ~ paste(nom.station,", Vélos dispos :",
                           as.character(df$`Nombre total vélos disponibles`),
                           sep=""),
           popupOptions = popupOptions(noHide = T))
}

## ---------------------------------------------------------
local.station("Jussieu - Fossés Saint-Bernard")
local.station("Gare Montparnasse - Arrivée")

## ---------------------------------------------------------
library(rAmCharts)
amHist(iris$Petal.Length)

## ---------------------------------------------------------
amPlot(iris, col = colnames(iris)[1:2], type = c("l", "st"), 
       zoom = TRUE, legend = TRUE)

## ---------------------------------------------------------
amBoxplot(iris)

## ---------------------------------------------------------
library(plotly)
n <- 100
X <- runif(n,-5,5)
Y <- 2+3*X+rnorm(n,0,1)
D <- data.frame(X,Y)
model <- lm(Y~X,data=D)

## ---------------------------------------------------------
D %>% plot_ly(x=~X,y=~Y) %>%
  add_markers(type="scatter",mode="markers",
              marker=list(color="red"),name="Nuage") %>%
  add_trace(y=fitted(model),type="scatter",mode='lines',
            name="Régression",line=list(color="blue")) %>% 
  layout(title="Régression",xaxis=list(title="abscisse"),
         yaxis=list(title="ordonnées"))

## ----name="plotly_html",eval=!comp_pdf,echo=!comp_pdf-----
plot_ly(z = volcano, type = "surface")
plot_ly(z = volcano, type = "contour")

## ----eval=FALSE,echo=FALSE--------------------------------
#  p <- plot_ly(z = volcano, type = "surface")
#  orca(p, "./FIGURES/surface-plot.pdf")

## ----name="plotly_pdf",echo=comp_pdf,eval=FALSE-----------
#  plot_ly(z = volcano, type = "surface")

## ----name="plotly_pdf1",eval=comp_pdf,echo=comp_pdf-------
#  plot_ly(z = volcano, type = "contour")

## ---------------------------------------------------------
p <- ggplot(iris)+aes(x=Species,y=Sepal.Length)+geom_boxplot()+theme_classic()
ggplotly(p)

## ---------------------------------------------------------
nodes <- data.frame(id = 1:15, label = paste("Id", 1:15),
                    group=sample(LETTERS[1:3], 15, replace = TRUE))
edges <- data.frame(from = trunc(runif(15)*(15-1))+1,to = trunc(runif(15)*(15-1))+1)
library(visNetwork)
visNetwork(nodes,edges)

## ---------------------------------------------------------
visNetwork(nodes, edges) %>% visOptions(highlightNearest = TRUE)

## ---------------------------------------------------------
visNetwork(nodes, edges) %>% visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

## ---------------------------------------------------------
visNetwork(nodes, edges) %>% visOptions(selectedBy = "group")

## ---------------------------------------------------------
nodes <- read.csv("data/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("data/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
head(nodes)
head(links)

## ---------------------------------------------------------
library(igraph)
media <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
V(media)$name <- nodes$media

## ---------------------------------------------------------
plot(media)

## ----echo=FALSE,eval=FALSE,indent='    '------------------
#  df <- read.table("data/ozone.txt")
#  gg.nuage <- ggplot(df)+aes(x=T12,y=maxO3)+geom_point()+geom_smooth()
#  modT12 <- lm(maxO3~T12,data=df)
#  p12 <- predict(modT12) %>% as.numeric()
#  am.nuage <- amPlot(x=df$T12,y=df$maxO3) %>% amLines(y=p12,type="line")
#  pl.nuage <- ggplotly(gg.nuage)

## ----echo=cor,eval=cor,indent='        '------------------
#  df <- read.table("data/ozone.txt")
#  cc <- cor(df[,1:11])
#  mat.cor <- corrplot::corrplot(cc)

## ----echo=cor,eval=cor,indent='        '------------------
#  gg.H <- ggplot(df)+aes(x=maxO3)+geom_histogram(bins = 10)
#  am.H <- amHist(df$maxO3)
#  pl.H <- ggplotly(gg.H)

## ----echo=cor,eval=cor,indent='        '------------------
#  mod <- lm(maxO3~.,data=df)
#  res <- rstudent(mod)
#  df1 <- data.frame(maxO3=df$maxO3,r.student=res)
#  Ggg <- ggplot(df1)+aes(x=maxO3,y=res)+geom_point()+geom_smooth()
#  Gggp <- ggplotly(Ggg)

## ----eval=FALSE,indent='        '-------------------------
#  radioButtons("variable1",
#                     label="Choisir la variable explicative",
#                     choices=names(df)[-1],
#                     selected=list("T9"))

## ----eval=FALSE,indent='        '-------------------------
#  mod1 <- reactive({
#    XX <- paste(input$variable1,collapse="+")
#    form <- paste("maxO3~",XX,sep="") %>% formula()
#    lm(form,data=df)
#    })
#  #Df corresponds to the dataset
#  renderDataTable({
#    mod.sum1 <- summary(mod1())$coefficients %>% round(3) %>% as.data.frame()
#    DT::datatable(mod.sum1,options = list(dom = 't'))
#  })

## ----eval=FALSE,indent='        '-------------------------
#  renderPlotly({
#    (ggplot(df)+aes(x=!!as.name(input$variable1),y=maxO3)+
#       geom_point()+geom_smooth(method="lm")) %>% ggplotly()
#  })

## ----eval=FALSE,indent = '        '-----------------------
#  checkboxGroupInput("variable",
#                     label="Choisir la variable",
#                     choices=names(df)[-1],
#                     selected=list("T9"))

## ----name='app_dash_html',screenshot.opts=list(delay = 5, cliprect = 'viewport',zoom=2,vwidth=200,vheight=200),echo=FALSE,eval=!comp_pdf,out.width=760,out.height=750----
knitr::include_app('https://lrouviere.shinyapps.io/dashboard/', height = '650px')

## ----name='app_dash_pdf',echo=FALSE,eval=comp_pdf---------
#  webshot::webshot("https://lrouviere.shinyapps.io/dashboard/", file="dashboard.png",delay=20,zoom=1)

## ---- echo = TRUE, eval = FALSE---------------------------
#  selectInput(inputId = "color", label = "Couleur :",
#              choices = c("Rouge" = "red", "Vert" = "green", "Bleu" = "blue"))

## ---- echo = TRUE, eval = FALSE---------------------------
#  # ui.R
#  verbatimTextOutput("...")
#  
#  # server.R
#  output$... <- renderPrint({
#    summary(...)
#  })

## ---- echo = TRUE, eval = FALSE---------------------------
#  # rappel de la structure (ui.R)
#  navlistPanel(
#  "Title of the structure",
#  tabPanel("Title of the tab", ... "(content of the tab)"),
#  tabPanel("Title of the tab", ... "(content of the tab)")
#  )

## ---- echo = TRUE, eval = FALSE---------------------------
#  fluidRow(
#    column(width = 3, ...), # column 1/4 (3/12)
#    column(width = 9, ...)  # column 3/4 (9/12)
#  )

## ---- echo = TRUE, eval = FALSE---------------------------
#  # rappel de la structure (ui.R)
#  tabsetPanel(
#  tabPanel("Title of the tab", ... "(content of the tab)"),
#  tabPanel("Title of the tab", ... "(content of the tab)")
#  )

## ---- echo = TRUE, eval = FALSE---------------------------
#  # server.R
#  output$distPlot <- renderAmCharts({...})
#  
#  # ui.R
#  amChartsOutput("...")

## ---- echo = TRUE, eval = FALSE---------------------------
#  # think to add  "session"
#  shinyServer(function(input, output, session)
#  
#  # an id
#  tabsetPanel(id = "viz",
#    tabPanel("Histogram", ...
#  
#  # and finaly
#  observeEvent(input$go, {
#  updateTabsetPanel(session, inputId = "viz", selected = "Histogram")
#  })

## ---- echo = TRUE, eval = FALSE---------------------------
#  # Example of reactive
#  data <- reactive({
#    ...
#  })
#  
#  output$plot <- renderPlot({
#    x <- data()
#    ...
#  })

## ----echo=TRUE,eval=FALSE---------------------------------
#  h1("Dataset", style = "color : #0099ff;text-align:center")

## ----echo=correct,eval=correct----------------------------
#  library(bestglm)
#  amHist(SAheart$adiposity,freq=FALSE,xlab="adiposity")
#  amBoxplot(adiposity~chd,data=SAheart)

## ---- eval=FALSE, message=FALSE, warning=FALSE, include=TRUE----
#  choices=names(SAheart)[sapply(SAheart,class)=="numeric"]

## ----name='desc-app_html',screenshot.opts=list(delay = 5, cliprect = 'viewport',zoom=2,vwidth=200,vheight=200),echo=FALSE,eval=!comp_pdf,out.width=760,out.height=750----
knitr::include_app('https://lrouviere.shinyapps.io/DESC_APP/', height = '650px')

## ----name='desc-app_pdf',echo=FALSE,eval=comp_pdf---------
#  webshot::webshot("https://lrouviere.shinyapps.io/DESC_APP/", file="desc_app.png",delay=5,zoom=1)

## ----name='velib-app_html',screenshot.opts=list(delay = 5, cliprect = 'viewport',zoom=2,vwidth=200,vheight=200),echo=FALSE,eval=!comp_pdf,out.width=760,out.height=750----
knitr::include_app('https://lrouviere.shinyapps.io/velib/', height = '650px')

## ----name='velib-app_pdf',echo=FALSE,eval=comp_pdf--------
#  webshot::webshot("https://lrouviere.shinyapps.io/velib/", file="velib_app.png",delay=5,zoom=1)

