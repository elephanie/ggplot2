library(ggplot2)
#library(lattice)
titanic<-read.csv("Learn/titanic.csv", header = TRUE)


titanic$Survived<-as.factor(titanic$Survived)                 #preparing the data for visulation
titanic$Pclass<-as.factor(titanic$Pclass)
titanic$Sex<-as.factor(titanic$Sex)



ggplot(titanic, aes(x = Age, fill = Survived))+
  theme_bw()+
  geom_histogram(binwidth = 5)+
  labs(x = "Age (binwidth = 5)", title = "Age Distribution")


ggplot(titanic, aes(x = Survived, y = Age))+
  theme_bw()+
  geom_boxplot()+
  labs(x = "Survived", y = "Age", title = "Titanic Survival Rate by Age")

  
ggplot(titanic, aes(x = Age, fill = Survived))+
  theme_bw()+
  geom_histogram(binwidth = 5)+
  facet_wrap(Sex  ~Pclass)
  labs(x = "", title = "Age Distribution")
  
  
##-------------
##   Histogram
#-----------------
  
  
head(ChickWeight)
  
  
hist(ChickWeight$weight)  
  
hist(ChickWeight$weight, xlab = "Weight (grams)", ylab = "Frequency")
  
hist(ChickWeight$weight, xlab = "Frequency", ylab = "Weight", main = "Distribution of Chicken Weight")
hist(ChickWeight$weight, xlab = "Weight (grams)", ylab = "Frequency", 
     main = "Distribution of Chicken Weight", labels = TRUE, 
     col = c("magenta","pink"), breaks = 18)


##-------------------------
# variables-one continous, one categorical
##-------------------------

#--boxplot
boxplot(chickwts$weight ~ chickwts$feed)

#--piechart
defects<-c(9, 15, 42, 29)
colour<-c("red","orange","yellow","green")
names(defects)<-c("Loose cap", "Labels", "Volume", "Scratch")
pie(x = defects,col = colour )

#--barchart
barplot(defects)

#--dotchart
dotchart(chickwts$weight, groups = chickwts$feed)

#--stripchart
stripchart(chickwts$weight ~ chickwts$feed)

#------------------------
# 2 categorical variables
#------------------------
#--mosaicplot
library(ggplot2)
str(mtcars)
str(mpg)
motable<-table(mpg$class,mpg$cyl)
mosaicplot(motable,color = colour, main = "number of cylnder x class")

#---barchart
mtab<-table(mtcars$vs, mtcars$cyl)

barplot(mtab, legend = rownames(mtcars$vs))

data()
plot(LakeHuron)
head(LakeHuron)
str(LakeHuron)
dim(LakeHuron)
class(LakeHuron)

#--- HOMEWORK Q1
data("InsectSprays")
head(InsectSprays)
par(mfrow = c(1, 2))
boxplot(InsectSprays$count, xlab = " All Sprays" ,  y = "Counts", main = "Effectiveness of Sprays")
boxplot(InsectSprays$count ~ InsectSprays$spray, xlab = "Count", ylab = "Type of Spray", 
        main = "Effectiveness by Spray")

#---HOMEWORK Q2
head(women)
dim(women)
plot(women, xlab = "weight", ylab = "hight", main = "American Women - Weight vs. Height" )


##--------------------------
#      Lattice
##--------------------------

#--- histogram 

histogram( ~ weight | feed, chickwts )


histogram( ~ mpg | factor(am), data = mtcars)

tdata<-c(2, 2, 3,3,5,6)
names(tdata)<-"count"
tdata<-as.data.frame(tdata)
equal.count(tdata$count, 2)

#--- histogram with factors

densityplot( ~ mpg|factor(cyl), data = mtcars, groups = factor(am),
             layout = c(1,3), as.table = T)
densityplot( ~ weight | factor(feed), data = chickwts, layout = c(2,3))

#--- box and whisker plot
bwplot(~ weight, data = chickwts)
bwplot( weight~ feed, data = chickwts)

#--- bar chart
barchart(rownames(mtcars)~ mpg | factor(gear) + factor(am), data = mtcars)

#--- scatter plot - two continue variables
xyplot(mpg ~ wt, data = mtcars)
xyplot(mpg ~ wt| factor(gear), data = mtcars, grid = T, type = c("p", "r"))       # "p" --point, "r"--straight regression line
xyplot(mpg ~ wt| factor(gear), data = mtcars, grid = T, type = c("p", "smooth"),  # lwd --line width, col.line --colour of line
       col.line = "darkorange", lwd =3, pch =2)                                   # pch -- plot character

#--- scatter plot with matrix
splom(chickwts)
splom(mtcars)
splom(mtcars[c(1:6)], type = c ("p", "smooth"))

#--- 3D scatter plot
cloud(mpg ~ wt*disp, data = mtcars, groups = factor(cyl), auto.key = T)

#--- dotplot and strip plot
dotplot(weight~feed, data = chickwts)
dotplot(~weight| feed, data = chickwts)
stripplot(weight ~ feed, data = chickwts)


#------------------------
#  HOMEWORK 2
#------------------------
#-Q1 
histogram(~bwt, data = birthwt, xlab = "Weight (grams)", main = "Birth Wight of All Children")

#-Q2

histogram(~ bwt | factor(race) , data = birthwt, xlab = "Weight (grams)", 
          main = "Birth Wight of All Children (1 = white, 2 =black, 3 = other)", layout = c(2,2))

#-Q3
histogram(~ bwt | factor(race) , data = birthwt, xlab = "Weight (grams)", 
          main = "Birth Wight of All Children (1 = white, 2 =black, 3 = other)", layout = c(1,3))

#-Q4
densityplot(~bwt, data = birthwt, xlab = "Weight (grams)", 
            main = "Birth Wight of All Children (1 = white, 2 =black, 3 = other)", 
            plot.points = F, lwd = 3, groups = race, auto.key = T)

#------------------------------
#   GGPLOT2
#-----------------------------
#-- single continuous variable
ggplot(data=mtcars, aes(x = mpg))+geom_histogram(binwidth = 5)+theme_bw()
p<-ggplot(data=mtcars, aes(x = mpg))
p+geom_density(alpha = 0.5, colour = "orange", fill = "green", linetype="dotted", size =2) 
p+geom_area(stat="bin", binwidth = 6)
p+geom_freqpoly(stat="bin", bins = 6, alpha = 0.8, colour = "orange", fill = "green")

#-------
# Diamond Project
#-------
str(diamonds)

#-- histogram          
p<-ggplot( data = diamonds, aes(price)) 
p+geom_histogram()
cp<-ggplot( data = diamonds, aes(carat, price))
#-- scatter plot: two continous variable
cp+geom_point() + geom_smooth(se = F)                                       # se - error

ggplot(data = diamonds, aes(x = cut(carat, breaks =10), y=price)) + geom_boxplot()
#--- zoom in for <.3 carat
d03<-diamonds[diamonds$carat<0.3,]

ggplot(data =d03, aes(price))+geom_histogram()

#--- zoom in for >.29 carat <.31
d23<-diamonds[diamonds$carat<=0.31&diamonds$carat>=0.29, ]

dp<-ggplot(data = diamonds, aes(x = carat, y = price, colour = cut))
dp + geom_smooth(se = F)
d23p<-ggplot(data = d23, aes(x = cut, y = price, colour = cut))
d23p + geom_boxplot() + coord_cartesian(ylim = c(0,1500))

#--- zoom in for >.29 carat <.31 and Ideal cut only
d23i<-d23[d23$cut=="Ideal",]

#--- looking at colors of diamonds
ggplot( data = diamonds, aes(x=color, y= price, colour=color))+geom_boxplot()
ggplot( data = d23i, aes(x=color, y= price, colour=color))+geom_boxplot()

d23id<-d23i[d23i$color=="D",]

#--- looking at clarity and price
cp<-ggplot( data = d23id,aes(x = clarity, y = price, colour = clarity) )
cp + geom_boxplot()
d23idc <- d23id[d23id$clarity == "VS1",]

final<-ggplot(data = d23idc, aes (x = price)) + geom_histogram()
final

f<-d23idc[d23idc$price<=500,]

#--------------------------
#   GGPLOT2 AES
#--------------------------
#-- aes 1 using scatter plot
ggplot(data =diamonds, aes(x = carat, y = price))+ geom_point(shape =23, colour = "green", fill = "yellow") + geom_smooth(colour ="red")

#-- aes 2 using boxplot

ggplot( data = d23, aes(x = cut, y = price, fill = clarity)) + geom_boxplot()

#-- aes using bar plot
ggplot( data = d23, aes(x = clarity, fill = cut)) + geom_bar(position = "fill")
ggplot( data = d23, aes(x = clarity, fill = cut)) + geom_bar(position = "dodge")
ggplot( data = d23, aes(x = clarity, fill = cut)) + geom_bar(position = "stack")

# geom layer-- histogram

ggplot( data = diamonds, aes( x = price, fill = cut)) + geom_histogram(binwidth = 2000)

lh<-data.frame( year = 1875:1972, level = as.vector(LakeHuron))

ggplot( data =lh, aes( x =year, y = level))+geom_line(size =2, colour = "orange")

# facets layer
p<-ggplot(data=diamonds, aes( x = carat, y = price, colour = clarity)) + geom_point()
p+facet_grid(cut ~ clarity)
p+facet_wrap(~cut)
p + facet_wrap(~ color)

# statistic layer
p+facet_wrap(~cut)+geom_smooth( method = "loess",se = F)

# coordinate system
p<-ggplot(data =diamonds, aes(x = carat, y = price)) + geom_point(colour = "green", alpha = 0.4); p
r<-p+geom_smooth(colour = "orange")+coord_cartesian(xlim = c(3,5), ylim = c(15000,20000))

# ratio and flip
m<-ggplot(data = mtcars, aes(x = hp, y = mpg)) + geom_point() 
m + coord_fixed(6) + geom_smooth(colour = "orange") + coord_flip()

# pie chart --indirectly with ggplot2

ggplot(data = chickwts, aes(x =factor(1), fill = feed)) + geom_bar( width = 1) +
coord_polar(theta = "y")

ggplot(data = diamonds, aes(x = factor(1), fill = cut)) + geom_bar (width = 1) +
coord_polar(theta = "y")


# try adding this new line

