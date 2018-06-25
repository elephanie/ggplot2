data(cars)
head(cars)

plot(jitter(cars$speed),jitter(cars$dist), 
     xlab = "Distance", ylab = "Speed", main = "Distance to stop", pch =4)
abline(a =60, b =0)
abline( v = 15)
abline(lm(cars$dist ~ cars$speed))
text(x =10, y =100, label = "Tested on June 2, 2018")
mtext(text = "Universal Motor", side = 3, adj = 0.3)
