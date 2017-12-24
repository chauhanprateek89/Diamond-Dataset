install.packages("ggplot2", dependencies = T, repos = "http://cran.us.r-project.org")
install.packages("dplyr", dependencies = T, repos = "http://cran.us.r-project.org")
install.packages("gridExtra", dependencies = T, repos = "http://cran.us.r-project.org")
install.packages("gplots", dependencies = T, repos = "http://cran.us.r-project.org")
install.packages("cluster", dependencies = T, repos = "http://cran.us.r-project.org")
install.packages("fpc", dependencies = T, repos = "http://cran.us.r-project.org")
library(fpc)
library(cluster)
library(gplots)
library(gridExtra)
library(ggplot2)
library(dplyr)


data("diamonds")
str(diamonds)




#Z-test

diamonds.price <- diamonds$price
diamonds.price.length <- length(diamonds.price)
ggplot(aes(x = diamonds.price), data = diamonds) +
  geom_histogram(binwidth = 500, color = "black", fill = "white") + 
  labs(title = "Histogram: Price of diamonds",x = "Price")

#1. Z-test for one variable

#unbiassed
price_sample <- sample(diamonds.price, size = 300)
sample_mean <- mean(price_sample)

#biased - higher price

cut <- 1:53940
weights <- cut^.6
sorted_price <- sort(diamonds.price)
price_sample_biased <- sample(sorted_price, size = 300, prob = weights)
sample_mean_biased <- mean(price_sample_biased)

#population parameter calculations
pop_sd <- sd(diamonds.price)*sqrt((length(diamonds.price)-1)/(length(diamonds.price)))
pop_mean <- mean(diamonds.price)

#unbiased
#z-stat caclulation
sample_mean
z<- (sample_mean - pop_mean)/(pop_sd/sqrt(300))
z
#calculating the p-value
p_yellow1 <- pnorm(z)
p_green1 <- 1 - p_yellow1
p_green1


#costly-biased

#z-stat
sample_mean_biased
z<-(sample_mean_biased - pop_mean)/(pop_sd/sqrt(300))
z

#calculating the p-value
p_yellow2 <- pnorm(z)
p_green2 <- 1 - p_yellow2
p_green2

sample_out <- cbind(price_sample,price_sample_biased)
write.csv(sample_out, file='sample_out.csv')

#T-test

ggplot(aes(y = price, x = color, color = diamonds$color), data = diamonds) + 
  geom_boxplot()+  
  coord_cartesian(ylim = c(0,8000)) + 
  stat_summary(fun.y = mean, geom = 'point', shape = 4) +
  scale_y_continuous( breaks = seq(0, 7000, 500))

#performing the t-test
diamonds.cut.F <- subset(diamonds,diamonds$color == "F")
diamonds.cut.G <- subset(diamonds,diamonds$color == "G")
#Welsh t-test
t.test(diamonds.cut.F$price,diamonds.cut.G$price)
#classical t-test
t.test(diamonds.cut.F$price,diamonds.cut.G$price, var.equal = TRUE)

#paired t-test

ggplot(aes(y = price, x = clarity, color = diamonds$clarity), data = diamonds) + 
  geom_boxplot()+  
  coord_cartesian(ylim = c(0,7000)) + 
  stat_summary(fun.y = mean, geom = 'point', shape = 4) +
  scale_y_continuous( breaks = seq(0, 7000, 500))

diamonds.clarity.VS2 <- subset(diamonds,diamonds$clarity=="VS2")
diamonds.clarity.VS2_price <- sample(diamonds.clarity.VS2$price, size = 8000)
diamonds.clarity.VS1 <- subset(diamonds,diamonds$clarity=="VS1")
diamonds.clarity.VS1_price <- sample(diamonds.clarity.VS1$price, size = 8000)
t.test(diamonds.clarity.VS1_price,diamonds.clarity.VS2_price,
       paired = TRUE)

#chi-squared test

p1 <- qplot(x = price, 
            data = diamonds, binwidth = 100, 
            geom = 'freqpoly', 
            color = clarity, xlim = c(0,7000)) 

p2 <- qplot(x = price, 
            data = diamonds, binwidth = 100, 
            geom = 'freqpoly', 
            color = cut, xlim = c(0,7000))

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

chisq.test(diamonds$clarity,diamonds$cut, correct = FALSE)

#ANOVA

ggplot(aes(y = price, x = cut, color = diamonds$cut), data = diamonds) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(0,7000)) + 
  stat_summary(fun.y = mean, geom = 'point', shape = 4) +
  scale_y_continuous( breaks = seq(0, 7000, 500))

aov.test <- aov(diamonds$price~diamonds$cut)
summary(aov.test)

plotmeans(diamonds$price~diamonds$cut)
TukeyHSD(aov.test)

plot(aov.test) #we get 4 plots from here, i Have included residualvsfitting plot in report
plot(TukeyHSD(aov.test)) #Plot from Tukey's HSD test 

#by creating this plot, we can visualise the diamond's cut pairs and analyse 
#significant difference by plotting the "tuk" object in R. The significant
#difference are those which so not cross the vertival line at 0.

#distribution
#variable - price
ggplot(aes(price), data = diamonds) + 
  geom_histogram(color = 'black', fill = 'orange', binwidth = 500) #right skewed distribution
mean(diamonds$price)
median(diamonds$price)

#transformation
ggplot(aes(price), data = diamonds) + 
  geom_histogram(aes(y = ..density..),color = 'black', fill = 'orange') + 
  geom_density(alpha=.2, fill="#FF6666") + 
  scale_x_log10()+ 
  labs(title = 'log10 transformation')- 

#scatter plot
  ggplot(aes(x = carat, y = price), data = diamonds) +
  geom_point( color = 'orange', alpha = 1/4) +
  scale_x_continuous(lim = c(0, quantile(diamonds$carat, 0.99))) + 
  scale_y_continuous(lim = c(0, quantile(diamonds$price, 0.99)))

#regression line
ggplot(aes(x = carat, y = price), data = diamonds) +
  geom_point( color = 'orange', alpha = 1/4) +
  stat_smooth(method = 'lm') + 
  scale_x_continuous(lim = c(0, quantile(diamonds$carat, 0.99)), breaks = seq(0, 2.5, 0.2)) + 
  scale_y_continuous(lim = c(0, quantile(diamonds$price, 0.99)))

#calculate volume
volume <- diamonds$x*diamonds$y*diamonds$z
diamonds$volume<-volume  

ggplot(aes(x = volume, y = carat), data = diamonds) +
  geom_point(color = 'red',
             alpha = 1/10) + 
  geom_smooth(method = "lm") +
  xlim(0, 1000)

#correlation matrix, taken from assignment 1.

#clustering - kmean
#prepare data
diamonds.subset <- data.frame(diamonds$carat, diamonds$depth, diamonds$table, diamonds$price, diamonds$volume)
diamonds.subset <- na.omit(diamonds.subset)
k.means.fit <- kmeans(diamonds.subset, 2)
attributes(k.means.fit) #all the elements of cluster output
k.means.fit$centers #centroids
k.means.fit$cluster #cluster
k.means.fit$size #cluster size
#elbow curve
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(diamonds.subset, nc=6)


fit <- kmeans(diamonds.subset, 5) #k=5
clusplot(diamonds.subset,fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
plotcluster(diamonds.subset, fit$cluster)

cluster.table <- table(fit$cluster,diamonds.subset$diamonds.carat)
cluster.table[1:5,1:10] #confusion matrix
