mydata <- read.csv("/Users/sunnywu/Desktop/DataAnalysisAssignment1/abalones.csv", sep = "")
str(mydata)

#Define VOLUME and RATIO variables
mydata$VOLUME <- mydata$LENGTH * mydata$DIAM * mydata$HEIGHT mydata$RATIO <- mydata$SHUCK / mydata$VOLUME
summary(mydata)
head(mydata)

legend.text=c("Infant","Female","Male"),
main = "Comparison Abalone Sex Frequencies", ylab = "Frequency",

xlab = "Age Class", beside = TRUE, col = c("darkblue","red","green"), names.arg= c("A1","A2","A3","A4","A5","A6"))

set.seed(123)
work <- mydata[sample(1:nrow(mydata), 200, replace = FALSE),] plot(work[, 2:6])
plot (x=1:10, y=1:10,type="p",main="Example Scatterplot Using abline()",xlab="X",ylab="Y") abline(a=0, b=1)

#Q5a
with(mydata,
interaction.plot(CLASS, SEX, VOLUME,type="b",
col=c("#F8766D","#00BA38","#619CFF"),
lty = 1, ylab = "Mean VOLUME", xlab = "CLASS", lwd = 2, trace.label = "SEX", pch = c(1,1,1),

main = "Plot of Mean VOLUME versus CLASS for Three Sexes"))
out <-aggregate(VOLUME ~ SEX + CLASS, data = mydata, mean)

with(mydata,
interaction.plot(CLASS, SEX, RATIO,type="b", col=c("#F8766D","#00BA38","#619CFF"),
lty = 1, ylab = "Mean RATIO", xlab = "CLASS", lwd = 2, trace.label = "SEX", pch = c(1,1,1),

main = "Plot of Mean RATIO versus CLASS for Three Sexes"))
out <-aggregate(RATIO ~ SEX + CLASS, data = mydata, mean)

#Q5b
ggplot(data=out, aes(x= CLASS, y= VOLUME, group = SEX, colour = SEX)) +
geom_line() + geom_point(size=4) +
ggtitle("Plot of Mean VOLUME versus CLASS for Three Sexes")
ggplot(data=out, aes(x= CLASS, y = RATIO, group = SEX, colour = SEX)) + geom_line() + geom_point(size=4) +
ggtitle("Plot of Mean RATIO versus CLASS for Three Sexes")

data(mtcars)
par(mfrow=c(3,3))
grid.arrange(ggplot(mtcars,aes(x=factor(cyl),y=mpg, group=cyl)) + geom_boxplot() +
labs(x="Numbr of Cylinders", y= "Miles per Gallon"), ggplot(mtcars,aes(x=factor(cyl),y=hp, group=cyl)) + geom_boxplot() +
labs(x="Numbr of Cylinders", y= "Horsepower"), nrow=1,
top = "Example Boxplots using ggplot() and grid.arrange()" )
skewness(mydata$LENGTH) skewness(mydata$WHOLE)

par(mfrow=c(1,1))
hist(mydata$LENGTH) kurtosis(mydata$LENGTH) addmargins(table(mydata$SEX,mydata$CLASS)) skewness(mydata$SEX) plot(mydata$VOLUME,mydata$WHOLE) abline(a = 0, b = 3/10)
plot(mydata$SHUCK,mydata$WHOLE) abline(a = 0, b = 30/15)
par(mfrow=c(3,3)) hist(mydata$RATIO, SEX = I)
summary(mydata$LENGTH)
