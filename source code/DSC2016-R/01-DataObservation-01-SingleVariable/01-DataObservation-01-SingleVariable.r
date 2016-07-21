source("http://hjhsu.github.io/r_course/init-swirl.R")
library(swirl)
swirl()
?plot
?infert
View(infert)
spon <- infert$spontaneous
numeric()
plot(spon)
spon <- factor(spon)
plot(spon)
table(spon)
pie(table(spon))
age <- infert$age
numeric()
plot(age)
plot(age,type = "l")
x <- hist(age)
x
sum(age > 26 & age <= 28)
?cut
cut(1:10,2:4)
plot(cut(age, breaks = x$breaks))
infert$education
plot(density(age))
plot(density(age, bw = 0.1))
plot(density(age, bw = 1))
plot(density(age, bw = "SJ"))
sunspot.year
class(sunspot.year)
plot(sunspot.year)
x <- tail(sunspot.year, 100)
x
plot(x)
lines(x)
lines(x, lty = 3, lwd = 3, col = 2)
dst <- tempfile(fileext = ".png")
png(dst)
plot(x)
dev.off()
skip()
View(hsb)
plot(density(hsb$write,bw="SJ"))
pie(table(hsb$sex))
submit()
submit()
submit()
submit()
savehistory("C:/Users/L/Desktop/test.r")
