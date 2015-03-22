library("memisc")
library("lmtest")
library("ggplot2")
library("dplyr")
library("foreign")
library("vcd")
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")

h <- diamonds
glimpse(h)

qplot(data = h, carat, price)
bg <- qplot(data = h, log(carat), log(price))

bg + geom_hex()

f <- read.csv("flats_moscow.txt", sep = "\t", header = TRUE, dec = ".")
glimpse(f)

qplot(data = f, totsp, price)
str(f)

qplot(data = f, log(totsp), log(price))

mosaic(data = f, ~walk + brick + floor, shade = TRUE)

f <- mutate_each(f, "factor", walk, brick, floor, code)
glimpse(f)
glimpse(diamonds)

qplot(data = f, log(price))
qplot(data = f, log(price), fill = brick)
qplot(data = f, log(price), fill = brick, position = "dodge")
qplot(data = f, log(price), fill = brick, geom = "density")
qplot(data = f, log(price), fill = brick, geom = "density", alpha = 0.5)
g2 <- qplot(data = f, log(price), fill = brick, geom = "density", alpha = 0.5)
g2 + facet_grid(walk~floor)

model_0 <- lm(data = f, log(price)~log(totsp))
model_1 <- lm(data = f, log(price)~log(totsp)+brick)
model_2 <- lm(data = f, log(price)~log(totsp)+brick+brick:log(totsp))

nw <- data.frame(totsp=c(60, 60), brick = factor(c(1, 0)))
nw
predict(model_2, newdata = nw )
exp(predict(model_2, newdata = nw))
exp(predict(model_2, newdata=nw, interval="confidence"))

predict(model_2, newdata = nw, interval="prediction")
exp(predict(model_2, newdata = nw, interval="prediction"))

waldtest(model_0, model_1)
