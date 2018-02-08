library(ggplot2)
library(aptheme)

ggplot(mtcars, aes(x = mpg, y = qsec)) + geom_point() + geom_smooth() + labs(title = "Testing title", xlab = "X axis", ylab = "Y axis") + theme_ap()
