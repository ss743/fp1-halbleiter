library("Hmisc")
x <- c(59.5, 122.06, 136.47)
y1 <- c(314, 648, 724)

erry1 <- c(11, 11, 9)


plot(x, y1, type="p", xlim=c(0, 150), ylim=c(-30, 1000), xlab = "Energy/keV", ylab = "channel", pch=4)
with (
  data = data.frame(x,y1,erry1)
  , expr = errbar(x, y1, y1+erry1, y1-erry1, type="n",add=T, pch=1, cap=.015)
)


text(x=50, y=800, "y=(5.330+-0.010)channel/keV*x-(3.1+-1.1)channel", cex=0.8)
fm1 <- lm(y1 ~ x, weighted=1/erry1^2)
summary(fm1)
abline(fm1, col = "red")


#*******************************************************************************
