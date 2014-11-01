#sine and cosine functions
t = seq(0.0,0.98,0.01)
y1 = sin(2*pi*4*t)
plot(t, y1, type="l",col="red")
y2 = cos(2*pi*4*t)
lines(t, y2, col="green")

#sigmoid function
const = 5
t = seq(-1,1,0.001)
y1 = 1/(1+exp(-const * t))
plot(t, y1, type="l",col="red")

#log function
t = seq(0,5,0.01)
y1 = log(t)
plot(t, y1, type="l",col="red", main="Log Plot", ylim=c(-10, +10))
y2 = -log(t)
lines(t, y2, col="green")
