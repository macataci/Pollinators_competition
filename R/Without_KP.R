library(deSolve)
library(ggplot2) # because we will plot things
library(tidyr)


Model <- function(t, y, p) {
  N <- y
  TP <- N[2]+N[3]
  with(as.list(p), {
    dB.dt <- N[1] * (1-(N[1]/K))*c*(TP/(D+TP))-(d[1]*N[1])
    dP1.dt <- a[1]*N[1]*c*N[2]*(1/(TP+D))-(d[2]*N[2])
    dP2.dt <-  a[2]*N[1]*c*N[3]*(1/(TP+D))-(d[3]*N[3])
    return(list(c(dB.dt, dP1.dt, dP2.dt)))
  })
}

#N[B,P1,P2]

# We need to find a conversion rate for bees cause they go to KB TOO fast. 

#Parameters
a <- c(0.008, 0.005)
c <- 38
D <- 20
K <- 1000
d <- c(0.008, 0.1, 0.1)
p <- list(a, c, D, K, d)
N0 <- c(100, 50, 50)
t <- c(1:100)


out <- ode(y = N0, times = t, func = Model, parms = p)
out <- as.data.frame(out)
colnames(out) <- c("Time", "Bees", "P1", "P2")
head(out)
View(out)

df <- pivot_longer(out, cols = 2:4)
colnames(df) <- c("Time", "Pop", "N")
ggplot(df) +
  geom_line(aes(x = Time, y = N, color = Pop), size=1.5) +
  labs(x = "Time", y = "N", color = "Species") +
  theme_classic()



