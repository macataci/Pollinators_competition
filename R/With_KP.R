library(deSolve)
library(ggplot2) # because we will plot things
library(tidyr)

Model <- function(t, y, p) {
  N <- y
  TP <- N[2]+N[3]
  with(as.list(p), {
    dB.dt <- beta*N[1] * (1-(N[1]/K[1]))*c*(TP/(D+TP))-(d[1]*N[1])
    dP1.dt <- a[1]*N[1]*(1-(N[2]/K[2]))*c*N[2]*(1/(TP+D))-(d[2]*N[2])
    dP2.dt <-  a[2] * N[1]*(1-(N[3]/K[3]))*c*N[3]*(1/(TP+D))-(d[3]*N[3])
    return(list(c(dB.dt, dP1.dt, dP2.dt)))
  })
}

# we can change gammas

#Parameters

beta <- 0.7
a <- c(0.006, 0.001)
c <- 50
D <- 500
K <- c(100, 1000,1000)
d <- c(0.008, 0.004, 0.004)
p <- list(beta, a, c, D, K, d)
N0 <- c(10, 90, 10)
t <- c(1:10000)


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
