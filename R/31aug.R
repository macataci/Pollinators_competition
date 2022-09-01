library(deSolve)
library(ggplot2) 
library(tidyr)

Model <- function(t, y, p) {
  N <- y
  TP <- N[2]+N[3]
  with(as.list(p), {
    dB.dt <- beta*N[1]*(1-(N[1]/K[1]))*c*(TP/(D+TP))-(d[1]*N[1])
    dP1.dt <- a[1]*N[1]*N[2]*(c/(TP+D))*(1-(N[2]*K[2]))-(d[2]*N[2])
    dP2.dt <-  a[2]*N[1]*N[3]*(c/(TP+D))*(1-(N[3]*K[3]))-(d[3]*N[3])
    return(list(c(dB.dt, dP1.dt, dP2.dt)))
  })
}

# we can change gammas

#Parameters Killing P1
beta <- 0.05
a <- c(3.2, 20)
c <- 20
D <- 3
K <- c(80, 0.0981, 0.00058)
d <- c(0.008, 1, 1)
p <- list(beta, a, c, D, K, d)
N0 <- c(10, 100, 100)
t <- c(1:1000)


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
