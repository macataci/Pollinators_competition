library(deSolve)
library(ggplot2) 
library(tidyr)

Model <- function(t, y, p) {
  N <- y
  TP <- N[2]+N[3]
  with(as.list(p), {
    dB.dt <- -Kb*(N[1]^2) + (N[1]*beta*c*(TP/(D+TP)))-d*N[1]
    dP1.dt <- a[1]*N[1]*(N[2]/TP)*c*(TP/(D+TP)) -((K[1,1]*(N[2]^2))+(K[1,2]*N[2]*N[3]))
    dP2.dt <-   a[2]*N[1]*(N[3]/TP)*c*(TP/(D+TP)) -((K[2,2]*(N[3]^2))+(K[2,1]*N[2]*N[3]))
    return(list(c(dB.dt, dP1.dt, dP2.dt)))
  })
}


#Parameters

beta <- 0.8
a <- c(0.006, 0.005)
c <- 50
Kb <- 0.05
D <- 20
K <- matrix(c(0.05, 0.008, 0.1, 0.05), nrow = 2)
d <- 0.0008
p <- list(beta, a, c, D, K, d)
N0 <- c(10, 10, 1000)
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
