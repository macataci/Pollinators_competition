library(deSolve)
library(ggplot2) 
library(tidyr)

Model <- function(t, y, p) {
  N <- y
  TP <- N[2]+N[3]
  V <- (N[1]*TP)/(Db+TP)
  V1 <- (N[1]*N[2])/(Db+N[2])
  V2 <- (N[1]*N[3])/(Db+N[3])
  with(as.list(p), {
    dB.dt <- (beta*V) - (Kb*N[1]^2) - (d[1]*N[1])
    dP1.dt <- lambda[1]*V1/(Dv+V1) - (K1*N[2]^2) - d[2]*N[2]
    dP2.dt <- lambda[2]*V2/(Dv+V2) - (K2*N[3]^2) - d[3]*N[3]
    return(list(c(dB.dt, dP1.dt, dP2.dt)))
  })
}

# we can change gammas

#Parameters

beta <- 0.7
lambda <- c(1, 0.5)
Db <- 5
Dv <- 5
Kb <- 0.01
K1 <- 0.0004
K2 <- 0.0004
d <- c(0.02, 0.04, 0.04)
p <- list(beta, lambda, Dv, Db, Kb, K1, K2, d)
N0 <- c(20, 10, 10)
t <- c(1:1000)


out <- ode(y = N0, times = t, func = Model, parms = p)
out <- as.data.frame(out)
colnames(out) <- c("Time", "Bees", "P1", "P2")
#View(out)

df <- pivot_longer(out, cols = 2:4)
colnames(df) <- c("Time", "Pop", "N")
ggplot(df) +
  geom_line(aes(x = Time, y = N, color = Pop), size=1.5) +
  labs(x = "Time", y = "N", color = "Species") +
  theme_classic()
tail(out)

