library(deSolve)
library(ggplot2) 
library(tidyr)

Model <- function(t, y, p) {
  N <- y
  TP <- N[2]+N[3]
  V <- (N[1]*TP)/(Db+TP)
  with(as.list(p), {
    dB.dt <- (beta*V) - (Kb*N[1]^2) - (d[1]*N[1])
    dP1.dt <- lambda[1]*(V*N[2]/TP)/(Dv+(V*N[2]/TP)) - (K[1]*N[2]^2) - d[2]*N[2]
    dP2.dt <- lambda[2]*(V*N[3]/TP)/(Dv+(V*N[2]/TP)) - (K[4]*N[3]^2) - d[3]*N[3]
    return(list(c(dB.dt, dP1.dt, dP2.dt)))
  })
}

# we can change gammas
#Parameters

Kb <- 0.001
beta <- 0.1
lambda <- c(1, 2)
Dv <- 10
Db <- 100
K <-  matrix(c(0.0001, 0, 0, 0.0001), nrow = 2)
d <- c(0.01, 0.001, 0.001)
p <- list(Kb, beta, l, Dv, Db, K, d)
N0 <- c(50, 180, 160)
t <- c(1:2000)


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

ggplot(data.frame(out)) +
  geom_line(aes(x = P1, y = P2), size=1.5) +
  labs(x = "Plant 1", y = "Plant 2") +
  theme_classic()
tail(out)

