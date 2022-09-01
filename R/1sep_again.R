library(deSolve)
library(ggplot2) 
library(tidyr)

Model <- function(t, y, p) {
  N <- y
  TP <- N[2]+N[3]
  #Vi <- N[1]*TP/(Db+TP)
  P1Ra <- N[2]/TP
  P2Ra <- N[3]/TP
  
  with(as.list(p), {
    dB.dt <- beta*B(TP/(Db+TP))*(1-(N[1]*Kb))-d[1]*N[1] 
    dP1.dt <- l[1]*N[2]*(N[1]*(N[2]/TP)/(Dv+N[1]*(N[2]/TP)))*(1-(K[1,1]*N[2]+K[1,2]*N[3]))-(d[2]*N[2])
    dP2.dt <- l[2]*N[3]*(N[1]*(N[3]/TP)/(Dv+N[1]*(N[3]/TP)))*(1-(K[2,2]*N[3]+K[2,1]*N[2]))-(d[3]*N[3])
    return(list(c(dB.dt, dP1.dt, dP2.dt)))
  })
}

#COEXISTENCE

Kb <- 0.8
beta <- 0.05
l <- c(0.06, 0.02)
Dv <- 10
Db <- 20
K <-  matrix(c(0.056, 0, 0, 0.007), nrow = 2)
d <- c(0.008, 0.01, 0.01)
p <- list(Kb, beta, l, Dv, Db, K, d)
N0 <- c(10, 20, 20)
t <- c(1:5000)

#KILLINGGGGGGGGGGGGGGGGGGGGGGG! <3 Just one plantinha

Kb <- 0.0072
beta <- 0.05
l <- c(0.3, 0.5)
D <- 10
K <-  matrix(c(0.00056, 0, 0, 0.00376), nrow = 2)
d <- c(0.008, 0.01, 0.01)
p <- list(Kb, beta, l, Dv, Db, K, d)
N0 <- c(10, 20, 20)
t <- c(1:5000)

#KILLINGGGGGGGGGGGGGGGGGGGGGGG! <3 Both plantinhas

Kb <- 0.0072
beta <- 0.05
l <- c(1.5, 1.3)
D <- 100
K <-  matrix(c(0.00056, 0, 0, 0.000376), nrow = 2)
d <- c(0.008, 0.01, 0.01)
p <- list(Kb, beta, l, Dv, Db, K, d)
N0 <- c(10, 20, 20)
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
