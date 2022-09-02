library(deSolve)
library(ggplot2) 
library(tidyr)

Model <- function(t, y, p) {
  N <- y
  TP <- N[2]+N[3]
  V <- (N[1]*TP*cp)/(Dp+TP)
  with(as.list(p), {
    dB.dt <- (beta*V) - (Kb*N[1]^2) - (d[1]*N[1])
    dP1.dt <- lambda[1]*N[2]*(cv*V*N[2]/TP)/(Dv+(V*N[2]/TP)) - (K[1]*N[2]^2) - d[2]*N[2]
    dP2.dt <- lambda[2]*N[3]*(cv*V*N[3]/TP)/(Dv+(V*N[3]/TP)) - (K[4]*N[3]^2) - d[3]*N[3]
    return(list(c(dB.dt, dP1.dt, dP2.dt)))
  })
}

# we can change gammas
#Parameters
dvar <- seq(0.01, 5, 0.05)
res <- NULL

l2var <- c(0.5, 1, 3)
l1var <- seq(0.1,6,0.1)

critical <- matrix(data = NA, nrow = length(l1var)*length(l2var), ncol = 4)
colnames(critical) <- c("l1", "l2", "T1", "T2")

it <- 1
for(l2 in l2var){
  for(l1 in l1var){
    run <- data.frame()
    res <- data.frame()
    for (i in 1:length(dvar)) {
      Kb <- 0.01
      beta <- 0.5
      lambda <- c(l1, l2)
      K <-  matrix(c(1, 0, 0, 1), nrow = 2)
      d <- c(0.01 + dvar[i], 0.1, 0.8)
      cp <- 10
      Dp <- 10
      cv <- 50
      Dv <- 30
      p <- list(Kb, beta, lambda, cp, Dp, cv, Dv, K, d)
      N0 <- c(50, 10, 10)
      t <- c(1:1000)
      
      
      out <- ode(y = N0, times = t, func = Model, parms = p)
      out <- as.data.frame(out)
      colnames(out) <- c("Time", "Bees", "P1", "P2")
      run <- tail(out,1 )[,2:4]
      run <- cbind(d[1], run)
      res <- rbind(res, run)
    }
    critical[it,1] <- l1
    critical[it,2] <- l2
    critical[it,3] <- res[which(res[,3]<1)[1],1]
    critical[it,4] <- res[which(res[,4]<1)[1],1]
    
    print(it)
    it <- it+1
  }
}

DFcritical <- as.data.frame(critical)

plt1 <- data.frame(rbind(cbind(DFcritical$l1[1:60],DFcritical$T1[1:60]),cbind(DFcritical$l1[1:60],DFcritical$T2[1:60])))
plt1$label <- c(rep("T1",60),rep("T2",60))

plt2 <- data.frame(rbind(cbind(DFcritical$l1[61:120],DFcritical$T1[61:120]),cbind(DFcritical$l1[61:120],DFcritical$T2[61:120])))
plt2$label <- c(rep("T1",60),rep("T2",60))

plt3 <- data.frame(rbind(cbind(DFcritical$l1[121:180],DFcritical$T1[121:180]),cbind(DFcritical$l1[121:180],DFcritical$T2[121:180])))
plt3$label <- c(rep("T1",60),rep("T2",60))


g <- ggplot(data=plt1, aes(x=X1, y=X2, group=label, colour=label))+
  geom_line(size=1.5)+
  labs(x = "lambda 1", y = "Bees death rate")+
  theme_classic()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15,face="bold"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=15,face="bold"))

g + scale_colour_discrete(name = "Threshold",
                          breaks = c("T1","T2"),
                          labels = c("Plant 1", "Plant 2"))


### PLT 2

g2 <- ggplot(data=plt2, aes(x=X1, y=X2, group=label, colour=label))+
  geom_line(size=1.5)+
  labs(x = "lambda 1", y = "Bees death rate")+
  theme_classic()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15,face="bold"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=15,face="bold"))

g2 + scale_colour_discrete(name = "Threshold",
                          breaks = c("T1","T2"),
                          labels = c("Plant 1", "Plant 2"))


### PLT 3

g3 <- ggplot(data=plt3, aes(x=X1, y=X2, group=label, colour=label))+
  geom_line(size=1.5)+
  labs(x = "lambda 1", y = "Bees death rate")+
  theme_classic()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15,face="bold"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=15,face="bold"))

g3 + scale_colour_discrete(name = "Threshold",
                          breaks = c("T1","T2"),
                          labels = c("Plant 1", "Plant 2"))



