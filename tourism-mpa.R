#Pristine seas tourism-MPA model
#Author: Reniel Cabral
#Last Edit: 18 May 2021

#Empirically derived parameters
Qd0 <- 100 #current number of dives
P0 <- 50 #current price per dive in USD
C0 <- 150 #choke price in USD
alpha <- 0.5
beta <- 0.5
X0 <- 10 #dive price (USD) where no company will offer their service
nu <- 1.1

#Model intermediate output
deltaB <- 20 #change in biomass
deltaS <- 10 #change in species diversity metric

#Parameter calculations
#we need a and b to compute mu
(a <- (C0*Qd0)/(C0-P0))
(b <- Qd0/(C0-P0))
(mu <- nu*(a-(b*P0))) #assume that MPA will increase the demand for diving by 10%.

(e <- (a-(b*P0))/(P0-X0)) #Slope of the supply curve
(c <- (((b*P0)-a)*X0)/(P0-X0)) #Dive quantity supplied by the industry when the price of diving is zero

(P1 <- (a-c+mu+(alpha*deltaB)+(beta*deltaS))/(e+b)) #Price per dive at site i when the site is converted to an MPA
(Qd1 <- a-(b*P1)+mu+(alpha*deltaB)+(beta*deltaS)) #Number of dives at site i when the site is converted to an MPA
(C1 <- (a+mu+(alpha*deltaB)+(beta*deltaS))/b)

##Tourism model output
#Change in tourism revenue at site i
(P1*Qd1) - (P0*Qd0)

#Change in counsumer surplus
(0.5*(C1-P1)*Qd1) - (0.5*(C0-P0)*Qd0)

#Change in consumer + producer surplus
(Qd0*(C1-C0)) + (0.5*(Qd1-Qd0)*(C1-C0))
