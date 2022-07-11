
### script to calculate the ESS with bet-hedging
## depending on the expected magnitude of error in a strain estimating frequency
## we use the strategy that maximises avergae fitness across trials

### load required packages, palettes, and functions
library(ggplot2)
library(wesanderson)
pals10 <- wes_palette(10, name="Zissou1", type='continuous')
belch3 <- function(x, y, z) {eval(parse(text=(paste(x, y, z, sep=""))))}
belch5 <- function(x, y, z, a, b) {eval(parse(text=(paste(x, y, z, a, b, sep=""))))}
# constants
C <- 1
B <- 5
theta <- 0.5*(1/C - 1/B)
S <- C * B
# resolution of data
errordraws <- 25000
dp.output <- 3
dp.frequency <- 3
spec<-(10^dp.output)
spec1<-(10^dp.frequency)
P <- seq(from = 0, to = 1, by = 1/spec1)
dp=3

# calculate N-player strategy
IndOpt <- 0.5*((1/C) - (1/(B*P)))
IndOpt[IndOpt<0] <- 0

# function to find ESS and handle data
ESSfind2 <- function(x1, p, s2, p2){-(1-(C*x1))*(1+B*( (x1*p) + (s2*p2)) )}

# storage
output <- matrix(NA, spec1+1, 2)
colnames(output) <- c("p1", "ESS")
output[, 1] <- P
ESS <- rep(0, length(P))
ESS2 <- rep(0, length(P))
j <- 0 # needed to assign output to correct row

for (i in P){ # loop through frequencies and calculate normal ESS
  j <- j + 1  
  p1 <- i # set frequency
  p2 <- 1 - i
  s2 <- 0
  ESS[j] <- optimize(ESSfind2, c(0, theta), p=p1, s2=s2, p2=p2)$min
  output[j, 2] <- ESS[j]
}

### now look at bet hedging

# define range of standard deviations to use. This defines how badly a strain estimates
# its freqeuncy, and we will plot a different line for each standard deviation
sd.range <- seq(from=0.1, to=1, by=0.1) # seq from 0.1 -> 1 with 0.1 intervals

## function to find fitness for any strategy
pila <- function(par, pi, s2){-sum((1-par)*(1+B*( (par*pi) + (s2*(1-pi)) ) ) )}

for (m in sd.range){ # loop through range of standard deviations
  belch3("answer",m,"<<-c()")
  print(paste("SD LEVEL: ", m, sep=""))
  sd <- m
  deviations <- round(rnorm(errordraws, 0, sd), dp.frequency) 
  s2 <- rev(output[, 2]) # ESS from perfect information 
  answer <- c() 
  j <- 0
  qq=0.175
  for (i in P){ # loop through frequencies, find the strategies that maximises average fitness
    j <- j + 1
    pi <- pi <- i + round(deviations * (dnorm(i,0.5,qq)-dnorm(0,0.5,qq))/(dnorm(0.5,0.5,qq)-dnorm(0,0.5,qq)), dp)
    pi[pi<0] <- 0
    pi[pi>1] <- 1
    answer <- append(answer, optimize(pila, c(0,theta), pi=pi, s2=0)$min)# max average fitness across trials
  }
  belch3("answer",m,"<<-answer")
}

ESSS <- matrix(0, length(P), length(sd.range)+1)
ESSS[, 1] <- P
for(i in 1:length(sd.range)){
belch3("ESSS[, i+1] <<- answer", sd.range[i],"")
}  

outputy <- as.data.frame(output)
varys <- as.data.frame(ESSS)

pl198 <- ggplot(varys, aes(x=V1, y=V2))
pl198 <- pl198 + 
  geom_line(data=outputy, aes(x=p1, y=ESS), col='black', size=1.5) + 
  geom_line(data=varys, aes(x=V1, y=V3), col=pals10[2], size=1.5) +
  geom_line(data=varys, aes(x=V1, y=V4), col=pals10[3], size=1.5) +
  geom_line(data=varys, aes(x=V1, y=V5), col=pals10[4], size=1.5) +
  geom_line(data=varys, aes(x=V1, y=V6), col=pals10[5], size=1.5) +
  geom_line(data=varys, aes(x=V1, y=V7), col=pals10[6], size=1.5) +
  geom_line(data=varys, aes(x=V1, y=V8), col=pals10[7], size=1.5) +
  geom_line(data=varys, aes(x=V1, y=V9), col=pals10[8], size=1.5) +
  geom_line(data=varys, aes(x=V1, y=V10), col=pals10[9], size=1.5) +
  geom_line(data=varys, aes(x=V1, y=V11), col=pals10[10], size=1.5) +
  ylim(0,0.35)+
  scale_y_continuous(limits=c(-0.02,0.42),breaks = seq(0, 0.4, by = 0.4),
                     labels=c(0,expression(theta)),expand = c(0, 0))+
  labs(x='Relatedness',y='Investment in Public Good') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title=element_text(hjust=0)) +
  theme(aspect.ratio=1) + 
  theme(text = element_text(size=28))
pl198
