
### Script to plot the effects of error in frequency estimation
### on investment strategy

### A) threshold for relatedness at which a strain invests;
# >0 (red line)
# > 1/4 theta (yellow line)
# > 1/2 theta (green line)
# > 3/4 theta (blue line)
### as the magnitude of error in frequency estimation increases

### B) The same thresholds for a fixed magnitue of error, 
### but varying how frequency-dependant the rror is

errordraws=25000 # how many error iterations to use (>250000 for high qual figures)
eee <- seq(0, 0.5, by=0.01) # define range of errors to investigate
BB=2.5 # fix benefit B
CC=1 # fix cost C
thetas1=0.5*((1/CC)-(1/BB)) # define theta (optimal investment)

### The function 'ESSfindD' finds the ESS with an adaptive response to
## transdifferentiation
ESSfindD <- function(x1, p, s2, b, c, thetas){-(1-(c*(x1+(D*(thetas-(x1*p)))))) * 
    (1 + (b*p)*( ((x1) + (s2*(1-p)) ) + 
                   (D* (thetas-( (x1*p) + (s2*(1-p)) ) ) ) ) )} 
## input parameters: 
## x1 = the investment strategy of the focal player
## p = the frequency/relatedness of the focal player
## s2 = the investment strategy of player 2
## b = benefit
## c = cost

## load packages and colour palettes
library(wesanderson)
library(ggplot2)
## define colour palettes for plotting
pals15 <- wes_palette(5, name="Zissou1", type='continuous') # colour palette
pals15=rev(pals15)
pals16=wes_palette(4, name="Zissou1", type='continuous')
pals16=rev(pals16)

## make required variables and output matrix
PP=seq(0,1,0.001) # range of frequencies to search through
dp=3 # decimal places for rounding
errors <- PP # range of errors
zz <- round(PP, dp) # frequencies rounded to correct level
Search <- matrix(NA, length(PP), length(eee))
t <- 0
for(k in 1:length(eee)){ # loop through errors
  t <- t+1
  ESS <- PP
  D=0.0 # set the value of transdifferentiation D
  jj <- 0 # needed to assign output to correct row
  for (w in PP){ # loop through frequencies
    jj <- jj+1
    ## find optimal ESS without error
    ESS[jj] <- optimize(ESSfindD, c(0, thetas1), p=w, s2=0, c=CC, b=BB, thetas=thetas1)$min
  }
  ### use a normal distribution to define errors in frequency estimations
  deviations <- round(rnorm(errordraws, 0, eee[k]), dp)
  w <- 0
  for (i in PP){
    w <- w+1
    # estimated frequency = true frequency +- error
    pi <- i + round(deviations, dp)
    pi <- pi[pi<=1]
    pi <- pi[pi>=0]
    # chosen strategy becomes the already-calculated strategy for the new
    # estimated frequency
    Search[w, t] <- mean(ESS[match(round(pi, dp), zz)]) # save ESS for that error
  }
} # end loop through errors

# calculate the thresholds, and plot
Search=Search/thetas1
fig2amat=matrix(NA, length(eee), 4)
for(q in 1:ncol(Search)){
  fig2amat[q,1]=which(Search[,q]>min(Search[q]))[1]
  fig2amat[q,2]=which(Search[,q]>0.25)[1]
  fig2amat[q,3]=which(Search[,q]>0.5)[1]
  fig2amat[q,4]=which(Search[,q]>0.75)[1]
}
fig2amat=cbind(eee, fig2amat)
vardf3=as.data.frame(fig2amat)

pl3 <- ggplot(vardf3, aes(x=eee, y=V2))
pl3 <- pl3 + 
  geom_line(col=pals16[1], size=2)+
  geom_line(data=vardf3, aes(x=eee, y=V3), col=pals16[2], size=2) +
  geom_line(data=vardf3, aes(x=eee, y=V4), col=pals16[3], size=2) +
  geom_line(data=vardf3, aes(x=eee, y=V5), col=pals16[4], size=2) +
  ylim(0, 1000) +
  xlim(0, 0.5)+
  ggtitle('A')+
  labs(x='Error',y='Relatedness Threshold') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title=element_text(hjust=0)) +
  theme(aspect.ratio=1) + 
  theme(text = element_text(size=18))+
  scale_y_continuous(breaks=c(0, 250, 500, 750, 1000), labels=c(0, 0.25, 0.5, 0.75, 1.00))+
  geom_segment(aes(x = 0.45, y = 600, xend = 0.475, yend = 600), col=pals16[1], size=1.5) +
  annotate(geom="text", x=0.49, y=600, label="0",color=pals16[1], size=4)+
  geom_segment(aes(x = 0.45, y = 550, xend = 0.475, yend = 550), col=pals16[2], size=1.5) +
  annotate(geom="text", x=0.49, y=550, label=expression("\u00BC"),color=pals16[2], size=4)+
  geom_segment(aes(x = 0.45, y = 500, xend = 0.475, yend = 500), col=pals16[3], size=1.5) +
  annotate(geom="text", x=0.49, y=500, label=expression("\u00BD"),color=pals16[3], size=4)+
  geom_segment(aes(x = 0.45, y = 450, xend = 0.475, yend = 450), col=pals16[4], size=1.5) +
  annotate(geom="text", x=0.49, y=450, label=expression("\u00BE"),color=pals16[4], size=4)
pl3

#####################################################################
### Panel B, frequency-dependant error

eee <- seq(0.5, 0.5, by=0.01)
BB=2.5
CC=1
thetas1=0.5*((1/CC)-(1/BB))

errors <- PP
zz <- round(PP, dp)
Search2 <- matrix(NA, length(PP), 4)
t <- 0
for(k in 1:length(eee)){ # start loop through errors
  print(eee[k])
  for(z in 1:4){
    t <- t+1
    ESS <- PP
    jj <- 0 # needed to assign output to correct row
    for (w in PP){ # loop through p1
      jj <- jj+1
      ESS[jj] <- optimize(ESSfindD, c(0, thetas1), p=w, s2=0, c=CC, b=BB, thetas=thetas1)$min
    }
    deviations <- round(rnorm(errordraws, 0, eee[k]), dp) # Gaussian
    w <- 0
    for (i in PP){
      w <- w+1
      if(z==4){pi <- i + round(deviations * 256 * (i^4) * ((1-i)^4), dp)}
      if(z==3){pi <- i + round(deviations * 64 * (i^3) * ((1-i)^3), dp)}
      if(z==2){pi <- i + round(deviations * 16 * (i^2) * ((1-i)^2), dp)}
      if(z==1){pi <- i + round(deviations * 4 * (i^1) * ((1-i)^1), dp)}
      pi <- pi[pi<=1]
      pi <- pi[pi>=0]         
      Search2[w, t] <- mean(ESS[match(round(pi, dp), zz)]) # save ESS for that error
    }
  }
} # end loop through errors

Search2=Search2/thetas1

fig2amat2=matrix(NA, 4, 4)
for(q in 1:ncol(Search2)){
  fig2amat2[q,1]=which(Search2[,q]>min(Search[q]))[1]
  fig2amat2[q,2]=which(Search2[,q]>0.25)[1]
  fig2amat2[q,3]=which(Search2[,q]>0.5)[1]
  fig2amat2[q,4]=which(Search2[,q]>0.75)[1]
}

fig2amat2=cbind(c(1,2,3,4), fig2amat2)
vardf4=as.data.frame(fig2amat2)

pl4 <- ggplot(vardf4, aes(x=V1, y=V2))
pl4 <- pl4 + 
  geom_line(col=pals16[1], size=2)+
  geom_line(data=vardf4, aes(x=V1, y=V3), col=pals16[2], size=2) +
  geom_line(data=vardf4, aes(x=V1, y=V4), col=pals16[3], size=2) +
  geom_line(data=vardf4, aes(x=V1, y=V5), col=pals16[4], size=2) +
  ylim(0, 1000) +
  xlim(1, 4)+
  ggtitle('B')+
  labs(x='Error Exponent',y='Relatedness Threshold') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title=element_text(hjust=0)) +
  theme(aspect.ratio=1) + 
  theme(text = element_text(size=18))+
  geom_segment(aes(x = 3.72, y = 600, xend = 3.86, yend = 600), col=pals16[1], size=1.5) +
  annotate(geom="text", x=3.95, y=600, label="0",color=pals16[1], size=4)+
  geom_segment(aes(x = 3.72, y = 550, xend = 3.86, yend = 550), col=pals16[2], size=1.5) +
  annotate(geom="text", x=3.95, y=550, label=expression("\u00BC"),color=pals16[2], size=4)+
  geom_segment(aes(x = 3.72, y = 500, xend = 3.86, yend = 500), col=pals16[3], size=1.5) +
  annotate(geom="text", x=3.95, y=500, label=expression("\u00BD"),color=pals16[3], size=4)+
  geom_segment(aes(x = 3.72, y = 450, xend = 3.86, yend = 450), col=pals16[4], size=1.5) +
  annotate(geom="text", x=3.95, y=450, label=expression("\u00BE"),color=pals16[4], size=4)+
  scale_y_continuous(breaks=c(0, 250, 500, 750, 1000), labels=c(0, 0.25, 0.5, 0.75, 1.00), limits=c(0,1000))
pl4

