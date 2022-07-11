
#### Script to find the ESS of the collective investment game
#### with transdifferentitation

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

## load packages
library(wesanderson)
library(ggplot2)

## define colour palettes for plotting
pals15 <- wes_palette(5, name="Zissou1", type='continuous') # colour palette
pals15=rev(pals15)
pals16=wes_palette(4, name="Zissou1", type='continuous')
pals16=rev(pals16)

PP=seq(0,1,0.001) # range of frequencies to search through
beeseq=c(2, 3, 4, 6, 12) # range of benefits (B) to search through
CC=1 # set the cost
outmat=matrix(NA, length(PP), length(beeseq)) # matrix to store outut

## loop through benefits
for(i in 1:length(beeseq)){
  BB=beeseq[i] # set the benefit
  thetas1=0.5*((1/CC)-(1/BB)) # calculate4 theta
  D=0.0 # set the value of transdifferentiation D
  ESS <- PP # prior guess for ESS
  jj <- 0 # variable to assign output to correct row
  for (w in PP){ # loop through frequencies
    jj <- jj+1
    ## apply function to calculate ESS
    ESS[jj] <- optimize(ESSfindD, c(0, thetas1), p=w, s2=0, c=CC, b=BB, thetas=thetas1)$min
  }
  outmat[,i]=ESS/thetas1 # save ESS in output
}

### plot output
outmat=cbind(PP, outmat)
vardf1=as.data.frame(outmat)

pl1 <- ggplot(vardf1, aes(x=PP, y=V2))
pl1 <- pl1 + 
  theme_bw()+
  geom_hline(yintercept=0.5, linetype='dashed')+
  geom_line(col=pals15[1], size=2)+
  geom_line(data=vardf1, aes(x=PP, y=V3), col=pals15[2], size=2) +
  geom_line(data=vardf1, aes(x=PP, y=V4), col=pals15[3], size=2) +
  geom_line(data=vardf1, aes(x=PP, y=V5), col=pals15[4], size=2) +
  geom_line(data=vardf1, aes(x=PP, y=V6), col=pals15[5], size=2) +
  geom_segment(aes(x = 0.85, y = 0.4, xend = 0.92, yend = 0.4), col=pals15[1], size=1.5) +
  annotate(geom="text", x=0.95, y=0.4, label="2",color=pals15[1], size=4)+
  geom_segment(aes(x = 0.85, y = 0.35, xend = 0.92, yend = 0.35), col=pals15[2], size=1.5) +
  annotate(geom="text", x=0.95, y=0.35, label="3",color=pals15[2], size=4)+
  geom_segment(aes(x = 0.85, y = 0.3, xend = 0.92, yend = 0.3), col=pals15[3], size=1.5) +
  annotate(geom="text", x=0.95, y=0.3, label="4",color=pals15[3], size=4)+
  geom_segment(aes(x = 0.85, y = 0.25, xend = 0.92, yend = 0.25), col=pals15[4], size=1.5) +
  annotate(geom="text", x=0.95, y=0.25, label="6",color=pals15[4], size=4)+
  geom_segment(aes(x = 0.85, y = 0.2, xend = 0.92, yend = 0.2), col=pals15[5], size=1.5) +
  annotate(geom="text", x=0.95, y=0.2, label="12",color=pals15[5], size=4)+
  xlim(0, 1.01)+
  ggtitle('A')+
  labs(x='Relatedness',y='Public goods Investment') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title=element_text(hjust=0))+
  theme(aspect.ratio=1) + 
  theme(text = element_text(size=18))+
  scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), labels=c(0, expression("\u00BC"*theta), expression("\u00BD"*theta), expression("\u00BE"*theta), expression(theta)))
pl1





