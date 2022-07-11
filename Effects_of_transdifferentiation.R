
### script to plot model predictions of allocated and realized investment into 
## public goods, as frequency/relatedness varies

## allocated investment is the strategy that a strain chooses
## realized investment is the strategy that occurs AFTER transdifferentiation

PP=seq(0,1,0.001) # range of relatedness
BB=12 # define a benefit
CC=1 # define a cost
thetas1=0.5*((1/CC)-(1/BB)) # calculate optimal investment
D=0.15 # set a value for transdifferentiation
DD=D
ESSJ=c()
for(i in PP){ # loop through frequencies and calculate strategies
  left=(1-DD)/(CC-(CC*DD*i))
  right=1/(BB*i)
  x=0.5*(left-right)
  ESSJ=append(ESSJ, x)
}

## ESSJ is the allocated investment of a strain
ESSJ[which(ESSJ<0)]=0

## jdone is the realised investsment in 2-player groups
jdone=ESSJ+(DD*(thetas1-(PP*ESSJ)))
## jdone2 is the realised investment in N-player groups
jdone2=ESSJ+(DD*(thetas1-( (PP*ESSJ)+((1-PP)*rev(ESSJ)) )))

# normalize values, and plot
ESSJ=(1-thetas1)/(1-ESSJ)
jdone=(1-thetas1)/(1-jdone)
jdone2=(1-thetas1)/(1-jdone2)
vardf5=data.frame(V1=PP, V2=ESSJ, V3=jdone, V4=jdone2)

pl5 <- ggplot(vardf5, aes(x=V1, y=V2))
pl5 <- pl5 + 
  geom_line(col='blue', size=2)+
  geom_line(data=vardf5, aes(x=V1, y=V3), col='red', size=2) +
  geom_line(data=vardf5, aes(x=V1, y=V4), col='black', size=2) +
  ylim(0, 1) +
  xlim(0, 1)+
  xlim(0,0.25)+
  ggtitle('A')+
  labs(x='Relatedness',y='Stalk Investment') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title=element_text(hjust=0)) +
  theme(aspect.ratio=1) + 
  theme(text = element_text(size=18))+
  scale_y_continuous(breaks=c(0.5416666, 0.541666+(0.458333/2)), labels=c(0, expression(paste(frac(1,2), theta))), limits=c(0.54,0.78))+
  geom_segment(aes(x = 0.17, y = 0.64, xend = 0.18, yend = 0.64), col='blue', size=1.5) +
  annotate(geom="text", x=0.215, y=0.64, label="Underlying",color='blue', size=4)+
  geom_segment(aes(x = 0.17, y = 0.62, xend = 0.18, yend = 0.62), col='red', size=1.5) +
  annotate(geom="text", x=0.21, y=0.62, label="N Strains",color='red', size=4)+
  geom_segment(aes(x = 0.17, y = 0.60, xend = 0.18, yend = 0.60), col='black', size=1.5) +
  annotate(geom="text", x=0.21, y=0.60, label="2 Strains",color='black', size=4)
pl5

