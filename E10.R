rm(list = ls())
library(ggplot2)
#Defines parameters for the model in the Untreated Scenario
Nt = 100
Mt = 1
rn = .1
rm = .1
rmt = .05
rnt = -.1
K = 1000000
timesteps = 500

#Builds vectors to store data
Ns = numeric(length = timesteps)
Ns[1] = Nt
Ms = numeric(length = timesteps)
Ms[1] = Mt

#Loops through timesteps to simulate dynamics of cancer cell growth
for(i in 1:(timesteps-1)){
 #Brings populations to equilibirum
   if(i<175){
    Ns[i+1]=Ns[i]+rn*Ns[i]*(1-(Ns[i]+Ms[i])/K)
    Ms[i+1]=Ms[i]+rm*Ms[i]*(1-(Ns[i]+Ms[i])/K)
   }
  #Impact of treatment on populations
  else{
    Ns[i+1]=Ns[i]+rnt*Ns[i]*(1-(Ns[i]+Ms[i])/K)
    Ms[i+1]=Ms[i]+rmt*Ms[i]*(1-(Ns[i]+Ms[i])/K)
  }
}

#Generates a plot of the data

simNn<-data.frame(time=1:length(Ns),N=Ns)
simMn<-data.frame(time=1:length(Ms),N=Ms)

ggplot() +
  geom_line(data = simNn, aes(x = time, y = N, color = "Wild-type"), size = 1.5) + 
  geom_line(data = simMn, aes(x = time, y = N, color = "Mutant"), size = 1.5) + 
  theme_classic() + xlab("Time") + ylab("Number of Cells") + labs(color = "Cell Type")
