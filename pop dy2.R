library(ggplot2)
library(reshape2)
  #rm(list=ls(all=TRUE))

mu_f = seq(0, 0.7, by=0.1)  #make a vector of total mortality from 0 to 0.7    #mu_f = matrix(nrow=10, ncol=3, data=0.3)
# quartz()
  # par(mfrow=c(2,2))
 
slot = "YES" 
feedback = "NO"

 #source('~/Documents/slot_limits/wrasse parameters.R', chdir = TRUE)
 source('~/Documents/slot_limits/lingcod parameters.R', chdir = TRUE)
  
#################################################################################################################
#For every level of fishing pressure (0-0.7): 
###Simulate population dynamics, start from an arbitrary population size and let the population reach a stable age distribution, then start fishing. The population will reach a new, fished, steady state (stable age dist). *Note:  recruitment is based only on Female abundance, and assuming 50:50 offspring sex ratio.
 SPR=rep(0, length(mu_f))
 Care=rep(0, length(mu_f))
 Lengthchange=rep(0, length(mu_f))
 
Yield = rep(0, length(mu_f))

Maleratio = rep(0, length(mu_f))

Nests = rep(0, length(mu_f))

 SNdepletion = rep(0, length(mu_f))

  NMdepletion = rep(0, length(mu_f))

  Femaledep =   rep(0, length(mu_f))

  for (fish in 1:length(mu_f)) {
  	NM_length <- rep(1, Tmax -1)
for(t in 1:(Tmax-1)) {
	
      E[t]=sum(N[, t,1]*pmat[,t,1]*eggs, na.rm=TRUE) #assuming spawning occurs between 1 t and the next and depends ONLY on mature females
      
      #NM_length[t]  = mean((N[, t, 2]*L[-c(Amax[3]),2] *pmat[, t, 2])   / sum(N[, t, 2]),na.rm=TRUE ) 
      NM_length[t]  = mean((N[, t, 2]*L[-c(Amax[2]),2] *pmat[, t, 2])   / sum(N[, t, 2]),na.rm=TRUE ) 
      
      Nests[t]=100*(NM_length[t])^1.25
      
  if (feedback=="YES" & t > Tfishing) {
      P[t] = E[t]* ((Nests[t]/E[t])  / (Nests[Tfishing-1]/E[Tfishing-1] ))  } else   P[t] = E[t]        
     
  for(g in 1:Ngroup) {
      
         
 N[1,t+1,g]= alpha*P[t]/(1+beta*P[t])*prop[g] #this is the N_0 class that is born and recruits to the population model in the next time step... 
       
         # calculate  probability of fishing mortality
    age <- 1
    for (age in 1:(Amax[g]-1)) {  
    
      if (Tfishing < t) {
      	
        Fishing[age,g] = select[age,g]*mu_f[fish]
        Fishing[Amax[g], g] = select[Amax[g], g]*mu_f[fish] 
        
      } else {
        Fishing[age,g] = 0
      } #end if
    
      
      Catch[,t+1,g] <- N[,t,g]*(1-exp(-Fishing[,g]))  #Note this is catch numbers, not biomass, assumes natural mortality occurs later in the year than fishing
              
      N[age+1,t+1,g] <- N[age,t,g]*exp(-natmort[g]-Fishing[age,g]) #surviving fish in each group enter the next age class in the following year, all fish get to spawn before mortality 		 
         
    } #end second age loop
  } #next group
} #end t loop


SPR[fish] = P[Tfishing+10]/P[Tfishing-1]

Care[fish] = (Nests[Tfishing+10]/E[Tfishing+10])  / (Nests[Tfishing-1]/E[Tfishing-1] )


Yield[fish] = sum(N[,Tfishing+10,]*(1-exp(-Fishing[,])))
  # # Maleratio[fish] = sum(N[, Tfishing+10, 2] *pmat[, Tfishing+10,2])/sum(N[, Tfishing+10, 3]*pmat[, Tfishing+10,3], na.rm=TRUE)

 #OSR[fish] = sum(N[, Tfishing+10, 1] *pmat[, Tfishing+10,2])/sum(N[-1, Tfishing+10, 2])  
  #Lengthchange[fish] = avg_male_length[Tfishing+10]/unfished_avg
  #SNdepletion[fish] = sum(N[, Tfishing+10, 3]) #sum(N[, Tfishing+10, 3]*pmat[, Tfishing+10, 3], na.rm=TRUE) 
  NMdepletion[fish] = sum(N[, Tfishing+10, 2]) #sum(N[, Tfishing+10, 2]*pmat[, Tfishing+10, 2],na.rm=TRUE) 
 Femaledep[fish] = sum(N[, Tfishing+10, 1]) 
} #end fish loop


  
# ##Plot these relationships
#    # #Age-length
#  
#  matplot(L[-(Amax+1), ], type="l", lwd=2, las=1, ylab ="Length", xlab = "Age", col=c('black','blue', 'red'), lty=1, 
#        ylim=c(min(L[,3]), max(L[,2])), xlim=c(0, Amax[3]))
#  # #       
#        # matplot(L[-(Amax+1), ], type="l", lwd=2, las=1, ylab ="Length", xlab = "Age", col=c('black','blue'), lty=1,
#       # ylim=c(min(L[,1]), max(L[,1])), xlim=c(0, Amax[1]))
# 
#   # # ###for wrasse  & bluegill
#  legend('bottomright', legend=c('Female','Territorial Male', 'Sneaker Male'),  lty=1, col=c('black','blue', 'red'), cex=0.65)
# 
#  # # ###for lingcod
#  # legend('bottomright', legend=c('Female','Territorial Male'),  lty=1, col=c('black','blue'), cex=0.65)
# 
# 
# # # #Maturation ogives
#  
#  matplot(pmat[,10,], type="l", lwd=2, las=1, lty=1, col=c('black','blue', 'red'),  ylab ="Probability Mature", xlab = "Age",
#       ylim=c(0, 1), xlim=c(0, max(Amax)))
# # ##mat ogive for lingcod
# # matplot(pmat[,10,], type="l", lwd=2, las=1, lty=1, col=c('black','blue' ),  ylab ="Probability Mature", xlab = "Age",
#       # ylim=c(0, 1), xlim=c(0, max(Amax)))
# 
#   legend('bottomright', legend=c('Female','Territorial Male', 'Sneaker Male'),  lty=1, col=c('black','blue', 'red'), cex=0.65) 
#         
# # plot(eggs, type="l", lwd=2, lty=1, col='black', las=1, ylab="", xlab="Age")  
# matplot((select), type="l", lwd=2, lty=3, col=c('black','blue', "red"),las=1, ylab ="Selectivity", xlab ="Age",  ylim=c(0,1), xlim=c(0,  max(Amax)))
  
  

 # quartz()
 
# # plot(colSums(N[,-1,1]), type="l", lwd=3,   ylab="Abundance", xlab="Time", col='black',
     # ylim=c(0, max(colSums(N[,,1], na.rm=TRUE))))
	# lines(colSums(N[,-1,2]), lwd=3, col='blue')
	  # lines(colSums(N[,-1,3]), lwd=3, col='red')
  # legend('bottomright', legend=c('Female','Territorial Male', 'Sneaker Male'),  lty=1, lwd=2, col=c('black','blue', 'red'), cex=1, 		bty='n')
 	
 	###for lingcod
 #legend('bottomright', legend=c('Female','Territorial Male'),  lty=1, col=c('black','blue'), cex=0.65)


############################
#####AGE STRUCTURE 
##################################

   # AgeMat<- rbind(N[,Tfishing-1 , 1]*pmat[,Tfishing-1, 1], N[,Tfishing-1, 2]*pmat[,Tfishing-1, 2], N[,Tfishing-1, 3]*pmat[,Tfishing-1, 3]) 
  
   # males<-colSums(AgeMat[2:3, ], na.rm=TRUE) #get numbers of mature males by age
   # females<-AgeMat[1, ]  #get numbers of females by age
    # dat=cbind(females, males)
    # dat2<-melt(dat, varnames=c("Age", "Sex"))
    
  
  # FAgeMat<- rbind(N[,Tmax-1 , 1]*pmat[,Tmax-1, 1], N[,Tmax-1, 2]*pmat[,Tmax-1, 2], N[,Tmax-1, 3]*pmat[,Tmax-1, 3]) 
  
   # Fmales<-colSums(FAgeMat[2:3, ], na.rm=TRUE) #get numbers of males by age
   # Ffemales<-FAgeMat[1, ]  #get numbers of females by age
    # fdat=cbind(Ffemales, Fmales)
    # fdat2<-melt(fdat, varnames=c("Age", "Sex"))


# #  ###for LINGCOD
# AgeMat<- rbind(N[,Tfishing-1 , 1]*pmat[,Tfishing-1, 1], N[,Tfishing-1, 2]*pmat[,Tfishing-1, 2]) 
  
   # males<-AgeMat[2,] #get numbers of mature males by age
   # females<-AgeMat[1, ]  #get numbers of females by age
    # dat=cbind(females, males)/100
    # dat2<-melt(dat, varnames=c("Age", "Sex"))
    
  
  # FAgeMat<- rbind(N[,Tmax-1 , 1]*pmat[,Tmax-1, 1], N[,Tmax-1, 2]*pmat[,Tmax-1, 2]) 
  
   # Fmales<-FAgeMat[2, ]#get numbers of males by age
   # Ffemales<-FAgeMat[1, ]  #get numbers of females by age
    # fdat=cbind(Ffemales, Fmales)/100
    # fdat2<-melt(fdat, varnames=c("Age", "Sex"))

fishing=seq(0,0.7,0.1)
   
 quartz()
par(mar=c(5,5,4,6)+0.1) 
    barplot(Yield, names.arg=fishing, ylab="Yield  (numbers)", xlab="Fishing mortality", las=1, ylim=c(0, 3000))
#  par(new=T, par(mar=c(5,5,4,6)+0.1))
plot(fishing, SPR, ylab="",  xlab="Fishing mortality", las=1, 
ylim=c(0, 1), type="b", pch=15, col="blue", lty=2)
  
 par(new=T, par(mar=c(5,5,4,6)+0.1))
#plot(Care, ylab="", xlab="", ylim=c(0, 1), type="b", pch=20, col="red", lty=2, xaxt="n",yaxt="n")
plot(Care, ylab="", xlab="", ylim=c(0, 20),  type="b", pch=20, col="red", lty=2, xaxt="n",yaxt="n")
axis(4,line=0,labels=seq(0, 25, 5), at=seq(0, 25, 5), col="red",col.axis="red", las=1)

 #axis(4,line=3,  
  mtext("Spawning Potential Ratio",side=2,line=3, col="blue")
 # #mtext("NM:SN ratio", side=4, line=4, col="red")
  mtext("Care capacity", side = 4, line = 2, col="red" )
 # 
 Care
 
 # par(new=T, par(mar=c(5,5,4,6)+0.1))  
 # plot(Maleratio, ylab="", xlab="",  ylim=c(0, 1), type="b", pch=19, col="red", lty=1, xaxt="n",yaxt="n")
 # # # barplot(rbind(SNdepletion, NMdepletion), col=c(3, 5), ylim=c(0, 1.05), names.arg=1:Amax[2], xlab = "Age", beside=TRUE)
 # par(new=T, par(mar=c(5,5,4,6)+0.1)) 
 # plot(Lengthchange, ylab="", xlab="", ylim=c(0, 1), type="b", pch=19, col="green", lty=1, xaxt="n",yaxt="n")
  
# ###Unfished age structure (all mature fish)]
 
# #       g2=ggplot(dat2, aes(Age)) + coord_flip() +  theme_bw()   +
     # geom_bar(data =  dat2[dat2[["Sex"]]=="females",],
         # aes(y = -value, fill="blue"), stat="identity")   + geom_bar(data =  dat2[dat2[["Sex"]]=="males",], aes(y = value, fill="red"), stat="identity") + ylim(-750, 750) 
         
   # g2+theme(legend.position="none")

       
# ####PLOT FISHED AGE STRUCTURE AT EQUILIBRIUM (note this is ALL fish, not just mature fish)
    
# #  
  # g3=ggplot(fdat2, aes(Age)) + coord_flip() +  theme_bw()   +   geom_bar(data =  fdat2[fdat2[["Sex"]]=="Ffemales",],
         # aes(y = -value, fill="blue"), stat="identity")   + geom_bar(data = fdat2[fdat2[["Sex"]]=="Fmales",], aes(y = value, fill="red"), stat="identity") + ylim(-900, 900) 
                    
   # g3+theme(legend.position="none")
      
  # # ####if min size yield and MR are saved
    if (slot == "NO") {
   minsizeY <- Yield
  # minsizeMR <- Maleratio
   # minsizeSPR <- SPR
   
    }
   
    deltaY <- (minsizeY-Yield)/minsizeY
        plot(fishing[-1], deltaY[-1]*100, type="b", pch=15, ylab="% change  with slot", las=1, xlab="Fishing Mortality", ylim=c(0, 100))

       # for lingcod
       # deltaSPR <- (SPR - minsizeSPR)/minsizeSPR
     # lines(fishing[-1], deltaSPR[-1]*100, type="l", col=4, lwd=2.5)  
        

 #deltaMR <- (Maleratio-minsizeMR)/minsizeMR


# ###if slotNMdepletion and slotSNdepletion are saved
# if(slot =="YES") {
# 	slotSN <- SNdepletion
# 	slotNM <- NMdepletion
# 	
# }


# #    malefreq = rbind(SNdepletion,slotSN, NMdepletion,  slotNM)/max(slotSN)

  # barplot(malefreq, col=c("gray20", "gray50", "gray75",  "white"),  names.arg=fishing, xlab = "Fishing mortality", ylab="Frequency of Male Type", beside=TRUE, ylim=c(0, 2))
 # legend("topright", legend=c("Sneakers with min size",  "Sneakers with slot", "Nesting Males with min size","Nesting Males with slot") ,  pch=c(15, 15, 15, 22), col=c("gray20", "gray50","gray75",  "black"), bty="n")


  # ###if slotNMdepletion and Femaledepletion are saved
   # OSR = rbind(Femaledep,slotFemdep, NMdepletion,  slotNMdepletion)/max(Femaledep)

  # barplot(OSR, col=c("gray20", "gray50", "gray75",  "white"),  names.arg=fishing, xlab = "Fishing mortality", ylab="Frequency of each sex", beside=TRUE, ylim=c(0, 1.2))
   # legend("topright", legend=c("Females with min size",  "Females with slot", "Males with min size","Males with slot") ,  pch=c(15, 15, 15, 22), col=c("gray20", "gray50","gray75",  "black"), bty="n")
# 
# # matplot(fishing, t(sprdat),type="l", col = c("yellow", 1, 3, 4), lty=1, lwd=c(8, 2, 2, 2), ylab = "Spawning Potential Ratio", xlab="Total mortality rate")
# legend("topright", legend = c("Min size limit, no feedback", "Slot limit, no feedback", "Slot limit, feedback", "Min size limit, feedback"), col = c(1, "yellow",  4, 3), lty=1, lwd=c(2, 6, 2, 2), cex=.65, bty="n")

  # barplot(as.matrix(Ydat[, -1]), beside=TRUE, names.arg=fishing,  col = c(1, "yellow",  4, 3), ylab="Yield  (numbers)", xlab="Total mortality rate", las=1, ylim=c(0, 2500))
   # legend("topleft", legend = c("Min size limit, no feedback", "Slot limit, no feedback", "Min size limit, feedback", "Slot limit, feedback"), col = c(1, "yellow",  4, 3), lty=1, lwd=c(2, 3, 2, 2), cex=.65, bty="n")
  # #  
   
   # #par(new=T, par(mar=c(5,5,4,6)+0.1))
 