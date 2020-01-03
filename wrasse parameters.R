library(ggplot2)
library(reshape2)
#rm(list=ls(all=TRUE))
Npop <- 1000   #initial pop size (not important)
Tmax <- 200 #max years
Tfishing <- 100 #when we start fishing
Ngroup <- 3 #Number of separate population groups - here three life history tactics 

##note group order is Female, Territorial male, Sneaker male 

Amax <- c(8, 8, 9) #max age across all groups
# 
initialsize = c(81.6, 93.1, 78.3) #mm, size at "recruitment" to the population model in year 1 	 
Linf = c(185.27, 205.76, 179.43 )  #mm, average max size at maturity for use in von Bert growth function (VBGF)
k = c(0.2972, 0.3994, 0.2625)   # VBGF growth coefficients 
c = c(0.0000066,0.0000093, 0.0000085) #mass-at-size coef (in kilograms)   
b = c(3.17576, 3.08885, 3.11409)   #mass-at-size exp 

prop=c(0.5, 0.25, 0.25) #initial proportions of each strategy at recruitment

#assuming Beverton Holt recruitment, these are general parameters I adjusted by eye
alpha = 0.05 
beta = 0.000008

 
Z = c(0.7738, 0.70259, 0.42835) #observed Z scores for females, males, and sneakers, from Halvorsen data but not using right now

natmort = 4.3/Amax #mortality of each group, assumed to be constant across ages (based on Hoenig's method using maximum lifespan)
natmort[3] = 0.4  #adjust sneaker mortality so it is less than Z  


 
pmat <- array(dim=c(Amax[3],Tmax,Ngroup), data=NA) #this will hold probability of maturation each year, note it is a matrix so could change with population density if we want to consider fisheries-induced changes in maturation over time

pmat[,,1]<-c(0, rep(1, Amax[1]-1), NA) #this fills pmat with the knife-edge maturation probability at age 2 for all females
pmat[,,2]<-c(0, rep(1, Amax[2]-1),NA) #this fills pmat with 1, but will be replaced with size dependent maturation probability at age 2 in the loop below
pmat[,,3]<-rep(1, Amax[3]) #this fills pmat with knife-edge maturation at age 1 for all sneakers



###Set up vectors 
W <- array(dim=c(Amax[3],Ngroup), data=NA) #this vector will hold individual weight-at-age
L <- array(dim=c(Amax[3]+1,Ngroup), data=0) #length-at-age
mu <- array(dim=c(Amax[3],Ngroup), data=0) #mortality-at-age (this is constant in initial case)
select <- array(dim=c(Amax[3],Ngroup), data=0) #probability of being caught at each age, given size-at-age
Fishing <- array(dim=c(Amax[3],Ngroup), data=0) #Fishing mortality at age

N <- array(dim=c(Amax[3],Tmax,Ngroup), data=0)  #this matrix holds population numbers in each year class over time

E <- array(dim=c(1,Tmax), data=0)  #holds number of eggs at each time
P <- array(dim=c(1,Tmax), data=0)  #holds number of larvae at each time
Catch <- array(dim=c(Amax[3],Tmax,Ngroup), data=0)  #Catch  given selectivity
 
#Age-dependent size, maturation, and mortality 
#we first need to set initial conditions and define the relationships between age, size, maturation, mortality, and fishery selectivity. In this simple case, we are assuming asymptotic selectivity based on length, not mass.
 
for(g in 1:Ngroup) {
  
  N[1,1,g] = Npop*prop[g] #initial population size
   
  L[1,g] = initialsize[g] #initial size when entering the population model 

    #Next define age-specific growth, mortality, and selectivity functions 
    
    for (a in 1:(Amax[3])) {
    	if (a <= Amax[g]) { #this if statement allows us to vary maximum age of each type (group)
    #GROWTH 		 
    L[a+1,g]= Linf[g]*(1-exp(-k[g])) + (L[a,g])*exp(-k[g]) #length at age
    W[a,g]=  c[g]*L[a,g]^b[g] #weight at age
 
     } #endif
    
             #FISHERY SELECTIVITY
              if(slot == "YES")    {
             #SLOT:
  if (L[a, g] > 120 & L[a,g] < 170 )  select[a,g] = 1 else select[a,g] = 0  # Slot between 120 and 170 mm based on Length-at-age
     } else { 
        # # MINIMUM SIZE: 
         if (L[a, g] > 120)  select[a,g] = 1 else select[a,g] = 0  #Minimum size at 120
            } #end slot if
  }#end a loop
}#next group

   #Adjust group specific traits
   
       pmat[2, ,2] <- 1 - 1/(1+exp(-18.5836 + 0.13361*L[2,2]))  #size-dependent maturation probability of NMs at age 2 
     
  #EGG PRODUCTION
    
    	eggs <- 1000*W[, 1]*295  #estimate of mass-specific egg production (in grams) from a Master's thesis (Chalaris 2011)
    
  
