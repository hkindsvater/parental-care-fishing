
Npop <- 1000   #initial pop size (not important)
Tmax <- 200 #max years
Tfishing <- 100 #when we start fishing
Ngroup <- 2 #Number of separate population groups - IE females,males
Amax <- c(20, 14) #max age across all groups

#Age and growth params from from PFMC stock assessment 2017 

initialsize = c(35, 34) #cm size at "recruitment" 

Linf = c(112.8,81.7)  #cm, average max size at maturity for use in von Bert growth function (VBGF)
k = c(0.145, 0.223)   # VBGF growth coefficients 
c = c(0.0017,0.0040) #mass-at-size coef (in kilograms)   
b = c(3.40, 3.21)   #mass-at-size exp 

prop=c(0.5, 0.5) #initial proportions of each strategy at recruitment

#assuming Beverton Holt recruitment, these are general parameters I adjusted by eye
alpha = 0.1 
beta = 0.00001

natmort = c(0.18, 0.32) #mortality of each group, from PFMC stock assessment

pmat <- array(dim=c(Amax[1],Tmax,Ngroup), data=NA) #this will hold probability of maturation each year, note it is a matrix so could change with population density if we want to consider fisheries-induced changes in maturation over time

#age-dependent maturity from King and Winter 2005
pmat[,,1]<-c(0, 0.25, 0.75,  rep(1, Amax[1]-3)) #this fills pmat with the knife-edge maturation probability at age 2 for all females
pmat[,,2]<-c(0, rep(1, Amax[2]-1), rep(NA, Amax[1]-Amax[2])) #this fills pmat after age 1 with 1, but will be replaced with size dependent maturation probability at age 2 in the loop below
matalpha=c(1.129, 1.240)
matbeta=c(3.814, 3.233)


###Set up vectors 
W <- array(dim=c(Amax[1],Ngroup), data=NA) #this vector will hold individual weight-at-age
L <- array(dim=c(Amax[1]+1,Ngroup), data=NA) #length-at-age
mu <- array(dim=c(Amax[1],Ngroup), data=0) #mortality-at-age (this is constant in initial case)
select <- array(dim=c(Amax[1],Ngroup), data=NA) #probability of being caught at each age, given size-at-age
Fishing <- array(dim=c(Amax[1],Ngroup), data=0) #Fishing mortality at age

N <- array(dim=c(Amax[1],Tmax,Ngroup), data=0)  #this matrix holds population numbers in each year class over time

E <- array(dim=c(1,Tmax), data=0)  #holds number of eggs at each time
P <- array(dim=c(1,Tmax), data=0)  #holds number of larvae at each time
Catch <- array(dim=c(Amax[1],Tmax,Ngroup), data=0)  #Catch  given selectivity

#Age-dependent size, maturation, and mortality 
#we first need to set initial conditions and define the relationships between age, size, maturation, mortality, and fishery selectivity. In this simple case, we are assuming asymptotic selectivity based on length, not mass.

for(g in 1:Ngroup) {
  
  N[1,1,g] = Npop*prop[g] #initial population size
  
  L[1,g] = initialsize[g] #initial size when entering the population model 
  
  #Next define age-specific growth, mortality, and selectivity functions 
  
  for (a in 1:(Amax[1])) {
    if (a <= Amax[g]) { #this if statement allows us to vary maximum age of each sex 
      
      #GROWTH 		 
      L[a+1,g]=Linf[g]*(1-exp(-k[g])) + (L[a,g])*exp(-k[g]) #length at age
      W[a,g]=  c[g]*L[a,g]^b[g] #weight at age
      
      ##AGE DEPENDENT MATURATION
      pmat[a,,g] <- 1/(1+exp(-matalpha[g]*(a-matbeta[g])))
      
      #FISHERY SELECTIVITY
      if(slot == "YES")    {
        #SLOT:
        if (L[a, g] > 66.04 & L[a,g] < 91.44 )  select[a,g] = 1 else select[a,g] = 0  # Slot between 26 and 36 inches based on Length-at-age
      } else {
        # # MINIMUM SIZE: 
        if (L[a, g] > 66.04 )  select[a,g] = 1 else select[a,g] = 0  #Minimum size at 66 cm
      } # end slot 
      
    } #endif
    
    
  }#end a loop
}#next group

#     
eggs <- 2.82e-4*W[, 1]^3.0011  #estimate of mass-specific egg production from PFMC 2017 assessment

