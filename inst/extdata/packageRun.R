require(bio.lobster)
require(devtools)
load_all(file.path(git.repo,'lobsterCatch'))


#initialize a parameter file to pass info into the code and then put all into a function
p = list()

p$nrowgrids = 10
p$ncolgrids = 10
p$ngrids = p$nrowgrids * p$ncolgrids
p$initlambda = 8 # Initial density of lobster
p$initD = 3  #Initial Dispersion of lobster (initlambda and initD go into rpoissonD to randomly allocation lobster across the grid space)
p$shrinkage = 0.993
#p$initlambda = 0.2 #is the density of lobsters at the beginning of simulation
#p$initD = 3 #is the dispersion index of lobsters on seabed at the beginning of the simulation
p$currentZoI = 15
p$radiusOfInfluence = 15
p$Trap = data.frame( x = c(3,5,6), y = c(3,5,6) )
p$ntraps = nrow(p$Trap)
p$saturationThreshold = 5
p$howClose = 0.5
p$dStep = 5
p$lengthBased = TRUE
p$lobsterSizeFile <- '~/R/x86_64-pc-linux-gnu-library/4.0/lobsterCatch/extdata/LobsterSizeFreqs.csv' 
#p$lobsterSizeFile <- 'LobsterSizeFreqs.csv'
p$lobLengthThreshold = 115
p$trapSaturation = TRUE
p$q0 = 0.5
p$qmin = 0
p$realizations = 20 #number of iterations/simulations
p$tSteps = 5       #timesteps per iteration
p$sexBased <- TRUE
# The following lines creates a sex distribution
p$lobsterSexDist <- list(labels = c('M','F','MM','BF'), #male, female, mature male, berried female
                         prob1 = c(0.55,0.35,0.05,0.05), #their prob in population
                         prob2 = c(0.5,0.50,0,0), # prob of small males and females that are under lobsterMatThreshold
                         lobsterMatThreshold = 100  # The average size of mature lobsters
)
# p$lobsterSexDist <- ''  # in case of p$sexBased = FALSE

TrialSim <- SimulateLobsterMovement(p)

Results  <- GetSimOutput(TrialSim)


## Changing Shrinkage factor
# Folder: ImpaceofShrinkageOnMean
# This folder saves all simulated data, parameters, and plots to compare mean of catch at
# each shrinkage parameter.
##

shrinkage = seq(0.95, 1, by =0.01)
meanCatch = list()
meanDispersion = list()
for(j in 1:length(shrinkage)){

  print(shrinkage[j])
  p$shrinkage = shrinkage[j]

  TrialSim <- SimulateLobsterMovement(p)
  Results  <- GetSimOutput(TrialSim)


  meanCatch[[j]]      = mean( apply(X = Results$MaxCatch, MARGIN = 2, FUN = mean) )

  if( ncol(Results$MaxCatch) == 1 ){
    meanDispersion[[j]] = mean( apply(X = Results$MaxCatch, MARGIN = 2, FUN = dispersion) )
  }

  if( ncol(Results$MaxCatch) > 1 ){
    meanDispersion[[j]] = mean( apply(X = Results$MaxCatch, MARGIN = 1, FUN = dispersion) )
  }

}

sim_number <- 2

if( !dir.exists( paste0('ImpactofShrinkageOnMean/sim', sim_number) ) ) {
  dir.create( path = (paste0('ImpactofShrinkageOnMean/sim', sim_number)) )
}

png(filename = paste0('ImpactofShrinkageOnMean/sim',sim_number,'/ImpaceofShrinkageOnMeanCatch.PNG'), width = 800, height = 600 )
plot(x = shrinkage,
     y = meanCatch,
     type = 'b',
     xlab = 'Shrinkage Factor',
     ylab = 'Mean of Catch')
dev.off()

png(filename = paste0('ImpactofShrinkageOnMean/sim',sim_number,'/ImpactofShrinkageOnDisperssion.PNG'), width = 800, height = 600 )
plot(x = shrinkage,
     y = meanDispersion,
     type = 'b',
     xlab = 'Shrinkage Factor',
     ylab = 'Mean of Disperssion')
dev.off()


save(meanCatch,
     meanDispersion,
     p,
     shrinkage,
     file = paste0('ImpactofShrinkageOnMean/Sim',sim_number,'/all.RData')
)








## Changing Lambda
# Folder: ImpactofDensityOnMean
# This folder saves all simulated data, parameters, and plots to compare mean of catch at
# each lambda parameter.
##

lambda = c(0.1, 0.5, 1, 1.6)
meanCatch = list()
meanDispersion = list()
for(j in 1:length(lambda)){

  print(lambda[j])
  p$initlambda = lambda[j]

  TrialSim <- SimulateLobsterMovement(p)
  Results  <- GetSimOutput(TrialSim)


  meanCatch[[j]]      = mean( apply(X = Results$MaxCatch, MARGIN = 2, FUN = mean) )

  if( ncol(Results$MaxCatch) == 1 ){
    meanDispersion[[j]] = mean( apply(X = Results$MaxCatch, MARGIN = 2, FUN = dispersion) )
  }

  if( ncol(Results$MaxCatch) > 1 ){
    meanDispersion[[j]] = mean( apply(X = Results$MaxCatch, MARGIN = 1, FUN = dispersion) )
  }

}

sim_number <- 1
png(filename = paste0('ImpactofDensityOnMean/sim',sim_number,'/ImpactofDensityOnMeanCatch.PNG'), width = 800, height = 600 )
plot(x = shrinkage,
     y = meanCatch,
     type = 'b',
     xlab = 'Lambda',
     ylab = 'Mean of Catch')
dev.off()

png(filename = paste0('ImpactofDensityOnMean/sim',sim_number,'/ImpactofDensityOnDisperssion.PNG'), width = 800, height = 600 )
plot(x = shrinkage,
     y = meanDispersion,
     type = 'b',
     xlab = 'Lambda',
     ylab = 'Mean of Disperssion')
dev.off()


save(meanCatch,
     meanDispersion,
     p,
     shrinkage,
     file = paste0('ImpactofDensityOnMean/Sim',sim_number,'/all.RData')
)


