#' This function calculates the probability of entry into a trap, also known as catchability. It includes the parameters described
#' in Addison and Bell (1997), and can also incorporate the length of the catch while calculating the catchability.
#' @param q0 is the initial probability of entry into an empty trap (range is from 0-1). Default value is 0.5.
#' @param qmin is the asymptotic minimum probability of entry with default value being 0.
#' @param saturationThreshold is the number of lobsters in a trap at which the probability of
#' another lobster entering the trap is zero (i.e. no more entry due to agnostic  behavior of trapped lobsters).
#' @param Ct is the number of caught lobster
#' @param lengthBased Logical. If TRUE the length of lobsters caught will be taken into account
#' @param lobLengthThreshold Logical.If TRUE the carapace length (in milliliters) beyond which there is no chance of catching another lobster due to bold agnostic  behavior of large lobsters.
#' @param lobSize is a size frequency dataset that is representative of the population and can be incorporated to the model.
#' @param sexBased Logical. If TRUE, lobster sex is taken into account and user must provide a list containing sex distribution for the simulated population
#' @param lobSex is the sex of trapped lobster
#' @references Julian T. Addison and Michael C. Bell (1997), Simulation modelling of capture processes in trap fisheries for clawed
#' lobsters, Marine Freshwater Research, 48(8), 1035-1044, https://www.publish.csiro.au/MF/MF97169
#' @return Returns the probability of entry to trap.

catchability <- function(q0, qmin, saturationThreshold, Ct, lengthBased, lobLengthThreshold, lobSize = NA, sexBased, lobSex){


  if( (lengthBased == FALSE) & (sexBased == FALSE) ){

    # r is the instantaneous rate of change in q0 with respect to the number of lobsters that are trapped (Ct).
    r  <- (log(0.01) - log(q0 - qmin))/(-saturationThreshold)
    q  <- ( (q0 - qmin) / exp(r*Ct) ) + qmin
    return(q)
  }

  if( (lengthBased == FALSE) & (sexBased == TRUE) ){

    temp2 <- unlist( strsplit( lobSex, split = '-' ) )
    temp2 <- temp2[2:length(temp2)]

    if( any(temp2 %in% 'BF') ){
      q = 0
      return(q)
    }else{
      r = (log(0.01) - log(q0 - qmin))/(-saturationThreshold)
      q = (q0-qmin) / exp(r*Ct) + qmin
      return(q)
    }
  }

  if( (lengthBased == TRUE) & (sexBased == FALSE) ){

    temp <- unlist( strsplit( lobSize, split = '-' ) )
    temp <- temp[2:length(temp)]
    temp <- as.numeric(temp)

    if( any(temp > lobLengthThreshold, na.rm = TRUE) ){
      q = 0
      return(q)
    }else{
      r = (log(0.01) - log(q0 - qmin))/(-saturationThreshold)
      q = (q0-qmin) / exp(r*Ct) + qmin
      return(q)
    }


  }

  if( (lengthBased == TRUE) & (sexBased == TRUE) ){

    temp <- unlist( strsplit( lobSize, split = '-' ) )
    temp <- temp[2:length(temp)]
    temp <- as.numeric(temp)

    temp2 <- unlist( strsplit( lobSex, split = '-' ) )
    temp2 <- temp2[2:length(temp2)]


    if( (any(temp > lobLengthThreshold, na.rm = TRUE) | ( any(temp2 %in% 'BF') ) ) ){
      q = 0
      return(q)
    }else{
      r = (log(0.01) - log(q0 - qmin))/(-saturationThreshold)
      q = (q0-qmin) / exp(r*Ct) + qmin
      return(q)
    }

  }

}
