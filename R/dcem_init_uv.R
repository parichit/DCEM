#'meu_uv: Part of DCEM package.
#'
#' This function is internally called by the dcem_train to initialize the
#' meu(s). It randomly selects the meu(s) from the
#' range min(data):max(data).
#'
#' @param data (matrix): The dataset provided by the user.
#' @param num_meu (number): The number of meu.
#'
#' @return A vector containing the selected samples from the dataset.
#'
#' @usage
#' # Randomly seeding the meu.
#' meu_uv(data, num_meu)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.

meu_uv <- function(data, num_meu) {
  meu_vector = c(sample(min(data):max(data), num_meu))
  return(meu_vector)
}

#'meu_uv_impr: Part of DCEM package.
#'
#' This function is internally called by the dcem_train to initialize the
#' meu(s). It uses the proposed implementation from
#' K-means++: The Advantages of Careful Seeding, David Arthur and Sergei Vassilvitskii.
#' URL http://ilpubs.stanford.edu:8090/778/1/2006-13.pdf.
#'
#' @param data (matrix): The dataset provided by the user.
#' @param num_meu (number): The number of meu.
#'
#' @return A vector containing the selected samples from the dataset.
#'
#' @usage
#' # Seeding the meu using the K-means++ implementation.
#' meu_uv_impr(data, num_meu)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.

meu_uv_impr <- function(data, num_meu){

  meu_vector = c()
  counter = 0
  total = 0


  #Selecting the first centroid in a uniform manner
  set.seed(1005)
  meu_vector = c(meu_vector, data[sample(1:length(data), 1)])

  #Increase the var to track the selected centroids
  counter = counter + 1

  while (counter < num_meu){

    dist_vector <- c()
    counter = counter + 1

    #Starting the probability calculations for selecting the next set of centroids
    for (row in 1:length(data)){
      for(srow in 1:counter){
        dist = sum((data[srow] - meu_vector[counter])^2)
        dist_vector = c(dist_vector, dist)
        total = total + dist
      }
    }

    dist_vector  = dist_vector / total
    meu_vector = c(meu_vector, data[counter])

  }

  return(meu_vector)
}

#'sigma_uv: Part of DCEM package.
#'
#' Initializes the standard deviation for the Gaussian(s).
#'
#' @param data (matrix): The dataset provided by the user.
#' @param num_sigma (number): Number of sigma (standard_deviations).
#'
#' @return
#'         A vector of standard deviation value(s).
#'
#' @usage
#' sigma_uv(data, num_sigma)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work was partially supported by NCI Grant 1R01CA213466-01.

sigma_uv <- function(data, num_sigma) {
  cov_vec = rep(sd(data), num_sigma)
  return(cov_vec)
}
