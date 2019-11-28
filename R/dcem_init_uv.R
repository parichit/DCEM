#'means_uv: Part of DCEM package.
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
#' # Randomly seeding the mean(s).
#' means_uv(data, num_meu)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.

means_uv <- function(data, num_meu) {
  mean_vector = c(sample(min(data):max(data), num_meu))
  return(mean_vector)
}

#'means_uv_impr: Part of DCEM package.
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
#' # Seeding the means using the K-means++ implementation.
#' means_uv_impr(data, num_meu)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.

means_uv_impr <- function(data, num_meu){

  mean_vector = c()
  counter = 0
  total = 0


  #Selecting the first centroid in a uniform manner
  set.seed(1005)
  mean_vector = c(mean_vector, data[sample(1:length(data), 1)])

  #Increase the var to track the selected centroids
  counter = counter + 1

  while (counter < num_meu){

    dist_vector <- c()
    counter = counter + 1

    #Starting the probability calculations for selecting the next set of centroids
    for (row in 1:length(data)){
      for(srow in 1:counter){
        dist = sum((data[srow] - mean_vector[counter])^2)
        dist_vector = c(dist_vector, dist)
        total = total + dist
      }
    }

    dist_vector  = dist_vector / total
    mean_vector = c(mean_vector, data[counter])

  }

  return(mean_vector)
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
#' sd_uv(data, num_sigma)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work was partially supported by NCI Grant 1R01CA213466-01.

sd_uv <- function(data, num_sigma) {
  cov_vec = rep(sd(data), num_sigma)
  return(cov_vec)
}
