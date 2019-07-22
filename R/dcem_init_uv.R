#'means_uv: Part of DCEM package.
#'
#' This function is internally called by the dcem_train to initialize the
#' mean(s) for the Gaussian(s). It randomly selects the mean(s) from the
#' range min(data):max(data). This is the \strong{default} method for initializing
#' the means(s).
#'
#' @param data (matrix): The dataset provided by the user (converted to matrix format).
#' @param num_means (number): The number of means (meu).
#'
#' @return A vector containing the selected samples from the dataset.
#' The initial means will be updated during the execution.
#'
#' @usage
#' # Randomly seeding the mean(s).
#' means_uv(data, num_means)
#'
#' @examples
#' # Randomly selecting the samples from a normal distribution as inital mean(s).
#' means_uv(rnorm(100,20,10), 2)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.
#'
means_uv <- function(data, num_means) {
  mean_vector = c(sample(min(data):max(data), num_means))
  return(mean_vector)
}

#'means_uv_impr: Part of DCEM package.
#'
#' This function is internally called by the dcem_train to initialize the
#' mean(s) for the Gaussian(s). It uses the proposed implementation from
#' K-means++: The Advantages of Careful Seeding, David Arthur and Sergei Vassilvitskii.
#' URL http://ilpubs.stanford.edu:8090/778/1/2006-13.pdf.
#'
#' @param data (matrix): The dataset provided by the user (converted to matrix format).
#' @param num_means (number): The number of means (meu).
#'
#' @return A vector containing the selected samples from the dataset.
#' The initial means will be updated during the execution.
#'
#' @usage
#'
#' # Seeding the means using the K-means++ implementation.
#' means_uv_impr(data, num_means)
#'
#' @examples
#' # Selecting the inital mean(s) using the non-uniform distance weighting.
#' # k-means++: The Advantages of Careful Seeding, David Arthur
#' # and Sergei Vassilvitskii.
#' # URL http://ilpubs.stanford.edu:8090/778/1/2006-13.pdf.
#' means_uv_impr(rnorm(100,20,10), 2)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.

means_uv_impr <- function(data, num_means){

  mean_vector = c()
  counter = 0
  total = 0


  #Selecting the first centroid in a uniform manner
  mean_vector = c(mean_vector, data[sample(1:length(data), 1)])

  #Increase the var to track the selected centroids
  counter = counter + 1

  while (counter < num_means){

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
    cent = which.max(dist_vector)
    mean_vector = c(mean_vector, data[counter])

    total = 0;

  }

  return(mean_vector)
}


#'sd_uv: Part of DCEM package.
#'
#' Initializes the standard deviation for the Gaussian(s).
#'
#' @param data (matrix): The dataset provided by the user (converted to matrix format).
#' @param num_sd (number): Number of values corresponding to the number of clusters.
#'
#' @return
#'         A vector of standard deviation value(s).
#'
#' @usage
#' sd_uv(data, num_sd)
#'
#' @examples
#' # Standard deviation of a random sample.
#'
#' sd_uv(rnorm(100,20,10), 2)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work was partially supported by NCI Grant 1R01CA213466-01.

sd_uv <- function(data, num_sd) {
  cov_vec = rep(sd(data), num_sd)
  return(cov_vec)
}
