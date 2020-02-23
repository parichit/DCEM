#'meu_mv: Part of DCEM package.
#'
#' Initialize the meus(s) by randomly selecting the samples from the dataset. This is the
#' \strong{default} method for initializing the meu(s).
#'
#' @param data (matrix): The dataset provided by the user.
#' @param num_meu (numeric): The number of meu.
#'
#' @return A matrix containing the selected samples from the dataset.
#'
#' @usage
#' # Randomly seeding the mean(s).
#' meu_mv(data, num_meu)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work was partially supported by NCI Grant 1R01CA213466-01.
#' @export
meu_mv <- function(data, num_meu) {
  mean_matrix = data[sample(1:nrow(data), num_meu),]
  return(mean_matrix)
}

#'meu_mv_impr: Part of DCEM package.
#'
#' Initialize the meu(s) by randomly selecting the samples from the dataset. It uses the proposed
#' implementation from K-means++: The Advantages of Careful Seeding, David Arthur and Sergei
#' Vassilvitskii. URL http://ilpubs.stanford.edu:8090/778/1/2006-13.pdf.
#'
#' @param data (matrix): The dataset provided by the user.
#' @param num_meu (numeric): The number of meu.
#'
#' @return A matrix containing the selected samples from the dataset.
#'
#' @usage
#' # Randomly seeding the meu.
#' meu_mv_impr(data, num_meu)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work was partially supported by NCI Grant 1R01CA213466-01.

meu_mv_impr <- function(data, num_meu){

  meu_matrix = matrix(nrow = num_meu,
                       ncol = ncol(data),
                       byrow = TRUE)

  counter = 0

  #Selecting the first centroid in a uniform manner
  meu_matrix[1,] = as.matrix(data[sample(nrow(data), 1),])

  #Increase the var to track the selected centroids
  counter = counter + 1

  while (counter < num_meu){

    dist_vector <- matrix(0, nrow = nrow(data), ncol=1, byrow = TRUE)

    #Starting the probability calculations for selecting the next set of centroids
    for (row in 1:nrow(data)){
      for(srow in 1:counter){
        dist = sum((data[row, ] - meu_matrix[counter, ])^2)
        dist_vector = rbind(dist_vector, dist)
      }
    }

    counter = counter + 1
    meu_matrix[counter, ] = data[counter, ]

  }

  return(meu_matrix)
}

#'sigma_mv: Part of DCEM package.
#'
#' Initializes the co-variance matrices as the identity matrices.
#'
#' @param num_sigma (numeric): Number of covariance matrices.
#' @param numcol (numeric): The number of columns in the dataset.
#'
#' @return
#'         A list of identity matrices. The number of entries in the list
#'         is equal to the input parameter (num_cov).
#'
#' @usage
#' sigma_mv(num_sigma, numcol)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.

sigma_mv <- function(num_sigma, numcol) {
  i = 1
  sigma_vec = list()
  while (i <= num_sigma) {
    sigma_vec[[i]] <- diag(numcol)
    i = i + 1
  }
  return(sigma_vec)
}

#' get_priors: Part of DCEM package.
#'
#' Initialize the priors.
#'
#' For example, if the user specify 2 priors then the vector will have 2
#' entries (one for each cluster) where each will be 1/2 or 0.5.
#'
#' @param num_priors (numeric): Number of priors one corresponding to each cluster.
#'
#' @return
#' A vector of uniformly initialized prior values (numeric).
#'
#' @usage
#' get_priors(num_priors)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work was partially supported by NCI Grant 1R01CA213466-01.

#Initialize the priors
get_priors <- function(num_priors) {
  prior_vec = rep(1/num_priors, num_priors)
  return(prior_vec)
}
