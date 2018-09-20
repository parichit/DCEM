#'means_mv: Part of DCEM package.
#'
#' Initialize the mean(s) for the Gaussian(s) by randomly selecting
#' the samples from the dataset.
#'
#' @param data (matrix): The dataset provided by the user (converted to matrix format).
#' @param num_means (numeric): The number of means (meu).
#'
#' @return A matrix containing the randomly selected samples from the dataset. The initial
#' means will be updated during the iterations of the algorithm.
#'
#' @usage
#' means_mv(data, num_means)
#'
#' @examples
#' # Generate random samples from a multivariate distribution.
#' sample_data = MASS::mvrnorm(n=10, rep(10,5), Sigma = diag(5))
#'
#' # Get the mean(s) from the data
#' means_mv(sample_data, num_means=2)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work was partially supported by NCI Grant 1R01CA213466-01.

means_mv <- function(data, num_means) {
  mean_matrix = data[sample(1:nrow(data), num_means),]
  return(mean_matrix)
}

#'cov_mv: Part of DCEM package.
#'
#' Initializes the co-variance matrices as the identity matrices for the Gaussian(s).
#' The list will simply contain one co-variance matrix per Gaussian. If the user specifies 3 cluster(s) then
#' there will be 3 entries in the list.
#'
#' The dimensions of each matrix will be numcol * numcol where numcol is the number of columns in the
#' dataset.
#'
#' @param num_cov (numeric): Number of covariance matrices corresponding to the cluster(s).
#' @param numcol (numeric): The number of columns in the dataset.
#'
#' @return
#'         A list of identity matrices. The number of entries in the list
#'         is equal to the input parameter (num_cov).
#'
#'         The elements of the list can be accessed as - list[[1]] or list[[2]].
#'
#' @usage
#' cov_mv(num_cov, numcol)
#'
#' @examples
#' # Genrating the Identity matrix as the co-variance matrix.
#'
#' cov_mv(2, 3)
#' cov_mv(10, 100)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.

cov_mv <- function(num_cov, numcol) {
  i = 1
  cov_vec = list()
  while (i <= num_cov) {
    cov_vec[[i]] <- diag(numcol)
    i = i + 1
  }
  return(cov_vec)
}

#'priors: Part of DCEM package.
#'
#' Initializes the prior vectors for the Gaussian(s)
#'
#' For example, if the user specify 2 priors then the vector will have 2
#' entries (one for each cluster) where each will be 1/2 or 0.5.
#'
#' @param num (numeric): Number of priors.
#'
#' @return
#' A vector of uniformly initialized prior values (numeric).
#'
#' @usage
#' priors(num)
#'
#' @examples
#' #Randomly generating the priors.
#'
#' priors(2)
#' priors(3)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work was partially supported by NCI Grant 1R01CA213466-01.

#Initialize the priors
priors <- function(num) {
  prior_vec = rep(1/num, num)
  return(prior_vec)
}
