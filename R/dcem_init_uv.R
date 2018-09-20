#'means_uv: Part of DCEM package.
#'
#' This function is internally called by the dcem_train to initialize the
#' mean(s) for the Gaussian(s). It randomly selects the mean(s) from the
#' range min(data):max(data).
#'
#' @param data (matrix): The dataset provided by the user (converted to matrix format).
#' @param num_means (number): The number of means (meu).
#'
#' @return A vector containing the randomly selected samples from the dataset.
#' The initial means will be updated during the execution.
#'
#' @usage
#' means_uv(data, num_means)
#'
#' @examples
#' # Randomly selecting the samples from a normal distribution as inital mean(s).
#'
#' means_uv(rnorm(100,20,10), 2)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.

means_uv <- function(data, num_means) {
  mean_vector = c(sample(min(data):max(data), num_means))
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
