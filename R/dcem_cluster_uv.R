#The main EM routine.
require(mvtnorm)
require(matrixcalc)

#' dcem_cluster_uv (univariate data): Part of DCEM package.
#'
#' Implements the Expectation Maximization algorithm for the univariate data. This function is internally
#' called by the dcem_train routine.
#'
#' @param data (matrix): The dataset provided by the user (converted to matrix format).
#'
#' @param meu (vector): The vector containing the initial meu.
#'
#' @param sigma (vector): The vector containing the initial standard deviation.
#'
#' @param prior (vector): The vector containing the initial prior.
#'
#' @param num_clusters (numeric): The number of clusters specified by the user. Default is 2.
#'
#' @param iteration_count (numeric): The number of iterations for which the algorithm should run. If the
#' convergence is not achieved then the algorithm stops.
#' Default: 200.
#'
#' @param threshold (numeric): A small value to check for convergence (if the estimated meu(s)
#' are within the threshold then the algorithm stops).
#'
#' \strong{Note: Choosing a very small value (0.0000001) for threshold can increase the runtime
#' substantially and the algorithm may not converge. On the other hand, choosing a larger
#' value (0.1) can lead to sub-optimal clustering. Default: 0.00001}.
#'
#' @param num_data (numeric): The total number of observations in the data.
#'
#' @param numcols (numeric): Number of columns in the dataset (After processing the
#' missing values).
#'
#'
#' @return
#'         A list of objects. This list contains parameters associated with the
#'         Gaussian(s) (posterior probabilities, meu, standard-deviation and prior)
#'
#'\enumerate{
#'         \item (1) Posterior Probabilities: \strong{prob}: A matrix of
#'         posterior-probabilities.
#'
#'         \item (2) Meu(s): \strong{meu}: It is a vector of
#'         meu. Each element of the vector corresponds to one meu.
#'
#'         \item (3) Sigma: Standard-deviation(s): \strong{sigma}: A vector of standard
#'         deviation.
#'
#'         \item (4) prior: \strong{prior}: A vector of prior.
#'         }
#'
#' @usage
#' dcem_cluster_uv(data, meu, sigma, prior, num_clusters, iteration_count,
#' threshold, num_data, numcols)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.
#'
#' @references
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#' (2016) <https://doi.org/10.1007/s41060-017-0062-1>.

dcem_cluster_uv <-

  function(data,
           meu,
           sigma,
           prior,
           num_clusters,
           iteration_count,
           threshold,
           num_data,
           numcols)

  {
    counter = 1
    t_status = TRUE

    weights = matrix(0,
                     nrow = num_clusters,
                     ncol = num_data,
                     byrow = TRUE)
    tolerance <- .Machine$double.eps

    #Repeat till threshold achieved or convergence whichever is earlier.
    while (counter <= iteration_count) {

      old_meu = meu

      # Expectation
      weights = expectation_uv(data, weights, meu, sigma, prior, num_clusters, tolerance)

      # Maximisation
      out = maximisation_uv(data, weights, meu, sigma, prior, num_clusters, num_data)
      meu = out$meu
      sigma = out$sigma
      prior = out$prior

      # Check convergence
      mean_diff = sqrt(sum((meu - old_meu) ^ 2))

      if (!is.na(mean_diff) && round(mean_diff, 4) <= threshold) {
        print((paste("Convergence at iteration number: ", counter)))
        break
      }

      # Check iterations
      else if (counter == iteration_count) {
        print("Maximum iterations reached. Halting.")
        break
      }

      counter = counter + 1
    }

    output = list('prob' = weights, 'meu' = meu, 'sigma' = sigma, 'prior' = prior)
    return(output)

  }
