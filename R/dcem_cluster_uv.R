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
#' @param mean_vector (vector): The vector containing the initial means of the Gaussians.
#'
#' @param sd_vector (vector): The vector containing the initial standard deviation for the Gaussians. The initial
#' sd are set to be 1. They are updated during the iterations of the algorithm.
#'
#' @param prior_vec (vector): The vector containing the initial priors for the Gaussians. They are initialized
#' uniformly.
#'
#' @param num (numeric): The number of clusters specified by the user. Default value is 2.
#'
#' @param iteration_count (numeric): The number of iterations for which the algorithm should run. if the
#' convergence is not achieved within the specified threshold then the algorithm stops and exits.
#' Default: 200.
#'
#' @param threshold (numeric): A small value to check for convergence (if the estimated mean(s) are within this
#' specified threshold then the algorithm stops and exit).
#'
#' \strong{Note: Choosing a very small value (0.0000001) for threshold can increase the runtime substantially
#' and the algorithm may not converge. On the other hand, choosing a larger value (0.1)
#' can lead to sub-optimal clustering. Default: 0.00001}.
#'
#' @param numrows (numeric): Number of rows in the dataset (After processing the missing values).
#'
#' @param numcols (numeric): Number of columns in the dataset (After processing the missing values).
#'
#'
#' @return
#'         A list of objects. This list contains parameters associated with the
#'         Gaussian(s) (posterior probabilities, mean, co-variance/standard-deviation and priors)
#'
#'\enumerate{
#'         \item [1] Posterior Probabilities: \strong{sample_out$prob}
#'         A matrix of posterior-probabilities
#'
#'         \item [2] Mean(s): \strong{sample_out$mean}
#'
#'         For univariate data: It is a vector of means. Each element of the vector
#'         corresponds to one Gaussian.
#'
#'         \item [3] Standard-deviation(s): \strong{sample_out$sd}
#'
#'         For univariate data: Vector of standard deviation for the Gaussian(s))
#'
#'         \item [4] Priors: \strong{sample_out$prior}
#'         A vector of priors for the Gaussian(s).
#'         }
#'
#' @usage
#' dcem_cluster_uv(data, mean_vector, sd_vector, prior_vec, num, iteration_count,
#' threshold, numrows, numcols)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.
#'
#' @references
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#' (2016) <doi:https://doi.org/10.1007/s41060-017-0062-1>.

dcem_cluster_uv <-

  function(data,
           mean_vector,
           sd_vector,
           prior_vec,
           num,
           iteration_count,
           threshold,
           numrows,
           numcols)

  {
    counter = 1
    t_status = TRUE

    p_density = matrix(0,
                       nrow = num,
                       ncol = numrows,
                       byrow = TRUE)

    #Repeat till threshold achieved or convergence whichever is earlier.
    while (counter <= iteration_count) {
      old_mean = mean_vector
      weight_mat = matrix(0,
                          nrow = num,
                          ncol = numrows,
                          byrow = TRUE)

      for (clus in 1:num) {
        p_density[clus, ] = dnorm(data, mean_vector[clus] , sd_vector[clus]) * prior_vec[clus]
      }

      sum_p_density = colSums(p_density)
      for (i in 1:num) {
        for (j in 1:numrows) {
          weight_mat[i, j] = p_density[i, j] / sum_p_density[j]
        }
      }

      #Maximize standard-deviation and mean
      for (clus in 1:num) {
        prior_vec[clus] = sum(weight_mat[clus, ]) / numrows
        mean_vector[clus] = (sum(data * weight_mat[clus, ]) / sum(weight_mat[clus, ]))
        sd_vector[clus] = sum(((data - mean_vector[clus]) ^ 2) * weight_mat[clus, ])
        sd_vector[clus] = sqrt(sd_vector[clus] / sum(weight_mat[clus, ]))
      }

      #Find the difference in the mean
      mean_diff = sum((mean_vector - old_mean) ^ 2)

      if (!is.na(mean_diff) && mean_diff < threshold) {
        #print((paste("Convergence at iteration number: ", counter)))
        break
      }

      if (counter == threshold) {
        #print("Maximum iterations reached. Halting.")
        break
      }

      counter = counter + 1
    }

    output = list(
      prob = weight_mat,
      mean = mean_vector,
      sd = sd_vector,
      prior = prior_vec
    )
    return(output)

  }
