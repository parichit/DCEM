#The main EM routine.
require(mvtnorm)
require(matrixcalc)

#' dcem_cluster (multivariate data): Part of DCEM package.
#'
#' Implements the Expectation Maximization algorithm for multivariate data. This function is internally
#' called by the dcem_train routine.
#'
#' @param data A matrix: The dataset provided by the user.
#'
#' @param mean_mat (matrix): The matrix containing the initial mean(s) for the Gaussian(s).
#'
#' @param cov_list (list): A list containing the initial covariance matrices for the Gaussian(s).
#'
#' @param prior_vec (vector): A vector containing the initial priors for the Gaussian(s).
#'
#' @param num (numeric): The number of clusters specified by the user. Default value is 2.
#'
#' @param iteration_count (numeric): The number of iterations for which the algorithm should run, if the
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
#' @return
#'         A list of objects. This list contains parameters associated with the
#'         Gaussian(s) (posterior probabilities, mean, co-variance and priors)
#'
#'\enumerate{
#'         \item [1] Posterior Probabilities:  \strong{sample_out$prob}
#'         (a matrix of posterior-probabilities for the points in the dataset.)
#'
#'         \item [2] Mean(s): \strong{sample_out$mean}
#'
#'         For multivariate data: It is a matrix of means for the Gaussian(s). Each row in
#'         the  matrix corresponds to a mean for the Gaussian.
#'
#'         \item [3] Co-variance matrices (in case of multivariate data): \strong{sample_out$cov}
#'         (list of co-variance matrices for the Gaussian(s))
#'
#'         \item [4] Priors: \strong{sample_out$prior}
#'         (a vector of priors for the Gaussian(s).)
#'         }
#'
#' @usage
#' dcem_cluster_mv(data, mean_mat, cov_list, prior_vec, num, iteration_count,
#' threshold, numrows, numcols)
#'
#' @references
#'Using data to build a better EM: EM* for big data.
#'
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#'(2016) <doi:https://doi.org/10.1007/s41060-017-0062-1>.
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.

dcem_cluster_mv <-
  function(data,
           mean_mat,
           cov_list,
           prior_vec,
           num,
           iteration_count,
           threshold,
           numrows,
           numcols)

  {

    counter = 1

    p_density = matrix(0,
               nrow = num,
               ncol = numrows,
               byrow = TRUE)

    # Repeat till convergence threshold or iteration which-ever is earlier.
    while (counter <= iteration_count) {
      old_mean = mean_mat
      weight_mat = matrix(0,
                          nrow = num,
                          ncol = numrows,
                          byrow = TRUE)

      for (clus in 1:num) {
        p_density[clus,] = mvtnorm::dmvnorm(data, mean_mat[clus,] , cov_list[[clus]]) * prior_vec[clus]
      }

      sum_p_density = colSums(p_density)
      for (i in 1:num) {
        for (j in 1:numrows) {
          weight_mat[i, j] = p_density[i, j] / sum_p_density[j]
        }
      }

      mean_mat = weight_mat %*% data
      mean_mat = mean_mat / rowSums(weight_mat)

      prior_vec = rowSums(weight_mat) / numrows

      # Maximise co-variance and prior vec
      for (clus in 1:num) {
        cov_list[[clus]] = 0
        temp = stats::cov.wt(data, weight_mat[clus,], cor = FALSE, center = TRUE, method = "unbiased")$cov

        if (matrixcalc::is.singular.matrix(temp)) {
          #print("Handling singularity condition.");
          diag(temp) = diag(temp) + 0.01
          cov_list[[clus]] = temp
        }
        else{
          cov_list[[clus]] = temp
        }

      }

      # Find the difference in the mean
      mean_diff = (sum((mean_mat - old_mean) ^ 2))


      if (mean_diff < threshold) {
        #print((paste("Convergence at iteration number: ",counter)))
        break
      }

      if(counter == threshold) {
        #print("Iteration threshold crossed. Stoping the execution.")
        break
      }

      counter = counter + 1
    }

  output = list(prob = weight_mat,  mean = mean_mat, cov = cov_list, prior = prior_vec)
  return(output)

  }
