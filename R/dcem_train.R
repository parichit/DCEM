#' dcem_train: Part of DCEM package.
#'
#' Implements the EM algorithm. It calls the relevant clustering routine internally
#' \code{\link{dcem_cluster_uv}} (univariate data) and
#' \code{\link{dcem_cluster_mv}} (multivariate data).
#'
#' @param data (dataframe): The dataframe containing the data. See \code{\link{trim_data}} for
#' cleaning the data.
#'
#' @param threshold (decimal): A  value to check for convergence (if the meu are within this
#' value then the algorithm stops and exit). \strong{Default: 0.00001}.
#'
#' @param iteration_count (numeric): The number of iterations for which the algorithm should run, if the
#' convergence is not achieved within the specified count then the algorithm stops and exit.
#' \strong{Default: 200}.
#'
#' @param num_clusters (numeric): The number of clusters. Default: \strong{2}
#'
#' @param seeding (string): The initialization scheme ('rand', 'improved'). Default: \strong{rand}
#'
#' @return
#'         A list of objects. This list contains parameters associated with the Gaussian(s)
#'         (posterior probabilities, meu, sigma and priors). The
#'         parameters can be accessed as follows where sample_out is the list containing
#'         the output:
#'
#'\enumerate{
#'         \item (1) Posterior Probabilities: \strong{sample_out$prob}: A matrix of
#'         posterior-probabilities
#'
#'         \item (2) Meu: \strong{sample_out$meu}
#'
#'         For multivariate data: It is a matrix of meu(s). Each row in
#'         the  matrix corresponds to one meu.
#'
#'         For univariate data: It is a vector of meu(s). Each element of the vector
#'         corresponds to one meu.
#'
#'         \item (3) Sigma: \strong{sample_out$sigma}
#'
#'         For multivariate data: List of co-variance matrices for the Gaussian(s).
#'
#'         For univariate data: Vector of standard deviation for the Gaussian(s))
#'
#'         \item (4) Priors: \strong{sample_out$prior}: A vector of priors.
#'         }
#'
#' @usage
#' dcem_train(data, threshold, iteration_count,  num_clusters, seeding)
#'
#' @references
#'Using data to build a better EM: EM* for big data.
#'
#'Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#'(2016) <https://doi.org/10.1007/s41060-017-0062-1>.
#'
#' @examples
#'# Simulating a mixture of univariate samples from three distributions
#'# with meu as 20, 70 and 100 and standard deviation as 10, 100 and 40 respectively.
#'sample_uv_data = as.data.frame(c(rnorm(100, 20, 10), rnorm(70, 70, 100), rnorm(50, 100, 40)))
#'
#'# Randomly shuffle the samples.
#'sample_uv_data = as.data.frame(sample_uv_data[sample(nrow(sample_uv_data)),])
#'
#'# Calling the dcem_train() function on the simulated data with threshold of
#'# 0.000001, iteration count of 1000 and random seeding respectively.
#'sample_uv_out = dcem_train(sample_uv_data, num_clusters = 3, iteration_count = 100,
#'threshold = 0.001)
#'
#'# Simulating a mixture of multivariate samples from 2 gaussian distributions.
#'sample_mv_data = as.data.frame(rbind(MASS::mvrnorm(n=100, rep(2,5), Sigma = diag(5)),
#'MASS::mvrnorm(n=50, rep(14,5), Sigma = diag(5))))
#'
#'# Calling the dcem_train() function on the simulated data with threshold of
#'# 0.00001, iteration count of 100 and random seeding method respectively.
#' sample_mv_out = dcem_train(sample_mv_data, threshold = 0.001, iteration_count = 100)
#'
#' sample_mv_out$meu
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.
#' @export

dcem_train <-
  function(data,
           threshold,
           iteration_count,
           num_clusters, seeding) {
    if (missing(threshold)) {
      threshold = 0.00001
      print("Using default value for convergence threshold = 0.00001.")
    }
    else{
      print(paste("Specified threshold = ", threshold))
    }

    if (missing(iteration_count)) {
      iteration_count = 200

      print("Using default value for iteration count = 200.")
    }
    else{
      print(paste("Specified iterations = ", iteration_count))
    }

    if (missing(num_clusters)) {
      num_clusters = 2

      print("Using default value for number of clusters = 2.")
    }
    else{
      print(paste("Specified number of  clusters = ", num_clusters))
    }

    if (missing(seeding) ) {
      seeding = "rand"
      print("Using the random initialization scheme.")
    }
    else{
      seeding = seeding
      print("Using the improved Kmeans++ initialization scheme.")
    }

    data <- apply(data, 2, as.numeric)
    data[is.na(data)] <- NULL

    # Safe copy the data for operations
    test_data <- as.matrix(data)
    num_data <- nrow(test_data)
    valid_columns <- ncol(test_data)

    em_data_out <- list()

    if (valid_columns >= 2) {
      if (seeding == "rand"){
        meu <- meu_mv(test_data, num_clusters)
      }
      else{
        meu <- meu_mv_impr(test_data, num_clusters)
        print("got the improved matrix")
      }
      sigma <- sigma_mv(num_clusters, valid_columns)
      priors <- get_priors(num_clusters)
      em_data_out <- dcem_cluster_mv(
        test_data,
        meu,
        sigma,
        priors,
        num_clusters,
        iteration_count,
        threshold,
        num_data
      )
    }

    if (valid_columns < 2) {
      if(seeding=="rand"){
        meu <- meu_uv(test_data, num_clusters)
      }
      else{
        meu <- meu_uv_impr(test_data, num_clusters)
      }
      sigma <- sigma_uv(test_data, num_clusters)
      priors <- get_priors(num_clusters)
      em_data_out <- dcem_cluster_uv(
        test_data,
        meu,
        sigma,
        priors,
        num_clusters,
        iteration_count,
        threshold,
        num_data,
        valid_columns
      )
    }

    return(em_data_out)
  }
