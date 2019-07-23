# The main EM_Star routine.
require(mvtnorm)
require(matrixcalc)

#' dcem_star_cluster_uv (univariate data): Part of DCEM package.
#'
#' Implements the EM* algorithm for the univariate data. This function is internally
#' called by the dcem_star_train routine.
#'
#' @param data (matrix): The dataset provided by the user (converted to matrix format).
#'
#' @param mean_vector (vector): The vector containing the initial means of the Gaussians.
#'
#' @param sd_vector (vector): The vector containing the initial standard deviation for the Gaussians. The initial
#' sd are set to be 1. They are updated during the iterations of the algorithm.
#'
#' @param prior_vec (vector): The vector containing the initial priors for the Gaussians (initialized
#' uniformly).
#'
#' @param num (numeric): The number of clusters specified by the user. Default is 2.
#'
#' @param iteration_count (numeric): The number of iterations for which the algorithm should run. If the
#' convergence is not achieved within the specified threshold then the algorithm stops and exits.
#' Default is 200.
#'
#' @param threshold (numeric): A small value to check for convergence (if the estimated mean(s) are within this
#' specified threshold then the algorithm stops and exit).
#'
#' \strong{Note: Choosing a very small value (0.0000001) for threshold can increase the runtime substantially
#' and the algorithm may not converge. On the other hand, choosing a larger value (0.1)
#' can lead to sub-optimal clustering. Default is 0.00001}.
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
#'         \item (1) Posterior Probabilities: \strong{sample_out$prob}
#'         A matrix of posterior-probabilities
#'
#'         \item (2) Mean(s): \strong{sample_out$mean}
#'
#'         For univariate data: It is a vector of means. Each element of the vector
#'         corresponds to one Gaussian.
#'
#'         \item (3) Standard-deviation(s): \strong{sample_out$sd}
#'
#'         For univariate data: Vector of standard deviation for the Gaussian(s))
#'
#'         \item (4) Priors: \strong{sample_out$prior}
#'         A vector of priors for the Gaussian(s).
#'         }
#'
#' @usage
#' dcem_star_cluster_uv(data, mean_vector, sd_vector, prior_vec, num, iteration_count,
#' threshold, numrows, numcols)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.
#'
#' @references
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#' (2016) <https://doi.org/10.1007/s41060-017-0062-1>.


dcem_star_cluster_uv <-

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

    # Create a list of heaps(one heap per cluster, heap is implemneted as a dataframes!).
    heap_list = rep(list(data.frame()), num)

    cluster_map = matrix(0,
                         nrow = 1,
                         ncol = numrows,
                         byrow = TRUE)

    # Repeat till threshold achieved or convergence whichever is earlier.
    while (counter <= iteration_count) {

      old_mean = mean_vector

      if (counter == 1){

      for (clus in 1:num) {
        p_density[clus, ] = dnorm(data, mean_vector[clus] , sd_vector[clus]) * prior_vec[clus]
      }

      sum_p_density = colSums(p_density)

      # Expectation, probability of data belonging to different gaussians.
      # Commented for testing.
      # Same operation can be done in a single loop.

      for (i in 1:num) {
          p_density[i, ] = p_density[i, ] / sum_p_density
      }

      # Put the data in the heap (data belonging to their own clusters)
      for (j in 1:numrows){

        # Commented to avoid extra variable.
        # p_density can be used instead of
        # weight_mat.

        heap_index = which.max(p_density[ , j])
        data_prob = max(p_density[ , j])
        heap_list[[heap_index]] <- rbind(heap_list[[heap_index]], c(data_prob, j))
        cluster_map[, j] = heap_index
      }

      # Maximize standard-deviation and mean
      for (clus in 1:num) {

        # Build the heap from data frames
        colnames(heap_list[[clus]]) <- c('keys', 'vals')
        heap_list[[clus]] <- build_heap(heap_list[[clus]])
        heap_list[[clus]] <- build_heap(heap_list[[clus]])

        # Commented to avoid extra variable.
        # p_density can be used instead of
        # weight_mat.

        prior_vec[clus] = sum(p_density[clus, ]) / numrows
        mean_vector[clus] = (sum(data * p_density[clus, ]) / sum(p_density[clus, ]))
        sd_vector[clus] = sum(((data - mean_vector[clus]) ^ 2) * p_density[clus, ])
        sd_vector[clus] = sqrt(sd_vector[clus] / sum(p_density[clus, ]))
      }

      }

      else{

        all_leaf_values = c()
        all_leaf_keys = c()

        # Get the leaf nodes.
        for (clus in 1:num) {

          # Get the heap into a temporary list
          leaf_mat = get_leaves(heap_list[[clus]])

          leaf_keys = leaf_mat$keys
          leaf_value = leaf_mat$vals

          # Get the posterior probability for all the data points.
          p_density[clus, ] = dnorm(data, mean_vector[clus] , sd_vector[clus]) * prior_vec[clus]

          # Putting all leaf nodes together to re-assign later.
          all_leaf_keys <- c(all_leaf_keys, leaf_keys)
          all_leaf_values <- c(all_leaf_values, leaf_value)
        }

        sum_p_density = colSums(p_density)

        # Expectation, probability of data belonging to different gaussians.
        for (i in 1:num) {
            p_density[i, ] = p_density[i, ] / sum_p_density
        }

        # Put the data into a heap (points belonging to their own clusters)
        for (j in 1:length(all_leaf_values)){

          index = all_leaf_values[j]

          heap_index = which.max(p_density[ , index])
          data_prob = max(p_density[ , index])

          # If data point has higher weight for another cluster than the previous one,
          # re-assign.
          if (heap_index != cluster_map[, index]){

            heap_list[[cluster_map[, index]]] <- remove_node(heap_list[[cluster_map[, index]]], all_leaf_keys[j], cluster_map[, index])

            # Insert into new heap.
            heap_list[[heap_index]] <- insert_node(heap_list[[heap_index]], c(data_prob, index))
            cluster_map[, index] = heap_index
          }
        }

        # Maximize standard-deviation and mean
        for (clus in 1:num) {
          prior_vec[clus] = sum(p_density[clus, ]) / numrows
          mean_vector[clus] = (sum(data * p_density[clus, ]) / sum(p_density[clus, ]))
          sd_vector[clus] = sum(((data - mean_vector[clus]) ^ 2) * p_density[clus, ])
          sd_vector[clus] = sqrt(sd_vector[clus] / sum(p_density[clus, ]))
        }

      }

      #Find the difference in the mean
      mean_diff = sum((mean_vector - old_mean) ^ 2)

      if (!is.na(mean_diff) && mean_diff < threshold) {
        #print((paste("Convergence at iteration number: ", counter)))
        heap_list <- NULL
        break
      }

      if (counter == threshold) {
        #print("Maximum iterations reached. Halting.")
        break
      }

      counter = counter + 1
    }

    output = list(
      prob = p_density,
      mean = mean_vector,
      sd = sd_vector,
      prior = prior_vec
    )
    return(output)

  }
