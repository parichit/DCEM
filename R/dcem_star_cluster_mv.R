#The main EM routine.
require(mvtnorm)
require(matrixcalc)

#' dcem_star_cluster_mv (multivariate data): Part of DCEM package.
#'
#' Implements the EM* algorithm for multivariate data. This function is internally
#' called by the dcem_star_train routine.
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
#'         \item (1) Posterior Probabilities:  \strong{sample_out$prob}
#'         (a matrix of posterior-probabilities for the points in the dataset.)
#'
#'         \item (2) Mean(s): \strong{sample_out$mean}
#'
#'         For multivariate data: It is a matrix of means for the Gaussian(s). Each row in
#'         the  matrix corresponds to a mean for the Gaussian.
#'
#'         \item (3) Co-variance matrices (in case of multivariate data): \strong{sample_out$cov}
#'         (list of co-variance matrices for the Gaussian(s))
#'
#'         \item (4) Priors: \strong{sample_out$prior}
#'         (a vector of priors for the Gaussian(s).)
#'         }
#'
#' @usage
#' dcem_star_cluster_mv(data, mean_mat, cov_list, prior_vec, num, iteration_count,
#' threshold, numrows, numcols)
#'
#' @references
#' Using data to build a better EM: EM* for big data.
#'
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#' (2016) <https://doi.org/10.1007/s41060-017-0062-1>.
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.

dcem_star_cluster_mv <-
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

    # Create a list of heaps(one heap per cluster, heap is implemneted as a dataframes!).
    heap_list = rep(list(data.frame()), num)

    cluster_map = matrix(0,
                         nrow = 1,
                         ncol = numrows,
                         byrow = TRUE)

    # Repeat till convergence threshold or iteration which-ever is earlier.
    while (counter <= iteration_count) {
      old_mean = mean_mat

      if (counter == 1){

      for (clus in 1:num) {
        p_density[clus,] = mvtnorm::dmvnorm(data, mean_mat[clus,] , cov_list[[clus]]) * prior_vec[clus]
      }

      sum_p_density = colSums(p_density)

      for (i in 1:num) {
          p_density[i, ] = p_density[i, ] / sum_p_density
      }

      # Put the data in the heap (data belonging to their own clusters)
      for (j in 1:numrows){
        heap_index = which.max(p_density[ , j])
        data_prob = max(p_density[ , j])
        heap_list[[heap_index]] <- rbind(heap_list[[heap_index]], c(data_prob, j))
        cluster_map[, j] = heap_index
      }


      # Maximise the mean matrix.
      mean_mat = p_density %*% data
      mean_mat = mean_mat / rowSums(p_density)
      prior_vec = rowSums(p_density) / numrows

      # Maximise co-variance and prior vec.
      # Take care of the singularity condition.
      for (clus in 1:num) {

        # Build the heap from data frames
        colnames(heap_list[[clus]]) <- c('keys', 'vals')
        heap_list[[clus]] <- build_heap(heap_list[[clus]])
        heap_list[[clus]] <- build_heap(heap_list[[clus]])

        cov_list[[clus]] = 0
        temp = stats::cov.wt(data, p_density[clus,], cor = FALSE, center = TRUE, method = "unbiased")$cov

        if (matrixcalc::is.singular.matrix(temp)) {
          #print("Handling singularity condition.");
          diag(temp) = diag(temp) + 0.0000000001
          cov_list[[clus]] = temp
        }
        else
          {
            cov_list[[clus]] = temp
          }

      }

      }

      else {

        all_leaf_values = c()
        all_leaf_keys = c()

        # Get the leaf nodes.
        for (clus in 1:num) {

          # Get the heap into a temporary list
          leaf_keys = get_leaves(heap_list[[clus]])[,1]
          leaf_value = get_leaves(heap_list[[clus]])[,2]

          # Get the posterior probability for all data points.
          p_density[clus,] = mvtnorm::dmvnorm(data, mean_mat[clus,] , cov_list[[clus]]) * prior_vec[clus]

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

        # Maximise the mean matrix.
        mean_mat = p_density %*% data
        mean_mat = mean_mat / rowSums(p_density)
        prior_vec = rowSums(p_density) / numrows

        for (clus in 1:num) {
          cov_list[[clus]] = 0
          temp = stats::cov.wt(data, p_density[clus,], cor = FALSE, center = TRUE, method = "unbiased")$cov

          if (matrixcalc::is.singular.matrix(temp)) {
            #print("Handling singularity condition.");
            diag(temp) = diag(temp) + 0.0000000001
          }
          cov_list[[clus]] = temp
        }

      }

      # Find the difference in the mean
      mean_diff = (sum((mean_mat - old_mean) ^ 2))

      if (!is.na(mean_diff) && mean_diff < threshold) {
        #print((paste("Convergence at iteration number: ",counter)))
        break
      }

      if(counter == threshold) {
        #print("Iteration threshold crossed. Stoping the execution.")
        break
      }

      counter = counter + 1

    }

    output = list(prob = p_density,  mean = mean_mat, cov = cov_list, prior = prior_vec)
    return(output)

  }
