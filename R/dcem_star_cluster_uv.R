#The main EM routine.
require(mvtnorm)
require(matrixcalc)
#source("./R/heap.R")

#' dcem_star_cluster_uv (univariate data): Part of DCEM package.
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
#' dcem_star_cluster_uv(data, mean_vector, sd_vector, prior_vec, num, iteration_count,
#' threshold, numrows, numcols)
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.
#'
#' @references
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#' (2016) <doi:https://doi.org/10.1007/s41060-017-0062-1>.


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

    weight_mat = matrix(0,
                        nrow = num,
                        ncol = numrows,
                        byrow = TRUE)

    old_weight_mat = matrix(0,
                            nrow = num,
                            ncol = numrows,
                            byrow = TRUE)

    # Repeat till threshold achieved or convergence whichever is earlier.
    while (counter <= iteration_count) {

      old_mean = mean_vector
      #print(paste('Iteration is:', counter))

      if (counter == 1){

      for (clus in 1:num) {
        p_density[clus, ] = dnorm(data, mean_vector[clus] , sd_vector[clus]) * prior_vec[clus]
      }

      sum_p_density = colSums(p_density)

      # Expectation, probability of data belonging to different gaussians.
      for (i in 1:num) {
        for (j in 1:numrows) {
          weight_mat[i, j] = p_density[i, j] / sum_p_density[j]
        }
      }

      # Put the data in the heap (data belonging to their own clusters)
      for (j in 1:numrows){
        heap_index = which.max(weight_mat[ , j])
        data_prob = max(weight_mat[ , j])
        heap_list[[heap_index]] <- rbind(heap_list[[heap_index]], c(data_prob, j))
        cluster_map[, j] = heap_index
      }

      # Maximize standard-deviation and mean
      for (clus in 1:num) {

        # Build the heap from data frames
        colnames(heap_list[[clus]]) <- c('keys', 'vals')
        heap_list[[clus]] <- build_heap(heap_list[[clus]])
        heap_list[[clus]] <- build_heap(heap_list[[clus]])

        prior_vec[clus] = sum(weight_mat[clus, ]) / numrows
        mean_vector[clus] = (sum(data * weight_mat[clus, ]) / sum(weight_mat[clus, ]))
        sd_vector[clus] = sum(((data - mean_vector[clus]) ^ 2) * weight_mat[clus, ])
        sd_vector[clus] = sqrt(sd_vector[clus] / sum(weight_mat[clus, ]))
      }

      }

      else{

        #old_weight_mat = weight_mat
        all_leaf_values = c()
        all_leaf_keys = c()

        # Get the leaf nodes.
        for (clus in 1:num) {

          # Get the heap into a temporary list
          leaf_mat = get_leaves(heap_list[[clus]])

          leaf_keys = leaf_mat$keys
          leaf_value = leaf_mat$vals

          # Get the posterior probability for only the leaf nodes.
          p_density[clus, ] = dnorm(data, mean_vector[clus] , sd_vector[clus]) * prior_vec[clus]

          # Putting all leaf nodes together to re-assign later.
          all_leaf_keys <- c(all_leaf_keys, leaf_keys)
          all_leaf_values <- c(all_leaf_values, leaf_value)
          print(paste("heap-", clus, "size-", nrow(heap_list[[clus]]),"leaves-", length(leaf_value)))
        }

        sum_p_density = colSums(p_density)

        #Expectation, probability of data belonging to different gaussians.
        for (i in 1:num) {
          for (j in 1:numrows) {
            weight_mat[i, j] = p_density[i, j] / sum_p_density[j]
          }
        }

        #print(paste("heap-1 size: ", nrow(heap_list[[1]])))
        #print(paste("heap-2 size: ", nrow(heap_list[[2]])))

        # Put the data into a heap (points belonging to their own clusters)

        #print(paste("all leaves-", length(all_leaf_values)))

        for (j in 1:length(all_leaf_values)){

          #for (j in 1:numrows){

          index = all_leaf_values[j]
          # heap_index = which.max(weight_mat[ , j])
          # data_prob = max(weight_mat[ , j])
          # old_data_prob = max(old_weight_mat[, j])

          heap_index = which.max(weight_mat[ , index])
          data_prob = max(weight_mat[ , index])
          #old_data_prob = max(old_weight_mat[, index])

          # If data point has higher weight for another cluster than the previous one,
          # re-assign.
          if (heap_index != cluster_map[, index]){
            #print(paste("not equal", heap_index, cluster_map[, index], all_leaf_keys[j]))

            #print(paste("1. heap-", cluster_map[, index], "size: ", nrow(heap_list[[cluster_map[, index]]])))
            # print(heap_list[[cluster_map[, index]]])
            # print(all_leaf_values)
            # print(all_leaf_keys)
            # print(weight_mat[ , index])
            # print(max(weight_mat[ , index]))

            heap_list[[cluster_map[, index]]] <- remove_node(heap_list[[cluster_map[, index]]], all_leaf_keys[j], cluster_map[, index])
            #print(paste("heap-", cluster_map[, index], "size: ", nrow(heap_list[[cluster_map[, index]]])))
            #print(paste("2. heap-", cluster_map[, index], "size: ", nrow(heap_list[[cluster_map[, index]]])))


            # Insert into new heap.
            #heap_list[[paste('heap-', heap, sep='')]] <- insert(heap_list[[paste('heap-', heap, sep='')]], -temp_prob, index)
            # temp = heap_list[[heap_index]]
            # temp = insert_node(temp, c(data_prob, index))
            # heap_list[[heap_index]] = temp

            heap_list[[heap_index]] <- insert_node(heap_list[[heap_index]], c(data_prob, index))
            #print(paste("heap-", heap_index, "size: ", nrow(heap_list[[heap_index]])))
            #print(heap_list[[heap_index]])
            cluster_map[, index] = heap_index
          }
        }

        # Maximize standard-deviation and mean
        for (clus in 1:num) {
          prior_vec[clus] = sum(weight_mat[clus, ]) / numrows
          mean_vector[clus] = (sum(data * weight_mat[clus, ]) / sum(weight_mat[clus, ]))
          sd_vector[clus] = sum(((data - mean_vector[clus]) ^ 2) * weight_mat[clus, ])
          sd_vector[clus] = sqrt(sd_vector[clus] / sum(weight_mat[clus, ]))
        }

        #print(paste("heap-1 size, ", nrow(heap_list[[1]]), "heap-2 size, ", nrow(heap_list[[2]])))

      }

      #Find the difference in the mean
      mean_diff = sum((mean_vector - old_mean) ^ 2)

      if (!is.na(mean_diff) && mean_diff < threshold) {
        print((paste("Convergence at iteration number: ", counter)))
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
      prob = weight_mat,
      mean = mean_vector,
      sd = sd_vector,
      prior = prior_vec
    )
    return(output)

  }
