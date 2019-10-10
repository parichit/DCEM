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
           numrows)

  {
    counter = 1

    p_density = matrix(0,
                       nrow = num,
                       ncol = numrows,
                       byrow = TRUE)

    # Create a list of heaps(one heap per cluster, heap is implemneted as dataframes!)
    heap_list <- rep(list(data.frame()), num)

    old_leaf_values <- c()
    all_leaf_keys <- c()

    # Expectation, probability of data belonging to different gaussians
    for (clus in 1:num) {
      p_density[clus, ] <- dnorm(data, mean_vector[clus] , sd_vector[clus]) * prior_vec[clus]
    }

    sum_p_density <- colSums(p_density)
    p_density <- p_density / sum_p_density

    heap_index <- apply(p_density, 2, which.max)
    data_prob <- apply(p_density, 2, max)
    cluster_map <- heap_index

    # Heap creation
    for (clus in 1:num) {
      ind = which(heap_index == clus)
      temp_data <- data.frame(data_prob[ind])
      temp_data <- cbind(temp_data, ind)
      colnames(temp_data) <- c('keys', 'vals')
      heap_list[[clus]] <- temp_data

      # Build the heap from data frames
      heap_list[[clus]] <- build_heap(heap_list[[clus]])
      heap_list[[clus]] <- build_heap(heap_list[[clus]])

      # Get the leaf nodes
      leaf_mat <- get_leaves(heap_list[[clus]])

      leaf_keys <- leaf_mat$keys
      leaf_values <- leaf_mat$vals

      prior_vec[clus] <- sum(p_density[clus, ]) / numrows
      mean_vector[clus] <- (sum(data * p_density[clus, ]) / sum(p_density[clus, ]))
      sd_vector[clus] <- sqrt(sum(((data - mean_vector[clus]) ^ 2) * p_density[clus, ]) / sum(p_density[clus, ]) )

      # Putting all leaf nodes together to re-assign later
      all_leaf_keys <- c(all_leaf_keys, leaf_keys)
      old_leaf_values <- c(old_leaf_values, leaf_values)

    }

    # Repeat till convergence (99 % of the leaves are same between iterations)
    while (counter <= iteration_count) {

        new_leaf_values <- c()

        # Expectation, probability of data belonging to different gaussians
        for (clus in 1:num) {
          p_density[clus, ] <- dnorm(data, mean_vector[clus] , sd_vector[clus]) * prior_vec[clus]
        }

        sum_p_density <- colSums(p_density)
        p_density <- p_density / sum_p_density

        heap_index <- apply(p_density[, old_leaf_values], 2, which.max)
        data_prob <- apply(p_density[, old_leaf_values], 2, max)
        heap_index <- unlist(heap_index)

        leaf_map <- cluster_map[old_leaf_values]

        # Find those points (leaves) whose heaps have changed after the new expectation
        points <- which(heap_index != leaf_map)
        #cluster_map[, old_leaf_values] = leaf_map

        # print("1--")
        # print(cluster_map[old_leaf_values])
        # print(heap_index)
        # print(old_leaf_values)
        #print("points")
        #print(points)

        if (length(points) != 0){

        # Re-assing leaf nodes
        for (index in points){

          # this is the actual point index whose heap have changed (compared to previous assignment)
          #index <- points[j]

          # Remove from heap
          #print(paste("delete from heap:", leaf_map[index], "Insert into heap:", heap_index[index], "val:", old_leaf_values[index]))
          #t <- heap_list[[leaf_map[index]]]

          # print the position at which the value was present in the old heap
          #print(which(t[, 2] == old_leaf_values[index]))

          heap_list[[leaf_map[index]]] <- remove_node(heap_list[[leaf_map[index]]], old_leaf_values[index], leaf_map[index])
          #print("after removal")
          #print(heap_list[[leaf_map[index]]])


          # Insert into heap
          heap_list[[heap_index[index]]] <- insert_node(heap_list[[heap_index[index]]], c(data_prob[index], old_leaf_values[index]))
          #print("after insert")
          #print(heap_list[[heap_index[index]]])

          cluster_map[old_leaf_values[index]] = heap_index[index]
        }
        }

        # print("2----")
        # print(cluster_map[old_leaf_values])

        # Print the dimensions of the heaps
        # for (clus in 1:num) {
        #   print(dim(heap_list[[clus]]))
        # }

        #cluster_map[old_leaf_values] <- leaf_map
        all_leaf_keys <- c()

        # Maximize standard-deviation and mean
        for (clus in 1:num) {
          prior_vec[clus] <- sum(p_density[clus, ]) / numrows
          mean_vector[clus] <- (sum(data * p_density[clus, ]) / sum(p_density[clus, ]))
          sd_vector[clus] <- sqrt(sum(((data - mean_vector[clus]) ^ 2) * p_density[clus, ]) / sum(p_density[clus, ]) )

          # Get the values from heap
          temp <- get_leaves(heap_list[[clus]])
          leaf_values <- temp$vals
          leaf_keys <- temp$keys

          # Putting all leaf nodes together to re-assign later
          new_leaf_values <- c(new_leaf_values, leaf_values)
          all_leaf_keys <- c(all_leaf_keys, leaf_keys)
        }

        # Working on the stopping criteria
        if (length(setdiff(old_leaf_values, new_leaf_values))/length(new_leaf_values) <= 0.01){
          print(paste("Convergence, Halting.", counter))
          break
        }

        else if(counter >= iteration_count){
          print("Max iterations reached.")
        }

        # print(paste("old leaves: ", length(old_leaf_values), "new leaves:", length(new_leaf_values), length(setdiff(old_leaf_values, new_leaf_values))))
        #
        # print("old leaf")
        # print(old_leaf_values)
        # print("new leaf")
        # print(new_leaf_values)

        # print("1st heap")
        # print(heap_list[[1]])
        # print("2nd heap")
        # print(heap_list[[2]])

        old_leaf_values <- c()
        old_leaf_values <- new_leaf_values

      counter <- counter + 1
    }

    output = list(
      prob = p_density,
      mean = mean_vector,
      sd = sd_vector,
      prior = prior_vec
    )
    return(output)

  }
