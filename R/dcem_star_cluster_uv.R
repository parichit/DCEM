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
<<<<<<< HEAD

remove_data <- function(heap, indices){

  if(!is.null(nrow(heap))){

  leaf_start <- floor((nrow(heap)/2 + 1))
  leaves <- as.matrix(heap[leaf_start:nrow(heap), ])

  if(length(indices) == nrow(leaves)){
    heap <- heap[1:leaf_start-1, ]
  }
  else{
    leaves <- leaves[-indices, ]
    heap <- heap[1:leaf_start-1, ]
    heap <- rbind(heap, leaves)
  }

  }

  #print("Heap and leaves dimensions")
  # print(dim(heap))
  # print(dim(leaves))
  # print(indices)
  # print(leaves)

  return(heap)
}

insert_data <- function(heap, node){

  heap <- rbind(heap, c(node[1], node[2]))
  i <- nrow(heap)
  # print(heap[floor(i/2), 1])
  # print(heap[i, 1])

  while(floor(i/2)>0){

    # print(paste("1", heap[floor(i/2), 1] ))
    # print(paste("2", heap[i, 1] ))
    # print(paste("3", i))

    if ( heap[floor(i/2), 1] <  heap[i, 1]){
      temp = heap[i, ]
      heap[floor(i/2), ] <- temp
      heap[i, ] <- temp
      i = floor(i/2);
    }
    else{
      break
    }

  }

  return(heap)

}
=======
>>>>>>> df71bd21b20d73663f51b71ba022d4b882fe95e6


dcem_star_cluster_uv <-

  function(data,
           mean_vector,
           sd_vector,
           prior_vec,
           num,
           iteration_count,
           numrows)

  {

    p_density = matrix(0,
                       nrow = num,
                       ncol = numrows,
                       byrow = TRUE)

    counter = 1
    tt <- .Machine$double.eps

    # Create a list of heaps (one heap per cluster)
    heap_list <- rep(list(matrix()), num)
    #leaf_list <- rep(list(matrix()), num)
    index_list <- rep(list(c()), num)

    old_leaf_values <- c()

    # Expectation
    for (clus in 1:num) {
      p_density[clus, ] <- dnorm(data, mean_vector[clus] , sd_vector[clus]) * prior_vec[clus]
    }

    sum_p_density <- colSums(p_density)
    p_density <- sweep(p_density, 2, sum_p_density, '/')

    p_density[is.nan(p_density)] <- tt
    p_density[p_density<=0.0] <- tt

    heap_index <- apply(p_density, 2, which.max)
    data_prob <- apply(p_density, 2, max)
    cluster_map <- heap_index

    # Maximisation
    for (clus in 1:num) {

      ind = which(heap_index == clus)
      temp_data <- as.matrix(data_prob[ind])
      temp_data <- cbind(temp_data, ind)
      heap_list[[clus]] <- temp_data

      # Build the heap from data frames
      heap_list[[clus]] <- c_build_heap(heap_list[[clus]])
      #temp_out <- c_separate_data(heap_list[[clus]])
      index_list[[clus]]  <- c_separate_data(heap_list[[clus]])

      # heap_list[[clus]] <- temp_out[[1]]
      # leaf_list[[clus]] <- temp_out[[2]]
      # index_list[[clus]] <- temp_out[[3]]

      #print(typeof(heap_list[[clus]]))
      # print("heap")
      # print(heap_list[[clus]])
      # print("leaves")
      # print(leaf_list[[clus]])
      # print(index_list[[clus]])

      # Get the leaf nodes
      old_leaf_values <- c(old_leaf_values, index_list[[clus]])

      # tmp = data[which(heap_index == clus)]
      # prior_vec[clus] <- sum(p_density[clus, ]) / numrows
      # mean_vector[clus] <- (sum(tmp * p_density[clus, which(heap_index == clus) ]) / sum(p_density[clus, which(heap_index == clus)]))
      # sd_vector[clus] <- sqrt(sum(((tmp - mean_vector[clus]) ^ 2) * p_density[clus, which(heap_index == clus)]) / sum(p_density[clus, which(heap_index == clus)]))

      prior_vec[clus] = sum(p_density[clus, ]) / numrows
      mean_vector[clus] = (sum(data * p_density[clus, ]) / sum(p_density[clus, ]))
      sd_vector[clus] = sqrt(sum(((data - mean_vector[clus]) ^ 2) * p_density[clus, ]) / sum(p_density[clus, ]) )

    }

    #print(old_leaf_values)

    # Repeat till convergence (99 % of the leaves are same between iterations)
    while (counter <= iteration_count) {

        new_leaf_values <- c()

        # Expectation
        for (clus in 1:num) {
          p_density[clus, ] <- dnorm(data, mean_vector[clus] , sd_vector[clus]) * prior_vec[clus]
          }

        sum_p_density <- colSums(p_density)
        p_density <- sweep(p_density, 2, sum_p_density, '/')

        p_density[is.nan(p_density)] <- tt
        p_density[p_density<=0.0] <- tt

        # heap_index <- unlist(apply(p_density[, old_leaf_values], 2, which.max))
        # data_prob <- apply(p_density[, old_leaf_values], 2, max)
        # leaf_map <- cluster_map[old_leaf_values]

        # Find those points (leaves) whose heaps have changed after the new expectation
        #points <- which(heap_index != leaf_map)

        for (clus in 1:num){

          prior_vec[clus] = sum(p_density[clus, ]) / numrows
          mean_vector[clus] = (sum(data * p_density[clus, ]) / sum(p_density[clus, ]))
          sd_vector[clus] = sqrt(sum(((data - mean_vector[clus]) ^ 2) * p_density[clus, ]) / sum(p_density[clus, ]))

          leaves_ind = index_list[[clus]]

          if (length(leaves_ind) == 1){
            new_heap_assign_for_leaves <-which.max(p_density[, leaves_ind])
            new_liklihood_for_leaves <- max(p_density[, leaves_ind])
          }
          else{
          new_heap_assign_for_leaves <- unlist(apply(p_density[, leaves_ind], 2, which.max))
          new_liklihood_for_leaves <- unlist(apply(p_density[, leaves_ind], 2, max))
        }

          # index of leaves for which new heap is different from old heap.
          leaves_whose_heap_has_changed = which(new_heap_assign_for_leaves != clus)

          # actual heap
          #new_heaps = new_heap_assign_for_leaves[leaves_whose_heap_has_changed]
          #print(new_heaps)

          # new data prob
          #new_data_prob = new_liklihood_for_leaves[leaves_whose_heap_has_changed]
          #print(new_data_prob)

          # actual node value
          #node_val = leaves_ind[leaves_whose_heap_has_changed]
          #print(node_val)

          #print(paste("length", length(leaves_whose_heap_has_changed)))

          if (length(leaves_whose_heap_has_changed) != 0)
          {
          new_heaps = new_heap_assign_for_leaves[leaves_whose_heap_has_changed]
          new_data_prob = new_liklihood_for_leaves[leaves_whose_heap_has_changed]
          node_val = leaves_ind[leaves_whose_heap_has_changed]

          # print("from loop")
          # print(leaves_whose_heap_has_changed)

          # Remove from old heap
          #heap_list[[clus]] <- cpp_remove_node(heap_list[[clus]], leaves_whose_heap_has_changed)
          heap_list[[clus]] <- remove_data(heap_list[[clus]], leaves_whose_heap_has_changed)

          # Insert into new heap
          #heap_list <- c_insert_node(heap_list, new_heaps, c(new_data_prob, node_val))
          for (j in 1:length(new_heaps)){
            heap_list[[new_heaps[j]]] <- insert_data(heap_list[[new_heaps[j]]], c(data_prob[j], node_val[j]))
          }
        }

        }

        for (clus in 1:num) {

        if (is.null(nrow(heap_list[[clus]]))){
          #print("not a matrix")
          #print(typeof(heap_list[[clus]]))
          #print(heap_list[[clus]])
          next
        }

          #print(clus)
          # print(dim(heap_list[[clus]]))
          # print(heap_list[[clus]])

        #heap_list[[clus]] <- rbind(heap_list[[clus]], leaf_list[[clus]])
        index_list[[clus]] <- c_separate_data(heap_list[[clus]])

        # Putting all leaf nodes together to re-assign later
        new_leaf_values <- c(new_leaf_values, index_list[[clus]])

        }

        # if (length(points) != 0){
        #
        # # Re-assing leaf nodes
        # for (index in points){
        #   # Remove from heap
        #   heap_list[[leaf_map[index]]] <- c_remove_node(heap_list[[leaf_map[index]]], old_leaf_values[index])
        #
        #   # Insert into heap
        #   heap_list[[heap_index[index]]] <- c_insert_node(heap_list[[heap_index[index]]], c(data_prob[index], old_leaf_values[index]))
        #   cluster_map[old_leaf_values[index]] <- heap_index[index]
        # }
        # }


        # Maximisation
        # for (clus in 1:num) {
        #
        #   # Get the values from heap
        #   temp <- c_get_leaves(heap_list[[clus]])
        #
        #   # Putting all leaf nodes together to re-assign later
        #   new_leaf_values <- c(new_leaf_values, temp[,2])
        #
        #   prior_vec[clus] = sum(p_density[clus, ]) / numrows
        #   mean_vector[clus] = (sum(data * p_density[clus, ]) / sum(p_density[clus, ]))
        #   sd_vector[clus] = sqrt(sum(((data - mean_vector[clus]) ^ 2) * p_density[clus, ]) / sum(p_density[clus, ]))
        # }

        # stopping criteria
        #print(old_leaf_values)
        #print(new_leaf_values)

        #print(paste("leaf size", length(old_leaf_values), length(new_leaf_values), length(setdiff(old_leaf_values, new_leaf_values))))

        if (length(setdiff(old_leaf_values, new_leaf_values))/length(new_leaf_values) <= 0.01){
          print(paste("Convergence at iteration", counter))
          break
        }

        else if(counter == iteration_count){
          print("Max iterations reached.")
          break
        }

        old_leaf_values <- c()
        old_leaf_values <- new_leaf_values

        #print("counter")
        #print(counter)

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
