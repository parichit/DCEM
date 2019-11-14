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
#' \strong{Note: Choosing a very small value (0.0000001) for threshold can increase the runtime substantially
#' and the algorithm may not converge. On the other hand, choosing a larger value (0.1)
#' can lead to sub-optimal clustering. Default: 0.00001}.
#'
#' @param numrows (numeric): Number of rows in the dataset (After processing the missing values).
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
#' dcem_star_cluster_mv(data, mean_mat, cov_list, prior_vec, num, iteration_count, numrows)
#'
#' @references
#' Using data to build a better EM: EM* for big data.
#'
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#' (2016) <https://doi.org/10.1007/s41060-017-0062-1>.
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is supported by NCI Grant 1R01CA213466-01.


# remove_data1 <- function(heap, indices){
#
#   if(!is.null(nrow(heap))){
#
#     leaf_start <- floor((nrow(heap)/2 + 1))
#     leaves <- as.matrix(heap[leaf_start:nrow(heap), ])
#
#     if(length(indices) == nrow(leaves)){
#       heap <- heap[1:leaf_start-1, ]
#     }
#     else{
#       leaves <- leaves[-indices, ]
#       heap <- heap[1:leaf_start-1, ]
#       heap <- rbind(heap, leaves)
#     }
#
#   }
#   return(heap)
# }


remove_data1 <- function(heap, indices){

  if(!is.null(ncol(heap))){

    leaf_start <- floor((ncol(heap)/2 + 1))
    leaves <- as.matrix(heap[, leaf_start:ncol(heap)])

    if(length(indices) == ncol(leaves)){
      heap <- heap[, 1:leaf_start-1]
    }
    else{
      leaves <- leaves[, -indices]
      heap <- heap[, 1:leaf_start-1]
      heap <- cbind(heap, leaves)
    }

  }
  return(heap)
}


# insert_data1 <- function(heap, node){
#
#     #temp = matrix()
#     #heap <- rbind(heap, c(node[1], node[2]))
#     heap <- rbindlist(list(heap, list(node[1], node[2])))
#     i <- nrow(heap)
#
#    while(floor(i/2)>0){
#
#      if ( heap[floor(i/2), 1] <  heap[i, 1]){
#      temp = heap[i, ]
#      heap[floor(i/2), ] <- temp
#      heap[i, ] <- temp
#      i = floor(i/2);
#      }
#      else{
#        break
#      }
#    }
#     return(heap)
# }


insert_data1 <- function(heap, node){

  heap <- cbind(heap, c(node[1], node[2]))
  i <- ncol(heap)

  while(floor(i/2)>0){

    if ( heap[1, floor(i/2)] < heap[1, i]){
      temp <- heap[, i]
      #heap[, c(floor(i/2), i)] <- heap[, c(i, floor(i/2))]
      heap[, i] <- heap[, floor(i/2)]
      heap[, floor(i/2)] <- temp
      i <- floor(i/2);
    }
    else{
      break
    }
  }
  return(heap)
}

test_fun <- function(heap){

  if(ncol(heap) == 1 | ncol(heap) == 0){
    return(heap[2, ])
  }

  leaf_start = floor(ncol(heap)/2)
  leaf_end = ncol(heap)

  #print(paste(leaf_start, leaf_end))

  out = list(heap[, 1:leaf_start-1], heap[2, leaf_start:leaf_end])
  return(out)
}


# my_seperate_data <- function(heap){
#
#   if(nrow(heap) == 1 || nrow(heap) == 0){
#     return(as.numeric(heap[,2]))
#   }
#
#   leaf_start = floor(nrow(heap)/2)
#   leaf_end = nrow(heap)-1
#
#   leaf_index = as.numeric(heap[leaf_start:leaf_end , 2])
#   return(leaf_index)
# }



dcem_star_cluster_mv <-
  function(data,
           mean_mat,
           cov_list,
           prior_vec,
           num,
           iteration_count,
           numrows)

  {
    counter <- 1

    p_density <- matrix(0,
                       nrow = num,
                       ncol = numrows,
                       byrow = TRUE)

    # Create a list of heaps(one heap per cluster, heap is implemneted as a dataframes!)
    heap_list <- rep(list(matrix()), num)
    index_list <- rep(list(c()), num)

    old_leaf_values <- c()
    tt <- .Machine$double.eps

    # Expectation
    for (clus in 1:num) {
      p_density[clus, ] <- dmvnorm(data, mean_mat[clus, ] , cov_list[[clus]]) * prior_vec[clus]
    }

    sum_p_density <- colSums(p_density)
    p_density <- sweep(p_density, 2, sum_p_density, '/')

    #p_density <- round(p_density, 8)

    p_density[is.nan(p_density)] <- tt
    p_density[p_density <= 0.0] <- tt

    heap_index <- apply(p_density, 2, which.max)
    data_prob <- apply(p_density, 2, max)
    cluster_map <- heap_index

    # Maximisation
    mean_mat <- p_density %*% data
    mean_mat <- mean_mat / rowSums(p_density)
    prior_vec <- rowSums(p_density) / numrows

    # Setup heap
    for (clus in 1:num) {

      # Put the data in the heap (data belonging to their own clusters)
      ind = which(heap_index == clus)
      #temp_data <- matrix(0, ncol=length(ind), nrow=2)
      temp_data <- matrix(data_prob[ind], nrow=1)
      temp_data <- rbind(temp_data, ind)
      heap_list[[clus]] <- temp_data

      # Build the heap from data frames
      heap_list[[clus]] <- c_build_heap(heap_list[[clus]])

      #index_list[[clus]] <- c_separate_data(heap_list[[clus]])
      print(paste("before division", clus, ncol(heap_list[[clus]])))
      out = test_fun(heap_list[[clus]])
      heap_list[[clus]] <- out[[1]]
      index_list[[clus]] <- out[[2]]
      print(paste("after division", clus, ncol(heap_list[[clus]]), "leaves", length(index_list[[clus]])))


      #Co-variance.
      cov_list[[clus]] = 0
      temp = stats::cov.wt(
        data,
        p_density[clus, ],
        cor = TRUE
      )$cov

      # Take care of the singularity condition.
      if (matrixcalc::is.singular.matrix(temp)) {
        diag(temp) <- diag(temp) + 0.000000000000001
      }
      cov_list[[clus]] <- temp

      # Get the leaf nodes
      old_leaf_values <- c(old_leaf_values, index_list[[clus]])
      #print(heap_list[[clus]][1:2,1:2])
    }


    #print("initial step done.")
    # Repeat till convergence threshold or iteration which-ever is earlier.
    while (counter <= iteration_count) {

      # print(counter)
      # print(paste("list len: ", length(index_list)))
      #
      # print(length(index_list[[clus]]))

      new_leaf_values <- c()

      for (clus in 1:num) {
        p_density[clus, ] <- dmvnorm(data, mean_mat[clus, ], cov_list[[clus]]) * prior_vec[clus]
      }

      # Expectation
      sum_p_density <- colSums(p_density)
      p_density <- sweep(p_density, 2, sum_p_density, '/')

      p_density[is.nan(p_density)] <- tt
      p_density[p_density <= 0.0] <- tt

      #get the new heap for the leaves
      # heap_index <- apply(p_density[, old_leaf_values], 2, which.max)
      # #get the new probability for the leaf
      # data_prob <- apply(p_density[, old_leaf_values], 2, max)
      # heap_index <- unlist(heap_index)
      # #get the old heap for the leaves
      # leaf_map <- cluster_map[old_leaf_values]
      # #get the leaves for which the heap has changes (new heap != old heap)
      # points <- which(heap_index != leaf_map)

      # Maximisation
      mean_mat <- p_density %*% data
      mean_mat <- mean_mat / rowSums(p_density)
      prior_vec <- rowSums(p_density) / numrows

      for (clus in 1:num){

        cov_list[[clus]] = 0
        temp <- stats::cov.wt(
          data,
          p_density[clus, ],
          cor = FALSE,
          method = "unbiased"
        )$cov

        if (matrixcalc::is.singular.matrix(temp)) {
          diag(temp) = diag(temp) + 0.000000000000001
        }
        cov_list[[clus]] <- temp
        leaves_ind <- index_list[[clus]]

        if (length(leaves_ind) == 1){
          new_heap_assign_for_leaves <-which.max(p_density[, leaves_ind])
          new_liklihood_for_leaves <- max(p_density[, leaves_ind])
        }
        else{
          new_heap_assign_for_leaves <- unlist(apply(p_density[, leaves_ind], 2, which.max))
          new_liklihood_for_leaves <- unlist(apply(p_density[, leaves_ind], 2, max))
        }

        # index of leaves for which new heap is different from old heap.
        # leaves_whose_heap_has_changed = which(new_heap_assign_for_leaves != clus)

        #print(paste("length", length(leaves_whose_heap_has_changed)))

        #if (length(leaves_whose_heap_has_changed) != 0)
        #{

          #new_heaps = new_heap_assign_for_leaves[leaves_whose_heap_has_changed]

          # new data prob
          #new_data_prob = new_liklihood_for_leaves[leaves_whose_heap_has_changed]

          # actual node value
          #node_val = leaves_ind[leaves_whose_heap_has_changed]

          # Remove from old heap
          # heap_list[[clus]] <- remove_data1(heap_list[[clus]], leaves_whose_heap_has_changed)

          # Insert into new heap
          for (j in 1:length(leaves_ind)){
            yu = new_heap_assign_for_leaves[j]
            #heap_list[[new_heaps[j]]] <- insert_data1(heap_list[[new_heaps[j]]], c(new_data_prob[j], node_val[j]))
            heap_list[[yu]] <- insert_data1(heap_list[[yu]], c(new_liklihood_for_leaves[j], leaves_ind[j]))
          }
        #}
        print(paste("heap: ", clus, " size: ", ncol(heap_list[[clus]])," leaves: ", length(leaves_ind), " going to change: ", length(leaves_whose_heap_has_changed)))
        #print(paste('processed clus: ', clus))
      }

      # if (length(points) != 0){
      #
      #   # Re-assing leaf nodes
      #   for (index in points){
      #
      #     # If data point has higher weight for another cluster than the previous one,
      #     #heap_list[[cluster_map[index]]] <- c_remove_node(heap_list[[leaf_map[index]]], old_leaf_values[index])
      #
      #     # Insert into new heap
      #     heap_list[[heap_index[index]]] <- c_insert_node(heap_list[[heap_index[index]]], c(data_prob[index], old_leaf_values[index]))
      #     cluster_map[old_leaf_values[index]] <- heap_index[index]
      #   }
      #
      # }

      for (clus in 1:num) {
        if (is.null(ncol(heap_list[[clus]]))){
          next
        }
        #index_list[[clus]] <- NULL
        #index_list[[clus]] <- c_separate_data(heap_list[[clus]])

        out = test_fun(heap_list[[clus]])
        heap_list[[clus]] <- out[[1]]
        index_list[[clus]] <- out[[2]]

        # Putting all leaf nodes together to re-assign later
        new_leaf_values <- c(new_leaf_values, index_list[[clus]])
      }

      # print(old_leaf_values)
      # print(new_leaf_values)

      # Maximisation
      # mean_mat <- p_density %*% data
      # mean_mat <- mean_mat / rowSums(p_density)
      # prior_vec <- rowSums(p_density) / numrows
      #
      # for (clus in 1:num) {
      #   cov_list[[clus]] = 0
      #   temp <- stats::cov.wt(
      #     data,
      #     p_density[clus, ],
      #     cor = FALSE,
      #     center = TRUE,
      #     method = "unbiased"
      #   )$cov
      #
      #   if (matrixcalc::is.singular.matrix(temp)) {
      #     diag(temp) = diag(temp) + 0.000000000000001
      #   }
      #   cov_list[[clus]] <- temp
      #
      #   # Get the values (data identifier) from heap
      #   temp <- c_get_leaves(heap_list[[clus]])
      #   #leaf_keys <- temp[, 1]
      #   leaf_values <- temp[, 2]
      #
      #   # Putting all leaf nodes together to re-assign later
      #   new_leaf_values <- c(new_leaf_values, leaf_values)
      #   #all_leaf_keys <- c(all_leaf_keys, leaf_keys)
      # }

      #print(paste("leaf size", length(old_leaf_values), length(new_leaf_values), length(setdiff(old_leaf_values, new_leaf_values))))

      # Working on the stopping criteria
      if (round( (length(setdiff(old_leaf_values, new_leaf_values)) / length(new_leaf_values)), 4) <= 0.01) {
        print(paste("Convergence at iteration", counter))
        break
      }

      if (counter == iteration_count) {
        print("Maximum iterations reached. Halting.")
        break
      }

      old_leaf_values <- new_leaf_values
      counter <- counter + 1

    }

    output = list(
      prob = p_density,
      mean = mean_mat,
      cov = cov_list,
      prior = prior_vec
    )
    return(output)

  }
