# The main EM_Star routine.
require(mvtnorm)
require(matrixcalc)

#' dcem_star_cluster_uv (univariate data): Part of DCEM package.
#'
#' Implements the EM* algorithm for the univariate data. This function is called by the
#' dcem_star_train routine.
#'
#' @param data (matrix): The dataset provided by the user.
#'
#' @param meu (vector): The vector containing the initial meu.
#'
#' @param sigma (vector): The vector containing the initial standard deviation.
#'
#' @param prior (vector): The vector containing the initial priors.
#'
#' @param num_clusters (numeric): The number of clusters specified by the user. Default is 2.
#'
#' @param iteration_count (numeric): The number of iterations for which the algorithm should run.
#' If the convergence is not achieved then the algorithm stops. Default is 100.
#'
#' @param num_data (numeric): number of rows in the dataset (After processing the missing values).
#'
#' @return
#'         A list of objects. This list contains parameters associated with the
#'         Gaussian(s) (posterior probabilities, meu, standard-deviation and priors)
#'
#'\enumerate{
#'         \item (1) Posterior Probabilities: \strong{prob}
#'         A matrix of posterior-probabilities
#'
#'         \item (2) Meu: \strong{meu}: It is a vector of meus. Each element of
#'         the vector corresponds to one meu.
#'
#'         \item (3) Sigma: Standard-deviation(s): \strong{sigma}
#'
#'         For univariate data: Vector of standard deviation.
#'
#'         \item (4) Priors: \strong{prior}: A vector of priors.
#'         }
#'
#' @usage
#' dcem_star_cluster_uv(data, meu, sigma, prior, num_clusters, num_data,
#' iteration_count)
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
           meu,
           sigma,
           prior,
           num_clusters,
           num_data,
           iteration_count)

  {
    weights = matrix(0,
                     nrow = num_clusters,
                     ncol = num_data,
                     byrow = TRUE)

    counter = 1
    tolerance <- .Machine$double.eps

    # Create a list of heaps (one heap per cluster)
    heap_list <- rep(list(list()), num_clusters)
    index_list <- c()
    old_leaf_values <- c()
    chk_count = 1

    # Expectation
    weights = expectation_uv(data, weights, meu, sigma, prior, num_clusters, tolerance)
    heap_index <- apply(weights, 2, which.max)
    data_prob <- apply(weights, 2, max)
    cluster_map <- heap_index

    # Maximisation
    out = maximisation_uv(data, weights, meu, sigma, prior, num_clusters, num_data)
    mean_vec = out$meu
    sd_vec = out$sigma
    prior = out$prior

    # Setup heap
    # loop to ensure that no heap is empty
    while (chk_count < 5){

    temp_heap_size = c()

    # Creating heaps
    for (clus in 1:num_clusters) {

      # Put the data in the heap (data belonging to their own clusters)
      ind <- which(heap_index == clus)
      temp_matrix <- matrix(data_prob[ind])
      temp_matrix <- cbind(temp_matrix, ind)

      heap_list[[clus]] <- temp_matrix
      #print(paste("heap: ", clus, "size: ", nrow(heap_list[[clus]])))

      # Build the heap from data frames
      temp_out <- build_heap(heap_list[[clus]])
      heap_list[[clus]] <- split(temp_out, 1:nrow(temp_out))

    }

      # Continue while loop if any heap is empty
      if(any(temp_heap_size==0)){
        print(paste("heap was 0 so repeating: ", chk_count))
        meu = meu_mv(data, num_clusters)
        # Expectation
        weights = expectation_mv(data, weights, meu, sigma, prior, num_clusters, tolerance)
        heap_index <- apply(weights, 2, which.max)
        data_prob <- apply(weights, 2, max)
        cluster_map <- heap_index
        chk_count =  chk_count + 1
      }
      # Break the while loop if none of the heap is empty
      else{
        # Maximisation
        out = maximisation_mv(data, weights, meu, sigma, prior, num_clusters, num_data)
        meu = out$meu
        sigma = out$sigma
        prior = out$prior
        break
      }
    }

    out = separate_data(heap_list, num_clusters)
    heap_list <- out[[1]]
    index_list <- unlist(out[[2]])

    # Get the leaf nodes
    old_leaf_values <- c(old_leaf_values, index_list)

    # Repeat till convergence
    while (counter <= iteration_count) {

      new_leaf_values <- c()
      temp_weights <- matrix(0,
                             nrow = num_clusters,
                             ncol = length(index_list),
                             byrow = TRUE)

      # Expectation
      temp_weights = expectation_uv(data[index_list,],
                                    temp_weights,
                                    meu,
                                    sigma,
                                    prior,
                                    num_clusters,
                                    tolerance)

      weights = update_weights(temp_weights, weights, index_list, num_clusters)


      sum_weights <- colSums(weights)
      weights <- sweep(weights, 2, sum_weights, '/')
      weights[is.nan(weights)] <- tolerance
      weights[weights <= 0.0] <- tolerance

      # Maximisation
      out = maximisation_uv(data, weights, meu, sigma, prior, num_clusters, num_data)
      meu = out$meu
      sigma = out$sigma
      prior = out$prior

      leaves_ind <- index_list

      if (length(leaves_ind) == 1) {
        new_heap_assign_for_leaves <- which.max(temp_weights)
        new_liklihood_for_leaves <- max(temp_weights)
      }
      else{
        new_heap_assign_for_leaves <-
          unlist(apply(temp_weights, 2, which.max))
        new_liklihood_for_leaves <-
          unlist(apply(temp_weights, 2, max))
      }


      # Insert into new heap
      #print(paste("Inserting", length(leaves_ind)))
      heap_list <-
        insert_nodes(
          heap_list,
          new_heap_assign_for_leaves,
          new_liklihood_for_leaves,
          leaves_ind,
          num_clusters
        )
      #print(paste("Inserting done: ", length(leaves_ind)))

      out = separate_data(heap_list, num_clusters)
      heap_list <- out[[1]]
      index_list <- out[[2]]

      # Putting all leaf nodes together to re-assign later
      new_leaf_values <- c(new_leaf_values, index_list)

      # Check convergence
      if (round((length(
        setdiff(old_leaf_values, new_leaf_values)
      ) / length(new_leaf_values)), 4) <= 0.01) {
        print(paste("Convergence at iteration", counter))
        break
      }

      # Check iterations
      else if (counter == iteration_count) {
        print("Maximum iterations reached. Halting.")
        break
      }

      old_leaf_values <- new_leaf_values
      index_list <- new_leaf_values
      counter <- counter + 1
    }

    output = list(
      prob = weights,
      'meu' = meu,
      'sigma' = sigma,
      'prior' = prior
    )
    return(output)

  }
