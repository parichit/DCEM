#' insert_node: Part of DCEM package.
#'
#' Implements node insertion in the heap structure. This function is for internal use
#' and is called by the \code{\link{dcem_star_cluster_uv}} (univariate data) and
#' \code{\link{dcem_star_cluster_mv}} (multivariate data).
#'
#' @param heap (dataframe): The dataframe holding the heap structure.
#'
#' @param node (vector): A vector containing the key, value pair.
#'
#' @usage
#' insert_node(heap, node)
#'
#' @examples
#'
#' # Create a sample data frame
#' heap <- data.frame(keys=c(0), vals=c(7))
#'
#' # Build the heap from the data frame.
#' heap <- build_heap(heap)
#'
#' # Insert a new node in the heap.
#' heap <- insert_node(heap, c(1,2))
#'
#' @references
#' Using data to build a better EM: EM* for big data.
#'
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#'(2016) <https://doi.org/10.1007/s41060-017-0062-1>.
#'
#' @return
#' data: The heap with the new node.
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#' This work is partially supported by NCI Grant 1R01CA213466-01.

insert_node <- function(heap, node){

  data = rbind(heap, node)
  i = nrow(heap)

  while( (i>1)  && (heap[floor(i/2), 1] <  heap[i, 1])){
    temp = heap[i, ]
    heap[i, ] = heap[floor(i/2), ]
    heap[floor(i/2), ] = temp
    i = floor(i/2)
  }
  return(heap)
}


#' remove_node: Part of DCEM package.
#'
#' Removes the given node from the heap structure. This function is for internal use and is called
#' by the \code{\link{dcem_star_cluster_uv}} (univariate data) and
#' \code{\link{dcem_star_cluster_mv}} (multivariate data).
#'
#' @param heap (dataframe): The dataframe holding the heap structure.
#'
#' @param node_key (numeric): The key corresponding to the node to be removed.
#'
#' @param index (numeric): The index of the node to be removed.
#'
#' @usage
#' remove_node(heap, node_key, index)
#'
#' @examples
#' # Create a sample data frame
#' heap <- data.frame(keys=c(0,3,4), vals=c(7,7,4))
#'
#' # Build the heap from the data frame.
#' heap <- build_heap(heap)
#'
#' # Remove a node from the heap (key=3).
#' remove_node(heap, 3, 2)
#'
#' @references
#' Using data to build a better EM: EM* for big data.
#'
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#'(2016) <doi:https://doi.org/10.1007/s41060-017-0062-1>.
#'
#' @return
#' data frame: The heap with node removed.
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#' This work is partially supported by NCI Grant 1R01CA213466-01.

remove_node <- function(heap, node_key, index){

  if (nrow(heap) == 0){
    print(paste("Heap empty, can't remove the node.", index))
    stop('Exiting: Heap node removal error.')
    #return(data)
  }

  heap = heap[-c(which(heap[, 1] == node_key)), ]
  return(heap)
}

#' get_leaves: Part of DCEM package.
#'
#' Return the leaf nodes from the heap structure. This function is for internal use and is called
#' by the \code{\link{dcem_star_cluster_uv}} (univariate data) and
#' \code{\link{dcem_star_cluster_mv}} (multivariate data).
#'
#' @param heap (dataframe): The dataframe holding the heap structure.
#'
#' @usage
#' get_leaves(heap)
#'
#' @examples
#' # Create a sample data frame
#' heap <- data.frame(keys=c(0,3,4), vals=c(7,7,4))
#'
#' # Build the heap from the data frame.
#' heap <- build_heap(heap)
#'
#' # Remove a node from the heap (key=3).
#' get_leaves(heap)
#'
#' @references
#' Using data to build a better EM: EM* for big data.
#'
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#'(2016) <doi:https://doi.org/10.1007/s41060-017-0062-1>.
#'
#' @return
#' data frame: A data frame containing the key, value pairs for the leaves.
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#' This work is partially supported by NCI Grant 1R01CA213466-01.

get_leaves <- function(heap) {

  if (nrow(heap) == 0){
    print("Heap empty, can't get leaves.")
    return(heap)
  }

  leaf_start = floor(nrow(heap)/2) + 1
  leaf_end = nrow(heap)

  return(heap[leaf_start:leaf_end,])

}

#' max_heapify: Part of DCEM package.
#'
#' Implements the max-heapify process to create the heap. This function is for internal
#' use and is called by the \code{\link{dcem_star_cluster_uv}} (univariate data) and
#' \code{\link{dcem_star_cluster_mv}} (multivariate data).
#'
#' @param data (dataframe): The dataframe containing the key, value pairs.
#'
#' @param index (numeric): Index of the node.
#'
#' @param N (numeric): Size of the heap.
#'
#' @usage
#' max_heapify(data, index, N)
#'
#' @examples
#'
#' # Create a sample data frame
#' heap <- data.frame(keys=c(0,3,4,5), vals=c(7,7,4,6))
#'
#' # Build the heap from the data frame.
#' heap <- build_heap(heap)
#'
#' # Call max-heapify untill the last node
#'
#' i = floor(nrow(heap)/2)
#' while (i>=1){
#'  heap = max_heapify(heap, i, nrow(heap))
#'  i = i-1
#' }
#'
#' @references
#' Using data to build a better EM: EM* for big data.
#'
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#'(2016) <doi:https://doi.org/10.1007/s41060-017-0062-1>.
#'
#' @return
#' data frame: The data frame containing the heap.
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#' This work is partially supported by NCI Grant 1R01CA213466-01.

max_heapify <- function(data, index, N){

  left = 2*index
  right = (2*index)+ 1
  largest = index

  if ( (left <= N) && (data[left, 1] > data[largest, 1])){
    largest = left
  }

  if ( (right <= N) && (data[right, 1] > data[largest, 1])){
    largest = right
  }

  if (largest != index){
    temp = data[largest, ]
    data[largest, ] = data[index, ]
    data[index, ] = temp
    max_heapify(data, largest, N)
  }
  return(data)
}


#' build_heap: Part of DCEM package.
#'
#' Create the heap from the data frame. This function is for internal use and is called
#' by the \code{\link{dcem_star_cluster_uv}} (univariate data) and
#' \code{\link{dcem_star_cluster_mv}} (multivariate data).
#'
#' @param data (dataframe): The dataframe containing the key, value pairs.
#'
#' @usage
#' build_heap(data)
#'
#' @examples
#' # Create a sample data frame
#' heap <- data.frame(keys=c(0,3,4), vals=c(7,7,4))
#'
#' # Build the heap from the data frame.
#' heap <- build_heap(heap)
#'
#' @references
#' Using data to build a better EM: EM* for big data.
#'
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#'(2016) <doi:https://doi.org/10.1007/s41060-017-0062-1>.
#'
#' @return
#' data frame: The dataframe containing the heap.
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#' This work is partially supported by NCI Grant 1R01CA213466-01.

build_heap <- function(data){

  if (nrow(data) == 1){
  print("only 1 element in the heap")
  return(data)
  }

  i = floor(nrow(data)/2)
  while (i>=1){
    data = max_heapify(data, i, nrow(data))
    i = i-1
  }
  return(data)
}

