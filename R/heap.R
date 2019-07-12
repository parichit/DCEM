# test_data1 = list(list(0,4), list(4,5), list(9,0), list(5,7))

insert_node <- function(data, node){

  # if (node[[1]] < heap[[length(heap)]][[1]]){
  #   print("New value less than the leaf, can't be inserted.")
  #   return(heap)
  # }
  data = rbind(data, node)
  i = nrow(data)

  while( (i>1)  && (data[floor(i/2), 1] <  data[i, 1])){
    temp = data[i, ]
    data[i, ] = data[floor(i/2), ]
    data[floor(i/2), ] = temp
    i = floor(i/2)
  }
  return(data)
}



# Function to remove the leaf node from the heap.
remove_node <- function(data, node_key, index){

  if (nrow(data) == 0){
    print(paste("Heap empty, can't remove the node.", index))
    stop('Exiting: Heap node removal error.')
    #return(data)
  }

  data = data[-c(which(data[, 1] == node_key)), ]
  return(data)
}



# Function to return the matrix of leaf nodex
# keys, values in 1 and 2nd columns
get_leaves <- function(data) {

  if (nrow(data) == 0){
    print("Heap empty, can't get leaves.")
    return(data)
  }

  leaf_start = floor(nrow(data)/2) + 1
  leaf_end = nrow(data)

  return(data[leaf_start:leaf_end,])

}



# Function to get the right child
get_right <-function(index){
  return((2*index)+ 1)
}



# Function to get the left child
get_left <-function(index){
  return(2*index)
}



max_heapify <- function(data, index, N){

  left = get_left(index)
  right = get_right(index)
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

# Function to build the heap structure
build_heap <- function(test_data){

  if (nrow(test_data) == 1){
  print("only 1 element in the heap")
  return(test_data)
  }

  i = floor(nrow(test_data)/2)
  while (i>=1){
    test_data = max_heapify(test_data, i, nrow(test_data))
    i = i-1
  }
  return(test_data)
}

# test_data = data.frame(keys=c(1,0,4,9,5), vals=c(2,4,5,0,7))
# print(test_data)
# data = build_heap(test_data)
# data = build_heap(data)
# print(data)

# leaves = get_leaves(data)
# print(leaves)
#
# d = remove_node(data, 0)
# print(d)
#
# data = build_heap(test_data1)
# data = build_heap(data)
#print(matrix(unlist(data), ncol=2, byrow=TRUE))

# data = list()
#
# #list(0,4), list(4,5), list(9,0), list(5,7)
#
# data = insert_node(data, list(1,2))
# #print(matrix(unlist(data), ncol=2, byrow=TRUE))
# data = insert_node(data, list(9,0))
# data = insert_node(data, list(5,7))
# data = insert_node(data, list(4,5))
# data = insert_node(data, list(0,4))
#
# print(matrix(unlist(data), ncol=2, byrow=TRUE))
