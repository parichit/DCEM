#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix insert_node(NumericMatrix heap, NumericVector node){

  int key=0, val=0;
  int i = heap.nrow();

  NumericVector key1 = heap(_, 0);
  NumericVector val1 = heap(_, 1);

  key1.push_back(node(0));
  val1.push_back(node(1));

  while( (i>0)  && (key1(floor(i/2)) <  key1(i))){

    key = key1(i);
    val = val1(i);

    key1(i) = key1(floor(i/2));
    val1(i) = val1(floor(i/2));

    key1(floor(i/2)) = key;
    val1(floor(i/2)) = val;

    i = floor(i/2);
  }

  NumericMatrix heap1(key1.length(), 2);
  heap1(_, 0) = key1;
  heap1(_, 1) = val1;

  return heap1 ;
}


// [[Rcpp::export]]
NumericMatrix remove_node(NumericMatrix heap, int node_key){

  if (heap.nrow() == 0){
    std::cout << "Heap empty, can't remove the node." << std::endl;
    exit(1);
  }

  NumericVector key = heap(_, 0);
  NumericVector val = heap(_, 1);

  for(int j=0; j<=key.length();j++){
    if (key[j] == node_key){
      key.erase(j);
      val.erase(j);
      break;
    }
  }

  NumericMatrix heap1(key.length(), 2);
  heap1(_, 0) = key;
  heap1(_, 1) = val;

  return heap1;
}


// [[Rcpp::export]]
NumericMatrix get_leaves(NumericMatrix heap) {

  if (heap.nrow() == 0){
    std::cout << "Heap empty, can't get leaves." << std::endl;
    return heap;
  }

  int leaf_start = floor(heap.rows()/2);
  int leaf_end = heap.rows()-1 ;

  NumericMatrix leaf_data = heap( Range(leaf_start, leaf_end), _ );

  return leaf_data;
}


// [[Rcpp::export]]
NumericMatrix max_heapify(NumericMatrix data, int index, int N){

                    int left = 2*index + 1;
                    int right = (2*index) + 2;
                    int largest = index;
                    int key=0, val=0 ;


                    if ( (left <= N) && (data(left, 0) > data(largest, 0)) ){
                      largest = left;
                    }

                    if ( (right <= N) && (data(right, 0) > data(largest, 0)) ){
                      largest = right;
                    }

                    if (largest != index)
                    {
                      key  = data(largest, 0);
                      val  = data(largest, 1);

                      data(largest, 0) = data(index, 0);
                      data(largest, 1) = data(index, 1);

                      data(index, 0) = key;
                      data(index, 1) = val;

                      max_heapify(data, largest, N);
                    }

                    return data;
                  }


// [[Rcpp::export]]
NumericMatrix build_heap(NumericMatrix data){

                    if (data.nrow() == 1){
                      Rcout << "only 1 element in the heap" << std::endl ;
                      return data;
                    }

                    int i = floor((data.nrow())/2);

                    while (i>=0){
                      data = max_heapify(data, i, data.nrow()-1);
                      i = i-1 ;
                    }

                    return data;
                  }


