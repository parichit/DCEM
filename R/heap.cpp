#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List c_insert_node(List heap_list, NumericVector heap_assn, NumericVector data_prob, NumericVector leaf_ind){

  int key=0, val=0;
  NumericVector key1;
  NumericVector val1;

  //Rcout << heap_assn(0) << "\n";

  //return heap_list;
  //Rprintf("total leaves: %d\n", leaf_ind.length());

  for (int k=0; k<leaf_ind.length(); k++)
  {

  NumericMatrix heap = heap_list[heap_assn(k)];

  int i = heap.ncol();
  key1 = heap(0, _);
  val1 = heap(1, _);

  key1.push_back(data_prob(k));
  val1.push_back(leaf_ind(k));

  Rcout << "11111" << "\n";

  //Rprintf("new_heap: %d, key: %f, val: %d\n", heap_assn(k), data_prob(k), leaf_ind(k));
  Rcout << heap_assn(k) << "\n";
  Rcout << leaf_ind(k) << "\n" ;

  Rcout << "2222" << "\n";

  while( i>0  && key1(floor(i/2)) < key1(i)){

    key = key1(i);
    val = val1(i);

    key1(i) = key1(floor(i/2));
    val1(i) = val1(floor(i/2));

    key1(floor(i/2)) = key;
    val1(floor(i/2)) = val;

    i = floor(i/2);
  }

  NumericMatrix heap1(2, key1.length());
  heap1(0, _) = key1;
  heap1(1, _) = val1;

  heap_list[heap_assn(k)] = heap1;

  printf("in function 6");

  }

  printf("in function 7");

  return heap_list;
}

// NumericMatrix cpp_remove_node(NumericMatrix heap, NumericVector indices_to_remove){
//
//     int leaf_start = floor(heap.rows()/2);
//
//     NumericMatrix orig_mat = heap(Range(0, leaf_start-1), _);
//     NumericMatrix leaf_matrix = heap(Range(leaf_start, heap.rows()-1), _);
//
//     NumericVector leaf_key = leaf_matrix(_, 0);
//     NumericVector leaf_val = leaf_matrix(_, 1);
//
//     for(int j=0; j<=indices_to_remove.length(); j++){
//       leaf_key.erase(indices_to_remove(j));
//       leaf_val.erase(indices_to_remove(j));
//     }
//
//     int len = orig_mat.rows() + leaf_mat.rows() -2;
//
//     NumericMatrix new_leaves(, 2);
//
//     heap1(Range(0, leaf_start-1), _) = orig_mat;
//     heap1(Range(leaf_start, len), _) = heap( Range(leaf_start, len), _);
//
//     return heap1;


  // NumericVector key = leaves(_, 0);
  // NumericVector val = leaves(_, 1);
  //
  // for(int j=0; j<=indices_to_remove.length(); j++){
  //     key.erase(indices_to_remove(j));
  //     val.erase(indices_to_remove(j));
  // }
  //
  // NumericMatrix new_leaves(key.length(), 2);
  // new_leaves(_, 0) = key;
  // new_leaves(_, 1) = val;
  //
  // return new_leaves;
//}


// [[Rcpp::export]]
NumericMatrix c_get_leaves(NumericMatrix heap) {

  if(heap.nrow() == 1 || heap.nrow() == 0){
    return heap;
  }

  int leaf_start = floor(heap.rows()/2);
  int leaf_end = heap.rows()-1 ;

  NumericMatrix leaf_data = heap( Range(leaf_start, leaf_end), _ );

  return leaf_data;
}



// NumericMatrix c_max_heapify(NumericMatrix data, int index, int N){
//
//   int left = 2*index + 1;
//   int right = (2*index) + 2;
//   int largest = index;
//   int key=0, val=0 ;
//
//
//   if ( (left <= N) && (data(left, 0) > data(largest, 0)) ){
//     largest = left;
//   }
//
//   if ( (right <= N) && (data(right, 0) > data(largest, 0)) ){
//     largest = right;
//   }
//
//   if (largest != index)
//   {
//     key  = data(largest, 0);
//     val  = data(largest, 1);
//
//     data(largest, 0) = data(index, 0);
//     data(largest, 1) = data(index, 1);
//
//     data(index, 0) = key;
//     data(index, 1) = val;
//
//     data = c_max_heapify(data, largest, N);
//   }
//
//   return data;
// }



// [[Rcpp::export]]
NumericMatrix c_max_heapify(NumericMatrix data, int index, int N){

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
    key  = data(0, largest);
    val  = data(1, largest);

    data(0, largest) = data(0, index);
    data(1, largest) = data(1, index);

    data(0, index) = key;
    data(1, index) = val;

    data = c_max_heapify(data, largest, N);
  }

  return data;
}


// NumericMatrix c_build_heap(NumericMatrix data){
//
//   if (data.nrow() == 1){
//     Rcout << "only 1 element in the heap" << std::endl;
//     return data;
//   }
//
//   int i = floor((data.nrow())/2);
//   while (i>=0){
//     data = c_max_heapify(data, i, data.nrow()-1);
//     i = i-1 ;
//   }
//   return data;
// }


// [[Rcpp::export]]
NumericMatrix c_build_heap(NumericMatrix data){

  if (data.ncol() == 1){
    Rcout << "only 1 element in the heap" << std::endl;
    return data;
  }

  int i = floor((data.ncol())/2);
  while (i>=0){
    data = c_max_heapify(data, i, data.ncol()-1);
    i = i-1 ;
  }
  return data;
}

// [[Rcpp::export]]
NumericVector c_separate_data(NumericMatrix heap){

  // if(heap.nrow() == 1 || heap.nrow() == 0){
  //   //NumericMatrix leaves = heap(0, 0);
  //   //List out = List::create(heap, leaves);
  //   return heap(_, 1);
  // }

  if(heap.ncol() == 1 | heap.ncol() == 0){
    //NumericMatrix leaves = heap(0, 0);
    //List out = List::create(heap, leaves);
    return heap(1, _);
  }

  // int leaf_start = floor(heap.rows()/2);
  // int leaf_end = heap.rows()-1;

  int leaf_start = floor(heap.cols()/2);
  int leaf_end = heap.cols()-1;

  //NumericMatrix new_heap = heap( Range(0, leaf_start-1), _);
  // NumericMatrix leaves = heap( Range(leaf_start, leaf_end), _);
  // NumericVector leaf_index = leaves(_, 1);

  NumericMatrix leaves = heap(_, Range(leaf_start, leaf_end));
  NumericVector leaf_index = leaves(1, _);

  //List out = List::create(new_heap, leaf_index);
  //printf("success");
  return leaf_index;
}
