#' validate_data: Part of DCEM package.
#'
#' Implements sanity check for the input data. This function is for internal use and is called
#' by the \code{\link{dcem_train}}.
#'
#' An example would be to check if the column to be removed exist
#' or not? \code{\link{trim_data}} internally calls this function before removing
#' the column(s).
#'
#' @param columns (string): A comma separated
#' list of columns that needs to be removed from the dataset.
#' Default: ''
#'
#' @param numcols (numeric): Number of columns in the dataset.
#'
#' @usage
#' validate_data(columns, numcols)
#'
#' @examples
#' #Generate a dataframe with 2 columns containing random values.
#'
#' # Check a range of columns.
#'
#' validate_data("2,3,4", ncol(as.data.frame(cbind(sample(1:100,10),
#' sample(500:1000, 10), sample(-100:0,10)))))
#'
#' # Check a single column.
#'
#' validate_data("2", ncol(as.data.frame(cbind(sample(1:100,10),
#' sample(500:1000, 10)))))
#'
#' @references
#' Using data to build a better EM: EM* for big data.
#'
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#'(2016) <doi:https://doi.org/10.1007/s41060-017-0062-1>.
#'
#' @return
#' boolean: TRUE if the columns exists otherwise FALSE.
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#' This work is partially supported by NCI Grant 1R01CA213466-01.

validate_data <- function(columns, numcols){

  if(columns != "")
  {
    if(grepl(",", columns, fixed = TRUE)){

      list_of_columns =  sort(strtoi(unlist(strsplit(columns,","))))

      if(list_of_columns[1] < 1 || list_of_columns[length(list_of_columns)] > numcols)
      {
        print(paste("The column range", list_of_columns[1], "-", list_of_columns[length(list_of_columns)],
                    "does not exist in the dataset."))
        return(FALSE);
      }
      return(TRUE)
    }

    if(strtoi(columns) > numcols || strtoi(columns) < 1){
      print(paste("The column", columns, "does not exist in the dataset."))
      return(FALSE);
    }

  }
  else
  {
    return(FALSE)
  }

  return(TRUE)
}

#' trim_data: Part of DCEM package.
#'
#' Removes the specified column(s) from the dataset.
#'
#' @param columns (string): A comma separated
#' list of column(s) that needs to be removed from the dataset.
#' Default: ''
#'
#' @param data (dataframe): Dataframe containing the input data.
#'
#' @usage
#' trim_data(columns, data)
#'
#' @examples
#' # Remove a range of columns. Generally, the columns containing the labels or
#' redundant values (such as all 0's) should be removed before training the model.
#'
#' trim_data("1,2", ncol(as.data.frame(cbind(sample(1:100,10),
#' sample(500:1000, 10), sample(-100:0,10)))))
#'
#' # Remove a single column.
#'
#' trim_data("2", ncol(as.data.frame(cbind(sample(1:100,10),
#' sample(500:1000, 10), sample(-100:0,10)))))
#'
#' @references
#' Using data to build a better EM: EM* for big data.
#'
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#' (2016) <doi:https://doi.org/10.1007/s41060-017-0062-1>.
#'
#' @return
#' A dataframe with the specified column(s) removed from it.
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#' This work is partially supported by NCI Grant 1R01CA213466-01.

trim_data <- function(columns, data){

  numcols = ncol(data)
  status = validate_data(columns, numcols)

  if(!status){
    print("Error in trim_data(): Please check if the correct columns are specified for removal.");
    return(status)
  }

  if(columns != ""){

      if(grepl(",",columns, fixed = TRUE)){
        list_of_columns =  strtoi(sort(unlist(strsplit(columns, ","))))
        data = data[,-list_of_columns]
      }
      else{
        data = data[,-strtoi(columns)]
      }
  }

  return(data)
}
