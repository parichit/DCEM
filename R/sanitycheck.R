#' validate_data: Part of DCEM package. Used internally in the package.
#'
#' Implements sanity check for the input data. This function is for internal use and is called
#' by the \code{\link{dcem_train}}.
#'
#' An example would be to check if the column to be removed exist
#' or not? \code{\link{trim_data}} internally calls this function before removing
#' the column(s).
#'
#' @param columns (string): A comma separated
#' list of columns that needs to be removed from the dataset. Default: ''
#'
#' @param numcols (numeric): Number of columns in the dataset.
#'
#' @usage
#' validate_data(columns, numcols)
#'
#' @references
#' Using data to build a better EM: EM* for big data.
#'
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#'(2016) <https://doi.org/10.1007/s41060-017-0062-1>.
#'
#' @return
#' boolean: TRUE if the columns exists otherwise FALSE.
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#' This work is partially supported by NCI Grant 1R01CA213466-01.

validate_data <- function(columns, numcols){

  # Check if the user specified columns exists or not
  if(columns != "")
  {
    if(grepl(",", columns, fixed = TRUE)){

      list_of_columns =  sort(strtoi(unlist(strsplit(columns,","))))
      for (i in list_of_columns){
        if(i < 1 || i > numcols)
        {
          print(paste("The specified column to be removed: ", list_of_columns[i], "does not exist in the data."))
          return(FALSE)
        }
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

#' trim_data: Part of DCEM package. Used internally in the package.
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
#' @references
#' Using data to build a better EM: EM* for big data.
#'
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#' (2016) <https://doi.org/10.1007/s41060-017-0062-1>.
#'
#' @return
#' A dataframe with the specified column(s) removed from it.
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#' This work is partially supported by NCI Grant 1R01CA213466-01.

trim_data <- function(columns, data){

  numcols = ncol(data)
  # Check if the user specified columns exists or not
  status = validate_data(columns, numcols)

  if(!status){
    print("Error in trim_data(): Please check if the correct columns are specified for removal.");
    return(status)
  }

  if(columns != ""){

    # Remove the user speciifed columns
    if(grepl(",",columns, fixed = TRUE)){
      list_of_columns =  c(strtoi(sort(unlist(strsplit(columns, ",")))))
      data = data[,-list_of_columns]
    }
    else{
      data = data[,-strtoi(columns)]
    }
  }
  return(data)
}
