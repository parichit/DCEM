source("./R/dcem_train.R")

#' dcem_test: Part of DCEM package.
#'
#' For demonstrating the execution on the bundled dataset.
#'
#' @section Details:
#'
#' The dcem_test performs the following steps in order:
#'
#' \enumerate{
#'
#'     \item Read the data from the disk (from the file data/ionosphere_data.csv). The data folder is under the
#'     package installation folder. \item The dataset details can be see by typing \code{\link{ionosphere_data}} in
#'     R-console or at \url{https://archive.ics.uci.edu/ml/datasets/ionosphere}.
#'
#'      \item Clean the data (by removing the columns). \strong{The data should be cleaned
#'      before use.} Refer \strong{\code{\link{trim_data}}} to see what columns
#'      should be removed and how. The package provides the basic interface for removing
#'      columns.
#'
#'      \item Call the \code{\link{dcem_train}} on the cleaned data.
#' }
#'
#' @section Accessing the output parameters:
#'
#' The function dcem_test() calls dcem_train() that returns a list of objects.
#' This list contains parameters associated with the Gaussian
#' (posterior probabilities, mean, covariance/standard-deviation and priors). The
#' parameters can be accessed as follows where sample_out is the list containing
#' the output:
#'
#'\enumerate{
#'         \item [1] Posterior Probabilities: \strong{sample_out$prob}
#'         A matrix of posterior-probabilities
#'
#'         \item [2] Mean(s): \strong{sample_out$mean}
#'
#'         For multivariate data: It is a matrix of means for the Gaussian(s). Each row in
#'         the  matrix corresponds to a mean for the Gaussian.
#'
#'         For univariate data: It is a vector of means. Each element of the vector
#'         corresponds to one Gaussian.
#'
#'         \item [3] Co-variance matrices: \strong{sample_out$cov}
#'
#'         For multivariate data: List of co-variance matrices for the Gaussian(s).
#'
#'         Standard-deviation: \strong{sample_out$sd}
#'
#'         For univariate data: Vector of standard deviation for the Gaussian(s))
#'
#'         \item [4] Priors: \strong{sample_out$prior}
#'         A vector of priors for the Gaussian(s).
#'         }
#'
#' @usage
#' dcem_test()
#'
#' @references
#' Using data to build a better EM: EM* for big data.
#'
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#'(2016) <doi:https://doi.org/10.1007/s41060-017-0062-1>.
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.

dcem_test <- function()
{

# Setting the filepath to read from the bundled csv file.
data_file =  paste(trimws(getwd()),"/data/","ionosphere_data.csv",sep = "")

# Reading the input file into a dataframe.
  ionosphere_data = read.csv2(
  file = data_file,
  sep = ",",
  header = FALSE,
  stringsAsFactors = FALSE
)

# Cleaning the data by removing the 35th and 2nd column as they contain the labels and 0's respectively.
ionosphere_data =  trim_data("2, 35", ionosphere_data)

# Calling the dcem_train() function with the cleaned dataset.
sample_out = dcem_train(ionosphere_data, 0.001, 200, 2)

# Return the list containing the output parameters.
return(sample_out)
}




