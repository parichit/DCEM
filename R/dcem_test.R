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
#'     R-console or at \url{http://archive.ics.uci.edu/ml/datasets/Ionosphere}.
#'
#'      \item Clean the data (by removing the columns). \strong{The data should be cleaned
#'      before use.} Refer \strong{\code{\link{trim_data}}} to see what columns
#'      should be removed and how. The package provides the basic interface for removing
#'      columns.
#'
#'      \item Call the \code{\link{dcem_star_train}} on the cleaned data.
#' }
#'
#' @section Accessing the output parameters:
#'
#' The function dcem_test() calls the \code{\link{dcem_star_train}}.
#' It returns a list of objects as output. This list contains estimated
#' parameters of the Gaussian (posterior probabilities, meu, sigma and prior). The
#' parameters can be accessed as follows where sample_out is the list containing
#' the output:
#'
#'\enumerate{
#'         \item (1) Posterior Probabilities: \strong{sample_out$prob}
#'         A matrix of posterior-probabilities
#'
#'         \item (2) Meu: \strong{meu}
#'
#'         For multivariate data: It is a matrix of meu(s). Each row in
#'         the  matrix corresponds to one meu.
#'
#'         \item (3) Co-variance matrices: \strong{sample_out$sigma}
#'
#'         For multivariate data: List of co-variance matrices for the Gaussian(s).
#'
#'         Standard-deviation: \strong{sample_out$sigma}
#'
#'         For univariate data: Vector of standard deviation for the Gaussian(s))
#'
#'         \item (4) Priors: \strong{sample_out$prior}
#'         A vector of prior.
#'
#'         \item (5) Membership: \strong{membership}: A vector of cluster
#'         membership for data.
#'         }
#'
#' @usage
#' dcem_test()
#'
#' @references
#' Using data to build a better EM: EM* for big data.
#'
#' Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic
#'(2016) <https://doi.org/10.1007/s41060-017-0062-1>.
#'
#' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mark Jenne, Mehmet Dalkilic
#'
#' This work is partially supported by NCI Grant 1R01CA213466-01.
#' @export

dcem_test <- function()
{

  # Setting the filepath to read from the bundled csv file.
  data_file =  file.path(trimws(getwd()),"data","ionosphere_data.csv")

  # Reading the input file into a dataframe.
  ionosphere_data = read.csv2(
    file = data_file,
    sep = ",",
    header = FALSE,
    stringsAsFactors = FALSE
  )

  # Cleaning the data by removing the 35th and 2nd column as they contain the labels and 0's respectively.
  ionosphere_data =  trim_data("2, 35", ionosphere_data)

  # Calling the dcem_star_train() function with the cleaned dataset.
  sample_out = dcem_star_train(ionosphere_data)

  # Return the list containing the output parameters.
  return(sample_out)
}




