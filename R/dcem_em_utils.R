expectation_uv <- function(data, weights, meu, sigma, prior, num_clusters, tolerance){

  for (clus in 1:num_clusters) {
    weights[clus, ] <- dnorm(data, meu[clus] , sigma[clus]) * prior[clus]
  }

  sum_weights <- colSums(weights)
  weights <- sweep(weights, 2, sum_weights, '/')

  weights[is.nan(weights)] <- tolerance
  weights[weights <= 0.0] <- tolerance

  return(weights)
}


maximisation_uv <- function(data, weights, meu, sigma, prior, num_clusters, num_data){

  for (clus in 1:num_clusters) {
  prior[clus] = sum(weights[clus, ]) / num_data
  meu[clus] = (sum(data * weights[clus, ]) / sum(weights[clus, ]))
  sigma[clus] = sqrt(sum(((data - meu[clus]) ^ 2) * weights[clus, ]) / sum(weights[clus, ]) )
  }

  return(list(meu=meu, sigma=sigma, prior=prior))
}

expectation_mv <- function(data, weights, meu, sigma, prior, num_clusters, tolerance){

  for (clus in 1:num_clusters) {
    weights[clus, ] <- dmvnorm(data, meu[clus, ] , sigma[[clus]]) * prior[clus]
  }

  sum_weights <- colSums(weights)
  weights <- sweep(weights, 2, sum_weights, '/')

  weights[is.nan(weights)] <- tolerance
  weights[weights <= 0.0] <- tolerance

  return(weights)
}


maximisation_mv <- function(data, weights, meu, sigma, prior, num_clusters, num_data){

  meu <- weights %*% data
  meu <- meu / rowSums(weights)
  prior <- rowSums(weights) / num_data

  for (clus in 1:num_clusters) {

    sigma[[clus]] = 0
    temp = stats::cov.wt(
      data,
      weights[clus, ],
      cor = TRUE
    )$cov

    # Take care of the singularity condition.
    if (matrixcalc::is.singular.matrix(temp)) {
      diag(temp) <- diag(temp) + 0.000000000000001
    }
    sigma[[clus]] <- temp
  }

  return(list("meu"=meu, "sigma"=sigma, 'prior'=prior))
}


update_weights <- function(temweights, weights, index_list, num_clusters){

  for (clus in 1:num_clusters) {
    weights[clus, index_list] = temweights[clus, ]
  }

  return(weights)
}
