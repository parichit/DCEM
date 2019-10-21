# census_data <- read.csv2(file="/Users/schmuck/Documents/Box Sync/Courses/Spring2018/Independent Study/gmailgithub/DCEM/census.txt",
#                          sep=",", header=FALSE, stringsAsFactors = FALSE)
#
# dim(census_data)
# # Remove the label column,
# census_data = census_data[-c(1),-c(7)]
# census_data <- na.omit(census_data)
#
# # Drop rows containig empty/NA values
# dim(census_data)
# census_data = apply(census_data, 2, as.numeric)
#
#
# tic(); em_c_out = dcem_train(census_data, iteration_count = 1000, num_clusters = 5, threshold = 0.000000001) ; toc()
# tic(); star_c_out = dcem_star_train(census_data, iteration_count = 100, num_clusters = 5) ; toc()


#a = em_c_out$prob ; a = data.frame(a) ; b = apply(a, 2, which.max) ; length(b[b==1]) ; length(b[b==2]);
# length(b[b==3])
# a =  rep(2,5)
# b = rep(2,5)
# c =  diag(5)
# print(a)
# print(b)

# # sample_mv_data = as.data.frame(rbind(MASS::mvrnorm(n=5, a, Sigma = c), MASS::mvrnorm(n=10, b, c)))
# # sample_mv_out = dcem_star_train(sample_mv_data, iteration_count = 100, num_clusters = 2)

#sample_uv_data = as.data.frame(c(rnorm(50000, 20, 15), rnorm(10000, 70, 10)))
#sample_uv_data = as.data.frame(sample_uv_data[sample(nrow(sample_uv_data)),])

# em_uv_out = dcem_train(sample_uv_data, num_clusters = 2, iteration_count = 100, threshold = 0.000001)
# a = em_uv_out$prob ; a = data.frame(a) ; b = apply(a, 2, which.max) ; length(b[b==1]) ; length(b[b==2]); length(b[b==3])
#
# star_uv_out = dcem_star_train(sample_uv_data, num_clusters = 2, iteration_count = 100)
# a = star_uv_out$prob ; a = data.frame(a) ; b = apply(a, 2, which.max) ; length(b[b==1]) ; length(b[b==2]); length(b[b==3])

