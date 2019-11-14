# library(tictoc)
# census_data <- read.csv2(file="/Users/schmuck/Documents/Box Sync/Courses/Spring2018/Independent Study/gmailgithub/DCEM/census.txt",sep=",", header=FALSE, stringsAsFactors = FALSE)
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
# star_time = c()
# dcem_time = c()
# clusters = c(20, 30)
#
# for (i in clusters){
#   tic();
#   star_c_out = dcem_star_train(census_data, iteration_count = 100, num_clusters = i)
#   t<-toc()
#   y<-t$toc - t$tic
#   star_time <- c(star_time, y)
# }
#
#  for (i in clusters){
#   tic()
#   em_c_out = dcem_train(census_data, iteration_count = 1000, num_clusters = i, threshold = 0.0001)
#   m<-toc()
#   f<-m$toc - m$tic
#   dcem_time <- c(dcem_time, f)
# }
# #
# print(star_time)
# print(dcem_time)

