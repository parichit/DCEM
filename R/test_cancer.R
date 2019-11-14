# library("tictoc")
# #Load the dataset.
# cancer_data <- read.csv2(file="/Users/schmuck/Documents/Box Sync/Courses/Spring2018/Independent Study/gmailgithub/DCEM/breastcancer.txt",
#                        sep=",", header=FALSE, stringsAsFactors = FALSE)
# # Store the labels.
# labels = cancer_data[,11]
# # Remove the ID and label column,
# cancer_data = cancer_data[,-c(1,11)]
# # Replacing the ? with "NA"
# cancer_data[cancer_data=="?"] <- NA
# # Drop rows containig empty/NA values
# cancer_data <- na.omit(cancer_data)
# dim(cancer_data)
# cancer_data = apply(cancer_data, 2, as.numeric)
# star_time = c()
# dcem_time = c()
# clusters = c(10, 20, 30, 35)
# #clusters = c(2, 5)
#
# for (i in clusters){
#   #print(paste("K = ", i))
#   tic()
#   star_c_out = dcem_star_train(cancer_data, iteration_count = 1000, num_clusters = i)
#   t<-toc()
#   y<-t$toc - t$tic
#   star_time <- c(star_time, y)
#   a = star_c_out$prob ; a = data.frame(a) ; b = apply(a, 2, which.max) ; length(b[b==1]) ; length(b[b==2])
# }
# #
# for (i in clusters){
#   tic()
#   em_c_out = dcem_train(cancer_data, iteration_count = 1000, num_clusters = i, threshold = 0.0000001)
#   m<-toc()
#   f<-m$toc - m$tic
#   dcem_time <- c(dcem_time, f)
#   a = em_c_out$prob ; a = data.frame(a) ; b = apply(a, 2, which.max) ; length(b[b==1]) ; length(b[b==2])
# }
# print(star_time)
# print(dcem_time)

