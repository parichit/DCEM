# library('DCEM')
# library("tictoc")
# #Load the dataset.
# cancer_data <- read.csv2(file="/Users/schmuck/Documents/Box Sync/Courses/Spring2018/Independent Study/gmailgithub/DCEM/breastcancer.txt",
#                          sep=",", header=FALSE, stringsAsFactors = FALSE)
# # Store the labels.
# labels = cancer_data[,11]
#
# # Remove the ID and label column,
# cancer_data = cancer_data[,-c(1,11)]
#
# # Replacing the ? with "NA"
# cancer_data[cancer_data=="?"] <- NA
#
# # Drop rows containig empty/NA values
# cancer_data <- na.omit(cancer_data)
# dim(cancer_data)
#
# cancer_data = apply(cancer_data, 2, as.numeric)
#
# star_time = c()
# dcem_time = c()
#
# clusters = c(2,5,10,15,20,25)
#
# for (i in clusters){
#   print(paste("K = ", i))
#   tic()
#   star_c_out = dcem_star_train(cancer_data, iteration_count = 1000, num_clusters = i)
#   t<-toc()
#   t<-t$toc - t$tic
#   star_time <- rbind(star_time, t)
#   a = star_c_out$prob ; a = data.frame(a) ; b = apply(a, 2, which.max) ; length(b[b==1]) ; length(b[b==2])
# }
#
# # star_c_out = dcem_star_train(cancer_data, iteration_count = 100, num_clusters = 2)
# # a = star_c_out$prob ; a = data.frame(a) ; b = apply(a, 2, which.max) ; length(b[b==1]) ; length(b[b==2])
#
# for (i in clusters){
#   print(paste("K = ", i))
#   tic()
#   em_c_out = dcem_train(cancer_data, iteration_count = 1000, num_clusters = 2, threshold = 0.0000001)
#   t<-toc()
#   t<-t$toc - t$tic
#   dcem_time <- rbind(dcem_time, t)
#   a = em_c_out$prob ; a = data.frame(a) ; b = apply(a, 2, which.max) ; length(b[b==1]) ; length(b[b==2])
# }
#
# print(star_time)
# print(dcem_time)
#
# plot(c(02,5,10), star_time, type="l", col="red" )
#
# plot(clusters, dcem_time, type="l", col="green" )
#
# # em_c_out = dcem_train(cancer_data, iteration_count = 1000, num_clusters = 2, threshold = 0.0000001)
# # a = em_c_out$prob ; a = data.frame(a) ; b = apply(a, 2, which.max) ; length(b[b==1]) ; length(b[b==2])
#
