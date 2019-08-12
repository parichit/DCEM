# library('tictoc')
# library('DCEM')
# # Load the dataset.
# cancer_data <- read.csv2(file="/Users/schmuck/Documents/Box Sync/Courses/Spring2018/Independent Study/gmailgithub/DCEM/breastcancer.txt",
#                          sep=",", header=FALSE)
#
# # Store the labels.
# labels = cancer_data[,11]
#
# # Remove the ID and label column,
# cancer_data = cancer_data[,-c(1,11)]
#
# dim(cancer_data)
#
# # Replacing the ? with "NA"
# cancer_data[cancer_data=="?"] <- NA
#
# # Drop rows containig empty/NA values
# cancer_data <- na.omit(cancer_data)
# #
# # dim(cancer_data)
#
#
# tic()
#
# em_star_out = dcem_star_train(cancer_data, iteration_count = 1000, num_clusters = 2)
#
# execem_star <- toc()
#
# print(paste("EM time is: ",execem$toc - execem$tic))
# print(paste("EM* time is: ",execem_star$toc - execem_star$tic))
#
