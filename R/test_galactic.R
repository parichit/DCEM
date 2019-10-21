# star_data <- read.csv2(file="/Users/schmuck/Documents/Box Sync/Courses/Spring2018/Independent Study/gmailgithub/DCEM/galaxySurveyDataSet.txt",
#                          sep=",", header=FALSE, stringsAsFactors = FALSE)
#
# dim(star_data)
# # Remove the label column,
# star_data = star_data[,-c(1)]
#
# star_data = apply(star_data, 2, as.numeric)
# star_data <- na.omit(star_data)
#
# # Drop rows containig empty/NA values
# dim(star_data)
#
# star_data <- scale(star_data)
#
# #star_data = apply(star_data, 2, as.numeric)
# #
# tic() ; em_c_out = dcem_train(star_data, iteration_count = 1000, num_clusters = 3, threshold = 0.001) ; toc()
# #
# tic() ; star_c_out = dcem_star_train(star_data, iteration_count = 100, num_clusters = 3) ; toc()
# #
