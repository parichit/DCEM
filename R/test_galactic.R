# star_data <- read.csv2(file="/Users/schmuck/Documents/Box Sync/Courses/Spring2018/Independent Study/gmailgithub/DCEM/galaxySurveyDataSet.txt",
#                          sep=",", header=TRUE, stringsAsFactors = FALSE)
#
# dim(star_data)
#
# # Remove the label column,
# star_data = star_data[-1,]
# star_data = star_data[,-1]
# star_data <- na.omit(star_data)
#
# #star_data = apply(star_data, 2, as.numeric)
#
# # Drop rows containig empty/NA values
# star_data <- na.omit(star_data)
# dim(star_data)
#
# tic() ; em_c_out = dcem_train(star_data, iteration_count = 1000, num_clusters = 3, threshold = 0.0001) ; toc()
# tic() ; star_c_out = dcem_star_train(star_data, iteration_count = 100, num_clusters = 2) ; toc()
