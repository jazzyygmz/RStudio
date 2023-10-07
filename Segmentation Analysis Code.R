##############################
# Segmentation Analysis in R #
##############################

## Install Packages (if needed)

## Load Packages and Set Seed
set.seed(1)

# Import Data
seg <- read.csv(file.choose()) ## Choose Retail Segmentation Data.csv file
View(seg)
head(seg)

# Run hierarchical clustering with bases variables
seg_hclust <- hclust(dist(scale(cbind(seg$avg_order_size, seg$avg_order_freq, seg$crossbuy, seg$multichannel, 
seg$per_sale, seg$tenure, seg$avg_mktg_cnt, seg$return_rate))), method="complete")
seg_hclust

# Elbow plot for first 10 segments
x <- c(1:10)
sort_height <- sort(seg_hclust$height,decreasing=TRUE)
y <- sort_height[1:10]
plot(x,y); lines(x,y,col="blue")

# Run k-means with 6 segments
seg_kmeans <- kmeans(x = data.frame(seg$avg_order_size, seg$avg_order_freq, seg$crossbuy, seg$multichannel, 
seg$per_sale, seg$tenure, seg$avg_mktg_cnt, seg$return_rate), 6)

# Add segment number back to original data
segment = seg_kmeans$cluster
segmentation_result <- cbind(seg, segment)
head(segmentation_result)

# Export data to a CSV file
write.csv(segmentation_result, file = file.choose(new=TRUE), row.names = FALSE) ## Name file segmentation_result.csv
