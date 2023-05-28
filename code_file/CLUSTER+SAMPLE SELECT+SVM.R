# Prepare work for clustering
path <- "./data3888/Optiver/individual_book_train"
names <- dir('./data3888/Optiver/individual_book_train')
# Get the list of file names in the directory
file_names <- list.files(path, pattern = ".csv", full.names = TRUE)
n <- 1
liquidity <- NULL
# Loop through each file, read it into a data frame, and add it to the list
for (file in file_names) {
  name <- names[n]
  n = n+1
  df <- read.csv(file)
  dff <- NULL
  # store stock names
  dff['name'] <- name
  # calculate liquidity for each stock 
  dff['liquidity'] <-round(sum(mean(df$bid_size1), mean(df$bid_size2),mean(df$ask_size1),mean(df$ask_size2)),0)
  liquidity <- rbind(liquidity,dff)
}
liquidity <- as.data.frame(liquidity)
# load the liquidity file
l <- read.csv('liquidity edited.csv')
# Use kmean to cluster
kmeans <- kmeans(l[2], centers = 5)







samples_per_cluster <- 5
cluster_labels <- kmeans$cluster
# Initialize an empty list to store selected samples
selected_samples <- NULL

# Loop through each cluster
for (i in 1:5) {
  # Extract samples belonging to the current cluster
  current_cluster_samples <- l[cluster_labels == i, , drop = FALSE]
  
  # Randomly select samples from the current cluster
  temp <- NULL
  temp <- current_cluster_samples[sample(nrow(current_cluster_samples), samples_per_cluster), , drop = T]
  temp['cluster'] <- i
  selected_samples <- rbind(selected_samples,temp)
}

# Combine the selected samples from all clusters into a single matrix
selected_samples <- do.call(rbind, selected_samples)
selected_samples <- as.data.frame(selected_samples)








square <- function(x) {
  return(x^2)
}
#make a function for calculate RMSE
Derek <- function(path,c) {
  # data prepare 
  s <- read.csv(path)
  s <- s %>% mutate(WAP = (bid_price1 * ask_size1 +
                             ask_price1 * bid_size1) /
                      (bid_size1 +   ask_size1))
  data <- s %>% mutate(BidAskSpread = 
                         ask_price1 / bid_price1 - 1)
  data <- data %>%  mutate(time_bucket =
                             ceiling(seconds_in_bucket / 30))
  lr <- c(data$WAP[1],diff(log(data$WAP)))
  lr = as.data.frame(lr)
  data <- cbind(data,lr)
  id <- unique(data$time_id)
  set.seed(c)
  selected_id = sample(id, size = 100, replace = F)
  results <- data.frame()
  # Train a model for each time id
  for (i in 1:length(selected_id)){
    k <- data.frame()
    d <- data %>% filter(time_id == selected_id[i])
    d = d[-1,]
    time <- unique(d$time_bucket)
    # Take variables we need for training 
    for (y in 1:length(time)){
      df <- NULL
      dt <- d %>% filter(time_bucket == time[y])
      df['time_id'] = selected_id[i]
      df['time_bucket'] = time[y]
      df['mean_WAP'] = mean(dt$WAP)
      df['mean_spread'] = mean(dt$BidAskSpread)
      df['voladility'] = sqrt(sum(square(dt$lr)))
      df = t(df)
      df = as.data.frame(df) 
      k <- rbind(df,k)
    }
    k <- k[order(k$time_bucket),]
    i_d = k$time_id[1]
    k = k[,-1]
    # Start to train
    train_control <- trainControl(method = "timeslice",
                                  initialWindow = 4,  
                                  horizon = 1,  
                                  fixedWindow = TRUE)
    svm_model <- train(voladility ~ ., data = k, method =
                         "svmRadial", trControl =train_control)
    # Take the RMSE
    result = data.frame(i_d, svm_model$results$RMSE[1])
    results = rbind(results,result)
    
  }
  return(results)
}






# Get RMSEs for each cluster and prepare data for creating Boxplots
# Read cluster file we got from kmeans
cluster <- read.csv('sample select.csv')
n <- length(unique(cluster$cluster))
df_list <- list()
# Go through each cluster
  for (i in 1:n) {
    select <- cluster %>% filter(cluster == i)
    print(i)
    final <- data.frame()
    #Go through each stock we randomly picked from each cluster
    for (p in 1:length(select$X)) {
      path = paste('./data3888/Optiver/individual_book_train/',select$X[p], sep = "")
      
      # Get RMSE in the cluster
      rmse <- Derek(path,i)
      rmse['Name'] = select$X[p]
      final = rbind(final,rmse) 
    }
    df_list[[i]] = final
  }
  # Write them as CSV
  for (i in seq_along(df_list)) {
    write.csv(df_list[[i]], file = paste0("df", i, ".csv"), row.names = FALSE)
  }









library(readxl)
# Read excel file that contains RMSEs for each model in a cluster
final <-  read_excel("C1.xlsx")
# Transfer some MSE to RMSE
final$LINEAR<- sqrt(final$LINEAR)
final$RF <- sqrt(final$RF)
# Process the data
final1 <- gather(final)
final1 <- na.omit(final1)
# We need to do it in cluster 6 since there is a super high RMSE in it.
d = which(final1$value == max(final1$value))
final1 = final1[-d,]
# Draw the boxplot
colors <- c("#0072B2", "#E69F00", "#009E73", "#F0E442", "#D55E00", "#CC79A7")
ggplot(final1, aes(x = key, y = value, fill = key)) +
  geom_boxplot(
    notch = TRUE,
    outlier.color = "black",
    outlier.shape = 16,
    width = 0.5
  ) +
  labs(x = "Models", y = "Rmse") +
  ggtitle("Boxplot for cluster6") +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 8),
    legend.position = "none",
    panel.background = element_rect(fill = "#F5F5F5")
  )
