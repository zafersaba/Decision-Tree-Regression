data_set <- read.csv("hw05_data_set.csv")
# get x and y values
x <- data_set$eruptions
y <- data_set$waiting

N <- length(y)

x_train <- x[1:150]
x_test <- x[151:N]
y_train <- y[1:150]
y_test <- y[151:N]

# get numbers of train and test samples
N_train <- length(y_train)
N_test <- length(y_test)

# create necessary data structures
is_terminal <- c()
node_splits <- c()
node_fits <- list()
node_indices <- list(1:N_train)
is_terminal <- c(FALSE)
need_split <- c(TRUE)

#Set P

P <- 25

# learning algorithm
while (1) {
  # find nodes that need splitting
  split_nodes <- which(need_split)
  # check whether we reach all terminal nodes
  if (length(split_nodes) == 0) {
    break
  }
  # find best split positions for all nodes
  for (split_node in split_nodes) {
    data_indices <- node_indices[[split_node]]
    need_split[split_node] <- FALSE
    node_fits[[split_node]] <- mean(y_train[data_indices])
    # check whether the node has P or fewer data points
    if (length((x_train[data_indices])) <= P) {
      is_terminal[split_node] <- TRUE
    } else {
      is_terminal[split_node] <- FALSE
      
      #initialize best score and best split
      
      best_score <- 0
      best_split <- 0
      
      unique_values <- sort(unique(x_train[data_indices]))
      split_positions <- (unique_values[-1] + unique_values[-length(unique_values)]) / 2
      split_scores <- rep(0, length(split_positions))
      for (s in 1:length(split_positions)) {
        left_indices <- data_indices[which(x_train[data_indices] <= split_positions[s])]
        right_indices <- data_indices[which(x_train[data_indices] > split_positions[s])]
        split_scores[s] <- 1 / length(data_indices) * (sum((y_train[left_indices]-mean(y_train[left_indices]))^2) +
           sum((y_train[right_indices]-mean(y_train[right_indices]))^2))
      }
      
      #find the best score
      best_score <- min(split_scores)
      
      #find the corresponding split
      best_split <- split_positions[which.min(split_scores)]
      
      node_splits[split_node] <- best_split
      
      # create left node using the selected split
      left_indices <- data_indices[which(x_train[data_indices] < best_split)]
      node_indices[[2 * split_node]] <- left_indices
      is_terminal[2 * split_node] <- FALSE
      need_split[2 * split_node] <- TRUE
      
      # create right node using the selected split
      right_indices <- data_indices[which(x_train[data_indices] >= best_split)]
      node_indices[[2 * split_node + 1]] <- right_indices
      is_terminal[2 * split_node + 1] <- FALSE
      need_split[2 * split_node + 1] <- TRUE
    }
  }
}



#Function that gives fits as an output

getFit <- function(z){
  index <- 1
  while(1)
  {
    y_predicted<-node_fits[index]
    if(is_terminal[index]==TRUE){
      return(y_predicted)
    }
    else{
    if(z <= node_splits[index]) {
      index <- 2*index
      }
    else{
      index <- 2*index +1
    }
    
  }
  }
}

minimum_value <- min(x)-0.1
maximum_value <- max(x)+0.1
data_interval <- seq(from = minimum_value, to = maximum_value, by = 0.001)



#find the fits to plot

fits <- c()

for (t in data_interval)
{
  
  fits <- c(fits,getFit(t))
}

#plot data points and fit


plot(x_train,y_train,type = "p", pch = 19, col="blue" ,
     ylim = c(min(y), max(y)), xlim = c(minimum_value, maximum_value),
     ylab = "Waiting time to next eruption (min)", xlab = "Eruption time (min)", las = 1, main = paste("P = ", P))
points(x_test,y_test, type = "p" , pch=19 , col="red", )


lines(data_interval, fits)

#Calculate predictions for test data

y_predicted_test <- c(1:122)
for (i in 1:N_test) {
  y_predicted_test[i] <- getFit(x_test[i])
}

#convert y_predicted_test to an array from a list

y_predicted_test <- array(as.numeric(unlist(y_predicted_test)), dim=c(1,122))

#Calculating RMSE for test data for P=25

RMSE25 <- sqrt(sum((y_test-y_predicted_test)^2)/length(y_test))

sprintf("RMSE is  %s when P is %i", RMSE25, P)
