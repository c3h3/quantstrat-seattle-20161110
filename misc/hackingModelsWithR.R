##########################################

load("lm_line_data.RData")
sample1_x <- lm_line_data$x
sample1_y <- lm_line_data$y
par(mfrow=c(1,1))
plot(lm_line_data)


##########################################

sample_models = cbind(3+runif(7, -1, 1),1+runif(7, -1, 1))
par(mfrow=c(1,2))
plot(lm_line_data)
for (i in 1:NROW(sample_models)){
  abline(sample_models[i,1],sample_models[i,2],col=i)
}

plot(sample_models,xlim=c(2,4),ylim=c(0,2),col=1:NROW(sample_models),xlab="a0",ylab="a1")

##########################################

min.RSS <- function(data, par) {
  with(data, sum((par[1] + par[2] * x - y)^2))
}


##########################################

par(mfrow=c(1,1))
a0_range = (-10:10)*0.1 + 3
a1_range = (-10:10)*0.1 + 1

a_outer = matrix(0,nrow=length(a0_range),ncol=length(a1_range))

for (i in 1:length(a0_range)){
  for (j in 1:length(a1_range)){
    a_outer[i,j] = min.RSS(lm_line_data,c(a0_range[i],a1_range[j]))
  }
}

contour(a0_range,a1_range,a_outer,xlab="a0",ylab="a1")

##########################################

par(mfrow=c(1,2))
plot(lm_line_data)
for (i in 1:NROW(sample_models)){
  abline(sample_models[i,1],sample_models[i,2],col=i)
}

contour(a0_range,a1_range,a_outer,xlab="a0",ylab="a1")
points(sample_models,col=1:NROW(sample_models))

##########################################

result <- optim(par = c(3, 1), min.RSS, data = lm_line_data)

##########################################

par(mfrow=c(1,2))
plot(lm_line_data)
for (i in 1:NROW(sample_models)){
  abline(sample_models[i,1],sample_models[i,2],col=i)
}
abline(result$par[1],result$par[2],col=NROW(sample_models)+1,lwd=10)

contour(a0_range,a1_range,a_outer,xlab="a0",ylab="a1")
points(sample_models,col=1:NROW(sample_models))
points(result$par[1],result$par[2],col=NROW(sample_models)+1,pch=19,cex = 3)

##########################################

cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

# learning rate and iteration limit
alpha <- 0.01
num_iters <- 1000

# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# initialize coefficients
theta <- matrix(c(3.5,1.5), nrow=2)

# add a column of 1's for the intercept coefficient
X <- cbind(1, matrix(sample1_x))

# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% theta - sample1_y)
  delta <- t(X) %*% error / length(sample1_y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, sample1_y, theta)
  theta_history[[i]] <- theta
}

##########################################

par(mfrow=c(1,2))

plot(sample1_x,sample1_y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent',xlab="x",ylab="y")
for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
  abline(coef=theta_history[[i]], col=rgb(0.8,0,0,0.3))
}
abline(coef=theta, col='blue')


contour(a0_range,a1_range,a_outer,xlab="a0",ylab="a1")
for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
  points(theta_history[[i]][1],theta_history[[i]][2], col=rgb(0.8,0,0,0.3))
  if (i>1){
    last_pt = c(theta_history[[i-1]][1],theta_history[[i-1]][2])
    now_pt = c(theta_history[[i]][1],theta_history[[i]][2])
    arrows(last_pt[1],last_pt[2],now_pt[1],now_pt[2],length=0.05)
  }
}

##########################################

min.RSS.W <- function(data ,w ,par) {
  with(data, sum(w*(par[1] + par[2] * x - y)^2))
}

##########################################


Old_Model_Error_Vec <- (cbind(1,lm_line_data$x) %*% result$par - lm_line_data$y)^2
First_Change_Data_Index <- which(Old_Model_Error_Vec == max(Old_Model_Error_Vec))
Second_Change_Data_Index <- which(Old_Model_Error_Vec[-First_Change_Data_Index] == max(Old_Model_Error_Vec[-First_Change_Data_Index]))
if (Second_Change_Data_Index > First_Change_Data_Index){
  Second_Change_Data_Index <- Second_Change_Data_Index + 1
}

Change_Data_Index = c(First_Change_Data_Index,Second_Change_Data_Index)


##########################################

NEW_WEIGHT = 1000
Change_Data_Index = c(First_Change_Data_Index,Second_Change_Data_Index)
# Change_Data_Index = c( which(lm_line_data$y == max(lm_line_data$y)),which(lm_line_data$y == min(lm_line_data$y)))
data_w <- rep(1,NROW(lm_line_data))
data_w[Change_Data_Index] = NEW_WEIGHT
data_w <- data_w / sum(data_w)
result_w <- optim(par = c(3, 1), min.RSS.W, data = lm_line_data, w = data_w)

par(mfrow=c(1,2))
plot(lm_line_data)
abline(result$par,lwd=10,col=8)
points(lm_line_data[First_Change_Data_Index,],pch=19,cex=2)
points(lm_line_data[Second_Change_Data_Index,],pch=19,cex=2)  
abline(result_w$par,lwd=2,col=4)


a0_range = (-50:50)*0.1 + 3
a1_range = (-50:50)*0.1 + 1

old_a_outer = matrix(0,nrow=length(a0_range),ncol=length(a1_range))

for (i in 1:length(a0_range)){
  for (j in 1:length(a1_range)){
    old_a_outer[i,j] = min.RSS(lm_line_data,c(a0_range[i],a1_range[j]))
  }
}

contour(a0_range,a1_range,old_a_outer,xlab="a0",ylab="a1")

new_a_outer = matrix(0,nrow=length(a0_range),ncol=length(a1_range))
for (i in 1:length(a0_range)){
  for (j in 1:length(a1_range)){
    new_a_outer[i,j] = min.RSS.W(lm_line_data, w = data_w, c(a0_range[i],a1_range[j]))
  }
}

contour(a0_range,a1_range,new_a_outer,col=4,xlab="a0",ylab="a1",add=TRUE)

##########################################


par(mfrow=c(1,2))
plot(lm_line_data)
abline(result$par,lwd=10,col=8)

data_w <- rep(1,NROW(lm_line_data))

for (i in 1:3){
  NEW_WEIGHT = 10^i
  data_w <- rep(1,NROW(lm_line_data))
  data_w[Change_Data_Index] = NEW_WEIGHT
  data_w <- data_w / sum(data_w)
  result_w <- optim(par = c(3, 1), min.RSS.W, data = lm_line_data, w = data_w)
  points(lm_line_data[First_Change_Data_Index,],pch=19,cex=2)
  points(lm_line_data[Second_Change_Data_Index,],pch=19,cex=2)  
  abline(result_w$par,lwd=2,col=i+1)
}




a0_range = (-50:50)*0.1 + 3
a1_range = (-50:50)*0.1 + 1

old_a_outer = matrix(0,nrow=length(a0_range),ncol=length(a1_range))

for (i in 1:length(a0_range)){
  for (j in 1:length(a1_range)){
    old_a_outer[i,j] = min.RSS(lm_line_data,c(a0_range[i],a1_range[j]))
  }
}

contour(a0_range,a1_range,old_a_outer,xlab="a0",ylab="a1")


for (k in 1:3){
  NEW_WEIGHT = 10^k
  data_w <- rep(1,NROW(lm_line_data))
  data_w[Change_Data_Index] = NEW_WEIGHT
  data_w <- data_w / sum(data_w)
  new_a_outer = matrix(0,nrow=length(a0_range),ncol=length(a1_range))
  for (i in 1:length(a0_range)){
    for (j in 1:length(a1_range)){
      new_a_outer[i,j] = min.RSS.W(lm_line_data, w = data_w, c(a0_range[i],a1_range[j]))
    }
  }
  contour(a0_range,a1_range,new_a_outer,col=k+1,xlab="a0",ylab="a1",add=TRUE)
}



##########################################
### Inversed Problem
##########################################


min.W <- function(ww) {
  Xw = rbind(X[1,]*(ww^2),X[2,]*(ww^2))
proj = (lm_line_data$y - t(model_w_coef) %*%X ) %*% t(Xw)  
sum(proj^2) + 0.001*sum((ww -1)^2)
}

sol_ww = optim(rep(1,NROW(lm_line_data)), min.W,method = "BFGS")
sol_w = sol_ww$par^2

new_result_w <- optim(par = c(3, 1), min.RSS.W, data = lm_line_data, w = sol_w)

print(new_result_w$par)
print(t(model_w_coef))



##########################################
### Inversed Problem
##########################################


par(mfrow=c(1,2))
plot(lm_line_data)
abline(result_w$par,lwd=2,col=4)
abline(new_result_w$par,lwd=2,col=3)

a0_range = (-50:50)*0.1 + 3
a1_range = (-50:50)*0.1 + 1

old_a_outer = matrix(0,nrow=length(a0_range),ncol=length(a1_range))

for (i in 1:length(a0_range)){
  for (j in 1:length(a1_range)){
    old_a_outer[i,j] = min.RSS.W(lm_line_data, w = data_w, c(a0_range[i],a1_range[j]))
  }
}

contour(a0_range,a1_range,old_a_outer,xlab="a0",ylab="a1",col=4)

new_a_outer = matrix(0,nrow=length(a0_range),ncol=length(a1_range))
for (i in 1:length(a0_range)){
  for (j in 1:length(a1_range)){
    new_a_outer[i,j] = min.RSS.W(lm_line_data, w = sol_w, c(a0_range[i],a1_range[j]))
  }
}

contour(a0_range,a1_range,new_a_outer,col=3,xlab="a0",ylab="a1",add=TRUE)



