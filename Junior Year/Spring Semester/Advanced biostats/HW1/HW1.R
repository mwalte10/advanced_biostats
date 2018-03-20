setwd("~/Desktop/")
data <- read.table("HW1.txt", header = TRUE)
install.packages("pROC")
install.packages("arm")
library(pROC)
library(arm)

data_roc <- roc(data$outcome, data$ferritin, percent = TRUE, ci = TRUE,
                boot.n= 100, ci.alpha = 0.9, stratified = FALSE,
                plot = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, grid = TRUE,
                print.auc = TRUE, show.thres = TRUE)


cancer <- read.csv("cancer.csv")
cancer <- as.data.frame(cancer)
glm.nom <- glm(cancer ~ as.factor(level), data = cancer, family = binomial(), weights = count)
glm.mid <- glm(cancer ~ as.numeric(mid), data = cancer, family = binomial(), weights = count)


odds_ratio.nom <- function(level){
  level <- level + 1
  beta <- coef(glm.nom)[level]
  beta <- as.numeric(beta)
  OR <- exp(beta)
  print(OR)
}

odds_ratio.mid <- function(mid){
  beta <- coef(glm.mid)[2]
  beta <- as.numeric(beta)
  se <- se.coef(glm.mid)[2]
  theta_hat <- mid * beta
  OR <- exp(theta_hat)
  print(OR)
}

ci.nom <- function(level){
  level <- level + 1
  se <- se.coef(glm.nom)[level]
  beta <- coef(glm.nom)[level]
  upper <- exp((beta) + 1.96 * (se))
  lower <- exp((beta) - 1.96 * (se))
  print(upper)
  print(lower)
}

ci.mid <- function(mid){
  beta <- coef(glm.mid)[2]
  se <- se.coef(glm.mid)[2]
  theta_hat <- mid * beta
  upper <- exp(theta_hat +  (1.96 * se * mid))
  lower <- exp(theta_hat - (1.96 * se * mid))
  print(upper)
  print(lower)
}

###########
#MLE
###########
n <- 10000
beta <-  rnorm(n, mean = 0.04, sd = 0.01)
cancer[,3] <- rep(1,12)
cancer[7:12,3] <- rep(0,6)
mle <- function(beta){
  data = cancer
  prob <- ((exp(as.numeric(mid) * beta) / (1 + exp(as.numeric(mid) * beta))) ^ (cancer)) * (1 / (1 + exp(as.numeric(mid) * beta))) ^ (1- cancer)
  which.max <- which.max(prob)
  mle.beta <- beta[which.max]
  return(mle.beta)
}







