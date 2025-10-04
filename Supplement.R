# Fit KNN model
hrt_attack_knn <- knn3(output ~ ., data = heartattack, k = 5)

# Predict test set
test_data$output.pred <- predict(hrt_attack_knn, test_data, type="class")

# Compute misclassification rate
mean(test_data$output.pred != test_data$output)

## Investigate different choices of k
mcr_test <- rep(NA, 100)
mcr_train <- rep(NA, 100)
for (k in 1:100) {
  # Fit each knn model
  fit <- knn3(output ~ ., data = heartattack, k=k)
  output.pred <- predict(fit, test_data, type="class")
  # Test mcr
  mcr_test[k] <- mean(output.pred != test_data$output)
  # Train mcr
  output.pred <- predict(fit, train_data, type="class")
  mcr_train[k] <- mean(output.pred != train_data$output)
}
which.min(mcr_test)
which.min(mcr_train)

## Plot mcr
# Make data frame of results
test <- data.frame(k=1:100, mcr=mcr_test, set="Test")
train <- data.frame(k=1:100, mcr=mcr_train, set="Train")
full <- rbind(test, train)

ggplot() +
  geom_line(aes(x=k, y=mcr, color=set), data=full, size=1.5) +
  geom_vline(xintercept=63)

########################################################################################
# Fit a logistic regression
hrtattack_log <- glm(output ~ ., family=binomial(link=logit), data=heartattack)
summary(hrtattack_log)

## ANOVA table which shows grouping variable effects
anova(hrtattack_log, test="Chisq")

######################################################################################
# Fit LDA model
hrtattack_lda <- lda(output ~ ., data = train_data)

# Test predictions
test_data$output.pred <- predict(hrtattack_lda, test_data)$posterior
