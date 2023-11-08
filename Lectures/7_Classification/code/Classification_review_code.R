set.seed(123)
cvRes <- NULL
for(i in 1:100) {
  # Run a logit regression
  m2 <- glm(yield ~ income + sat,ad,family = binomial(link = 'logit'))
  
  # Cross validation (single example)
  inds <- sample(1:nrow(ad),size = round(nrow(ad)*.75),replace = F)
  
  # Create training and test sets
  train <- ad %>% slice(inds)
  test <- ad %>% slice(-inds)
  
  # Run the regression: Logit
  mLG <- glm(yield ~ income + sat,train,family = binomial(link = 'logit'))
  mLM <- lm(yield ~ income + sat,train)
  
  # Predict probabilities in test
  test <- test %>%
    mutate(prob_LG = predict(mLG,newdata = test,type = 'response'),
           prob_LM = predict(mLM,newdata = test))
  
  auc_LG <- roc_auc(data = test %>%
                      mutate(truth = factor(yield,levels = c('1','0'))),
                    truth,prob_LG)
  
  auc_LM <- roc_auc(data = test %>%
                      mutate(truth = factor(yield,levels = c('1','0'))),
                    truth,prob_LM)

  tmp <- data.frame(model = 'Logit',auc = auc_LG$.estimate) %>%
    bind_rows(data.frame(model = 'Linear Regession',auc = auc_LM$.estimate))

  cvRes <- cvRes %>% bind_rows(tmp %>%
                                 mutate(cvInd = i))  
}

cvRes %>%
  group_by(model) %>%
  summarise(auc = mean(auc))

cvRes %>%
  ggplot(aes(x = auc,color = model)) + 
  geom_density()

cvRes %>%
  as_tibble() %>%
  spread(model,auc) %>%
  mutate(diff = Logit - `Linear Regession`) %>%
  # summarise(conf = mean(diff > 0))
  ggplot(aes(x = diff)) + 
  geom_density() + 
  geom_vline(xintercept = 0)
