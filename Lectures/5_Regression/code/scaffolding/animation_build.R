x <- rnorm(100)

y <- 1 + 2*x + rnorm(100)

plot(x,y)

require(tidyverse)
toplot <- data.frame(x = x,y= y)

lm(y ~ x,toplot)

p <- toplot %>%
  ggplot(aes(x = x,y = y)) + 
  geom_point() + 
  xlim(c(-4,4)) + 
  ylim(c(-12,12))

pListA <- list()
counter <- 1
rmsesA <- alphas <- NULL
for(a in round(seq(-4,4,by = .1),1)) {
  preds <- a
  rmse <- sqrt(mean((y - preds)^2))
  
  pListA[[counter]] <- p + 
    geom_hline(yintercept = a) +
    geom_segment(x = toplot$x,y = toplot$y,xend = toplot$x,yend = a,color = 'red') + 
    annotate(geom = 'label',x = 0,y = Inf,label = paste0('alpha: ',a,'\nRMSE: ',round(rmse,3)),vjust = 1)
  rmsesA <- c(rmsesA,rmse)
  alphas <- c(alphas,a)
  counter <- counter + 1
}

pListA[[which(rmsesA == min(rmsesA))]]
alpha <- alphas[which(rmsesA == min(rmsesA))]

pListB <- list()
counter <- 1
rmsesB <- betas <- NULL
for(b in round(seq(-4,4,by = .1),1)) {
  preds <- alpha + b*x
  rmse <- sqrt(mean((y - preds)^2))
  pListB[[counter]] <- p + 
    geom_abline(intercept = alpha,slope = b) + 
    geom_segment(x = toplot$x,y = toplot$y,xend = toplot$x,yend = preds,color = 'red') + 
    annotate(geom = 'label',x = 0,y = Inf,label = paste0('beta: ',b,'\nRMSE: ',round(rmse,3)),vjust = 1)
  rmsesB <- c(rmsesB,rmse)
  betas <- c(betas,b)
  counter <- counter + 1
}

ggsave(plot = p,
       filename = paste0('./scaffolding/raw.png'),
       device = 'png') 

for(i in 1:length(pListA)) {
  if(rmsesA[i] == min(rmsesA)) {
    ggsave(plot = pListA[[i]] + 
             theme(panel.background = element_rect(fill = 'white')) + 
             annotate(geom = 'label',x = 0,y = 1,label = 'MINIMUM',size = 10,color = 'red'),
           filename = paste0('./scaffolding/alpha_best.png'),
           device = 'png')
  } else {
    ggsave(plot = pListA[[i]],filename = paste0('./scaffolding/alpha_',round(alphas[i],1),'.png'),
           device = 'png') 
  }
}


for(i in 1:length(pListB)) {
  if(rmsesB[i] == min(rmsesB)) {
    ggsave(plot = pListB[[i]] + 
             theme(panel.background = element_rect(fill = 'white')) + 
             annotate(geom = 'label',x = 0,y = 1,label = 'MINIMUM',size = 10,color = 'red'),
           filename = paste0('./scaffolding/beta_best.png'),
           device = 'png')
  } else {
    ggsave(plot = pListB[[i]],filename = paste0('./scaffolding/beta_',round(betas[i],1),'.png'),
           device = 'png') 
  }
}

## list file names and read in
require(magick)
imgs <- list.files('./scaffolding/', pattern = 'alpha|beta',full.names = TRUE)

alphaPlots <- paste0('./scaffolding/alpha_',round(seq(-4,4,by = .1),1),'.png')

bestA <- setdiff(alphaPlots,imgs)
alphaPlots[which(alphaPlots == bestA)] <- paste0('./scaffolding/alpha_best.png')
alphaPlots <- c(alphaPlots,
                paste0('./scaffolding/alpha_',round(seq(4,1.2,by = -.1),1),'.png'),
                rep(paste0('./scaffolding/alpha_best.png'),20))

betaPlots <- paste0('./scaffolding/beta_',round(c(seq(0,-4,by = -.1),
                                                  seq(-4,4,by = .1)),1),'.png')
bestB <- setdiff(betaPlots,imgs)
betaPlots[which(betaPlots == bestB)] <- paste0('./scaffolding/beta_best.png')

betaPlots <- c(betaPlots,
               paste0('./scaffolding/beta_',round(seq(4,2,by = -.1),1),'.png'),
               rep(paste0('./scaffolding/beta_best.png'),20))

rawPlots <- rep('./scaffolding/raw.png',40)

imgs <- c(rawPlots,alphaPlots,betaPlots)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 20)

## save to disk
image_write(image = img_animated,
            path = "./figs/regression-line.gif")