require(tidyverse)
toplot <- data.frame(y = c(0,0,0,0,10,25,100,100,100,100,70,65,55,40,10,0,0,0),
                     x = c(2,3,4,5,10,11,17,18,19,22,25,26,27,28,29,31,32,34))
p <- toplot %>%
  ggplot(aes(x = x,y = y)) +
  geom_point(size = 4) +
  labs(x = 'Dosage (mg)',
       y = 'Effectiveness (%)')

plist <- list()
mins <- NULL
counter <- 1
splits <- seq(3,33)
for(split in splits) {
  tmpPlot <- toplot %>%
    mutate(split = ifelse(x < split,'below','above')) %>%
    group_by(split) %>%
    mutate(ymean = mean(y)) %>%
    ungroup() %>%
    mutate(se = (y - ymean)^2)

  mins <- c(mins,tmpPlot %>%
              summarise(sse = round(sum(se))) %>% .$sse)

  lab <- paste0('Split = ',split,
                '\nRSS = ',tmpPlot %>%
                  summarise(sse = round(sum(se))) %>% .$sse)

  hjuster <- ifelse(split > 30,1,0)

  plist[[counter]] <- p +
    geom_vline(xintercept = split,linetype = 'dashed') +
    annotate(geom = 'segment',x = 0,xend = split,
             y = unique(tmpPlot %>%
                          filter(split == 'below') %>% .$ymean),
             yend = unique(tmpPlot %>%
                             filter(split == 'below') %>% .$ymean),
             color = 'red',size = 2,alpha = .6) +
    annotate(geom = 'segment',x = split,xend = Inf,
             y = unique(tmpPlot %>%
                          filter(split == 'above') %>% .$ymean),
             yend = unique(tmpPlot %>%
                             filter(split == 'above') %>% .$ymean),
             color = 'red',size = 2,alpha = .6) +
    geom_segment(data = tmpPlot,aes(x = x,xend = x,y = ymean,yend = y),color = 'red') +
    annotate(geom = 'label',x = split,y = Inf,label = lab,vjust = 1,hjust = hjuster)
  counter <- counter + 1
}


mInd <- which.min(mins)

plist[[mInd]]
split <- splits[mInd]

tmpPlot <- toplot %>%
  mutate(split = ifelse(x < split,'below','above')) %>%
  group_by(split) %>%
  mutate(ymean = mean(y)) %>%
  ungroup() %>%
  mutate(se = (y - ymean)^2)


p2 <- p +
  geom_vline(xintercept = split,size = 3,color = 'blue',alpha = .6) +
  annotate(geom = 'segment',x = 0,xend = split,
           y = unique(tmpPlot %>%
                        filter(split == 'below') %>% .$ymean),
           yend = unique(tmpPlot %>%
                           filter(split == 'below') %>% .$ymean),
           color = 'red',size = 2,alpha = .6)



plist2 <- list()
mins2 <- NULL
counter <- 1
for(split2 in splits2) {
  tmpPlot <- toplot %>%
    filter(x > splits[mInd]) %>%
    mutate(split = ifelse(x < split2,'below','above')) %>%
    group_by(split) %>%
    mutate(ymean = mean(y)) %>%
    ungroup() %>%
    mutate(se = (y - ymean)^2)

  mins2 <- c(mins2,tmpPlot %>%
               summarise(sse = round(sum(se))) %>% .$sse)

  lab <- paste0('Split = ',split2,
                '\nRSS = ',tmpPlot %>%
                  summarise(sse = round(sum(se))) %>% .$sse)

  hjuster <- ifelse(split2 > 30,1,0)

  plist2[[counter]] <- p2 +
    geom_vline(xintercept = split2,linetype = 'dashed') +
    annotate(geom = 'segment',x = split2,xend = Inf,
             y = unique(tmpPlot %>%
                          filter(split == 'above') %>% .$ymean),
             yend = unique(tmpPlot %>%
                             filter(split == 'above') %>% .$ymean),
             color = 'red',size = 2,alpha = .6) +
    annotate(geom = 'segment',x = splits[mInd],xend = split2,
             y = unique(tmpPlot %>%
                          filter(split == 'below') %>% .$ymean),
             yend = unique(tmpPlot %>%
                             filter(split == 'below') %>% .$ymean),
             color = 'red',size = 2,alpha = .6) +
    geom_segment(data = tmpPlot,aes(x = x,xend = x,y = ymean,yend = y),color = 'red') +
    annotate(geom = 'label',x = split2,y = Inf,label = lab,vjust = 1,hjust = hjuster)
  counter <- counter + 1
}


mInd2 <- which.min(mins2)

plist2[[mInd2]]

split2 <- splits2[mInd2]

tmpPlot <- toplot %>%
  filter(x > splits[mInd]) %>%
  mutate(split = ifelse(x < split2,'below','above')) %>%
  group_by(split) %>%
  mutate(ymean = mean(y)) %>%
  ungroup() %>%
  mutate(se = (y - ymean)^2)

pFinal <- p2 +
  geom_vline(xintercept = c(split2),size = 3,color = 'blue',alpha = .6) +
  annotate(geom = 'segment',x = split2,xend = Inf,
           y = unique(tmpPlot %>%
                        filter(split == 'above') %>% .$ymean),
           yend = unique(tmpPlot %>%
                           filter(split == 'above') %>% .$ymean),
           color = 'red',size = 2,alpha = .6) +
  annotate(geom = 'segment',x = splits[mInd],xend = split2,
           y = unique(tmpPlot %>%
                        filter(split == 'below') %>% .$ymean),
           yend = unique(tmpPlot %>%
                           filter(split == 'below') %>% .$ymean),
           color = 'red',size = 2,alpha = .6)


# Saving
for(i in 1:length(plist)) {
  if(mins[i] == min(mins)) {
    ggsave(plot = plist[[i]] +
             theme(panel.background = element_rect(fill = 'white')) +
             annotate(geom = 'label',x = 17,y = 50,label = 'MINIMUM',size = 10,color = 'red'),
           filename = paste0('./rf0_',splits[i],'.png'),
           device = 'png')
  } else {
    ggsave(plot = plist[[i]],filename = paste0('./rf0_',splits[i],'.png'),
           device = 'png')
  }
}

for(i in 1:length(plist2)) {
  if(mins2[i] == min(mins2)) {
    ggsave(plot = plist2[[i]] +
             theme(panel.background = element_rect(fill = 'white')) +
             annotate(geom = 'label',x = 17,y = 50,label = 'MINIMUM',size = 10,color = 'red'),
           filename = paste0('./rf2_',splits2[i],'.png'),
           device = 'png')
  } else {
    ggsave(plot = plist2[[i]],filename = paste0('./rf2_',splits2[i],'.png'),
           device = 'png')
  }
}

ggsave(plot = pFinal,filename = 'rfFinal.png',device = 'png')

# And save as gif
## list file names and read in
require(magick)
imgs <- list.files('./', pattern = 'rf\\d_',full.names = TRUE)

rf0Plots <- paste0('./rf0_',splits,'.png')

rf0Plots <- c(rf0Plots,
                rev(rf0Plots),
              rf0Plots[1:9],
              rep(rf0Plots[10],10))


rf2Plots <- paste0('./rf2_',splits2,'.png')


which.min(mins2)
rf2Plots <- c(rf2Plots,
              rev(rf2Plots),
              rf2Plots[1:16],
              rep(rf2Plots[17],10),
              rep('rfFinal.png',30))


imgs <- c(rf0Plots,rf2Plots)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 5)

## save to disk
image_write(image = img_animated,
            path = "../figs/rfdemo.gif")
