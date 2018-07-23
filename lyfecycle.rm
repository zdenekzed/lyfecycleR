# reporting date
today <- as.Date('2012-04-11', format='%Y-%m-%d')
 
# processing data
orders <- dcast(orders, orderId + clientId + gender + orderdate ~ product, value.var='product', fun.aggregate=length)
 
orders <- orders %>%
 group_by(clientId) %>%
 mutate(frequency=n(),
 recency=as.numeric(today-orderdate)) %>%
 filter(orderdate==max(orderdate)) %>%
 filter(orderId==max(orderId)) %>%
 ungroup()
 
# exploratory analysis - TOHLŮE NEDELANE
ggplot(orders, aes(x=frequency)) +
 theme_bw() +
 scale_x_continuous(breaks=c(1:10)) +
 geom_bar(alpha=0.6, binwidth=1) +
 ggtitle("Dustribution by frequency")
 
ggplot(orders, aes(x=recency)) +
 theme_bw() +
 geom_bar(alpha=0.6, binwidth=1) +
 ggtitle("Dustribution by recency")

############################
# TED DALSI KROK - PRIADME SEGMENTY

orders.segm <- orders %>%
 mutate(segm.freq=ifelse(between(frequency, 1, 1), '1',
 ifelse(between(frequency, 2, 2), '2',
 ifelse(between(frequency, 3, 3), '3',
 ifelse(between(frequency, 4, 4), '4',
 ifelse(between(frequency, 5, 5), '5', '>5')))))) %>%
 mutate(segm.rec=ifelse(between(recency, 0, 6), '0-6 days',
 ifelse(between(recency, 7, 13), '7-13 days',
 ifelse(between(recency, 14, 19), '14-19 days',
 ifelse(between(recency, 20, 45), '20-45 days',
 ifelse(between(recency, 46, 80), '46-80 days', '>80 days')))))) %>%
 # creating last cart feature
 mutate(cart=paste(ifelse(a!=0, 'a', ''),
 ifelse(b!=0, 'b', ''),
 ifelse(c!=0, 'c', ''), sep='')) %>%
 arrange(clientId)
 
# defining order of boundaries
orders.segm$segm.freq <- factor(orders.segm$segm.freq, levels=c('>5', '5', '4', '3', '2', '1'))
orders.segm$segm.rec <- factor(orders.segm$segm.rec, levels=c('>80 days', '46-80 days', '20-45 days', '14-19 days', '7-13 days', '0-6 days'))

##############
# We have everything need to create LifeCycle Grids. We need to combine clients into segments with the 
##############
lcg <- orders.segm %>%
 group_by(segm.rec, segm.freq) %>%
 summarise(quantity=n()) %>%
 mutate(client='client') %>%
 ungroup()
