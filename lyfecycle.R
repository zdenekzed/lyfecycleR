# reporting date
today <- as.Date('2012-04-11', format='%Y-%m-%d')
 
# processing data - napojime na data z vytvoreneho datasetu
orders <- dcast(orders, orderId + clientId + gender + orderdate ~ product, value.var='product', fun.aggregate=length)
 
# Therefore, we need to calculate two values:
# 1/ number of orders that were placed by each client (or in some cases, it can be the number of items) - F,
# 2/ time lapse from the last purchase to the reporting date - R.

orders <- orders %>%
 group_by(clientId) %>%
 mutate(frequency=n(),
 recency=as.numeric(today-orderdate)) %>%
 filter(orderdate==max(orderdate)) %>%
 filter(orderId==max(orderId)) %>%
 ungroup()
 
############################
# We will use the following boundaries:
# for frequency: 1, 2, 3, 4, 5, >5,
# for recency: 0-6, 7-13, 14-19,  20-45, 46-80, >80

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
# We have everything need to create LifeCycle Grids. 
# We need to combine clients into segments 
##############
lcg <- orders.segm %>%
 group_by(segm.rec, segm.freq) %>%
 summarise(quantity=n()) %>%
 mutate(client='client') %>%
 ungroup()
