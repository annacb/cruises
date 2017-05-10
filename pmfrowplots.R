oxyno2P2 <- ggplot(subset(tgt66interpDepTime, variable == 'NO2'), aes(datetime, DepthBin)) + 
  geom_tile(aes(fill = value)) + 
  scale_y_reverse(limits = c(1000, 0), breaks = seq(from = 0, to = 1000, by = 50), name = 'Depth (m)') + 
  scale_x_datetime(limits = c(t0_AB, t1_AB), 
                   breaks = tseq_AB,
                   labels = scales::date_format(format = '%m/%d %H:%M', tz = 'US/Pacific'),
                   name = 'Date (hour)') +
  geom_point(data = tgt66n, aes(x = datetime, y = depth), shape = ".") + 
  scale_fill_gradientn(colours = c('white', 'darkorchid1', 'skyblue1', 'springgreen', 'yellow', 'orange', 'red'), name = "[NO2] (umol/kg)") + 
  geom_contour(data = subset(tgt66interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value), colour = ..level..)) + 
  # geom_contour(data = subset(tgt66interpDepTime, variable == 'temp'), aes(x = datetime, y = DepthBin, z = value), breaks = seq(3, 27, by = 6), colour = 'red', linetype = 'dotted', size = 1) +
  # geom_contour(data = subset(tgt66interpDepTime, variable == 'O2'), aes(x = datetime, y = DepthBin, z = log10(value)), breaks = seq(0, 3, by = 1), colour = 'gray20', size = 1.5, linetype = 'longdash') +
  geom_hline(aes(yintercept = d), tgt66hlineO2, lty = 5) +
  labs(size = '[NO2] (umol/kg)', colour = 'log([O2]) (umol/kg)', 
       title = 'ETNP TGT66 Cruise Feb 23 to Feb 28 1972', subtitle = '(9˚N, -110˚W) to (22.66˚N, -110˚W)') +
  theme(axis.text.x = element_text(angle = 90, size = 16),
        axis.text.y = element_text(size = 16),
        text = element_text(size=20),
        plot.title = element_text(size = 25, hjust = 0),
        plot.margin = unit(c(0, 0, 0, 0) , "in"))