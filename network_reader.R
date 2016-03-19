# http://datadrivensecurity.info/blog/posts/2014/Jan/blander-part1/


setwd("~/Downloads/NETWORK")
csv <- read.csv("marx.csv",sep=",")
# let's look at the first few rows.
head(csv)

csv$type <- factor(csv$type)
summary(csv$type)

csv$day <- as.Date(csv$datetime, format = "%Y-%m-%d %H:%M:%S")
# add a freq column
csv$freq <- c(1)  # they all occur once right now
hosts <- aggregate(freq ~ day + host, data = csv, FUN = sum)
head(hosts)

library(ggplot2)
# set up a ggplot instance, pretty color for each host
gg <- ggplot(hosts, aes(x = day, y = freq, fill = host))
# add in a simple bar plot
gg <- gg + geom_bar(stat = "identity", width = 1)
# create individual plots for each host with free scales
gg <- gg + facet_wrap(~host, scales = "free")
# simple theme, with no legend
gg <- gg + theme_bw() + theme(legend.position = "none")
print(gg)

# remove duplicate source IP per host, per day
u.hosts <- aggregate(freq ~ day + host + src, data = csv, FUN = min)
# now we can aggregate nicely
hosts <- aggregate(freq ~ day + host, data = u.hosts, FUN = sum)

# and create that same plot
gg <- ggplot(hosts, aes(x = day, y = freq, fill = host))
gg <- gg + geom_bar(stat = "identity", width = 1)
gg <- gg + facet_wrap(~host, scales = "free")
gg <- gg + theme_bw() + theme(legend.position = "none")
plot(gg)


library(reshape)
# cast this into a data.frame so we can operate on individual hosts
hmatrix <- cast(hosts, day ~ host, value = "freq")
# now loop on each host and apply a 7-day moving average
host.ma <- apply(hmatrix[, -1], 2, filter, filter = rep(1/7, 7))
# bring the days back in.
host.ma <- cbind(hmatrix$day, as.data.frame(host.ma))
# fix the column names
colnames(host.ma) <- colnames(hmatrix)
# get it back into a data frame for ggplot
hosts.ma <- melt(host.ma, id = c("day"), na.rm = T)
# and fix the names on it.
colnames(hosts.ma) <- c("day", "host", "freq")
gg <- ggplot(hosts.ma, aes(x = day, y = freq, fill = host))
gg <- gg + geom_bar(stat = "identity", width = 1)
gg <- gg + facet_wrap(~host)
gg <- gg + theme_bw() + theme(legend.position = "none")
plot(gg)


tcp.ports <- csv$dpt[csv$proto == "TCP"]
tcp.ports <- factor(tcp.ports)
summary(tcp.ports, maxsum = 10)

#

csv$day <- as.Date(csv$datetime, format = "%Y-%m-%d %H:%M:%S")
# add a freq column
csv$freq <- c(1)  # they all occur once right now
# remove duplicate source IP and port per host, per day
tcp.uniq <- aggregate(freq ~ day + host + src + dpt, data = csv[csv$proto == 
                                                                  "TCP", ], FUN = min)
# rough idea of top 10
top.ports <- names(summary(factor(tcp.uniq$dpt), maxsum = 11))[1:10]

# now have unique source address, convert to count per day per host
host.tcp <- aggregate(freq ~ dpt + day + host, data = tcp.uniq, FUN = sum)
avg.tcp <- aggregate(freq ~ day + dpt, data = host.tcp, FUN = mean)
plot.tcp <- avg.tcp[avg.tcp$dpt %in% top.ports, ]
plot.tcp$dpt <- factor(plot.tcp$dpt, levels = top.ports, ordered = T)
library(ggplot2)
gg <- ggplot(plot.tcp, aes(x = dpt, y = freq))
gg <- gg + geom_boxplot(fill = "lightsteelblue")
gg <- gg + xlab("TCP Destination Port")
gg <- gg + ylab("Average Unique Source Addresses per day")
gg <- gg + theme_bw()
plot(gg)