require(ggplot2)
require(data.table)
require(grid) # unit function

args <- commandArgs(trailingOnly = TRUE)

prefix <- '04_C3'

CONCURRENCY <- 4
SHADOW_READ_RATIO <- 0.1
REPLICATION_FACTOR <- 3
RATE_INTERVAL <- 20

latency <- read.table(paste("../logs/", prefix, "_Latency", sep=""))
colnames(latency)[1] <- "ServerId"
colnames(latency)[2] <- "Timestamp"
colnames(latency)[3] <- "Latency"
colnames(latency)[4] <- "ClientId"

trim <- 10

# latency <- latency[10:NROW(latency),]
latency <- latency[latency$Timestamp > trim,]
print(summary(latency[latency$Timestamp > trim,]))
latency.dt <- data.table(latency)
lat50.by.client <- latency.dt[,quantile(Latency,c(0.50)),by=list(ClientId)]
lat95.by.client <- latency.dt[,quantile(Latency,c(0.95)),by=list(ClientId)]
lat99.by.client <- latency.dt[,quantile(Latency,c(0.99)),by=list(ClientId)]
range50 <- max(lat50.by.client$V1) - min(lat50.by.client$V1)
range95 <- max(lat95.by.client$V1) - min(lat95.by.client$V1)
range99 <- max(lat99.by.client$V1) - min(lat99.by.client$V1)

options(width=10000)
print(c(prefix,
		quantile(latency$Latency,c(0.5, 0.95, 0.99, 0.999)),
		range50, range95, range99))


#trim <- 100 # TODO: delete
wait.mon <- read.table(paste("../logs/", prefix, "_WaitMon", sep=""))
colnames(wait.mon)[1] <- "ServerId"
colnames(wait.mon)[2] <- "Timestamp"
colnames(wait.mon)[3] <- "WaitingRequests"
#wait.mon <- wait.mon[wait.mon$Timestamp > trim,] # TODO: delete


sent.requests <- read.table(paste("../logs/", prefix, "_SentRequests", sep=""))
colnames(sent.requests)[1] <- "ClientId"
colnames(sent.requests)[2] <- "Timestamp"
colnames(sent.requests)[3] <- "ServerId"
colnames(sent.requests)[4] <- "SentRequests"

server.rate <- read.table(paste("../logs/", prefix, "_serverRate", sep=""))
colnames(server.rate)[1] <- "Dummy"
colnames(server.rate)[2] <- "Timestamp"
colnames(server.rate)[3] <- "ServerId"
colnames(server.rate)[4] <- "ServerRate"


#trim <- 14000
#latency <- latency[latency$Timestamp < trim,]
png(paste(prefix, "_latency.png", sep=""), height=1024, width=1024)
ggplot(latency) +
	  geom_point(aes(y=Latency, x=Timestamp, colour=ClientId), size=2) +
	  facet_grid(ClientId ~ .) +
	  ggtitle(paste(prefix, "Latencies")) +
	  theme_bw() +
	  theme(text = element_text(size=15),
			axis.text = element_text(size=20))
dev.off()

png(paste(prefix, "_wait.mon.png", sep=""), height=1024, width=1024)
if(grepl('04', prefix)) {
	print({
		ggplot(wait.mon[wait.mon$ServerId < 5,]) +
		  geom_line(aes(y=WaitingRequests, x=Timestamp), size=2) +
		  facet_grid(ServerId ~ .) +
		  ylim(c(0, 7000)) +
		  ggtitle(paste(prefix, "Wait")) +
		  theme_bw() +
		  theme(text = element_text(size=15),
				axis.text = element_text(size=20))
	})
	dev.off()
} else {
	print({
		ggplot(wait.mon[wait.mon$ServerId < 5,]) +
		  geom_line(aes(y=WaitingRequests, x=Timestamp), size=2) +
		  facet_grid(ServerId ~ .) +
		  ggtitle(paste(prefix, "Wait")) +
		  theme_bw() +
		  theme(text = element_text(size=15),
				axis.text = element_text(size=20))
	})
	dev.off()
}

server.rate <- data.table(server.rate)
servers <- unique(server.rate[,ServerId])
presentRate <- server.rate[1,ServerRate]
presentTimestamp <- server.rate[1,Timestamp]
# insert dots at the end of each service time interval to make server rate plots "quadratic"
for (i in 1:nrow(server.rate)) {
	if(presentTimestamp != server.rate[i,Timestamp]) {
		for(ii in servers) {
			server.rate <- rbind(server.rate, list(0, server.rate[i,Timestamp] - 1, ii, presentRate))
		}
		presentTimestamp <- server.rate[i,Timestamp]
		presentRate <- server.rate[i,ServerRate]
	}
}
server.rate.agg <- server.rate[,sum(ServerRate * CONCURRENCY),by=list(Timestamp)]

rate <- read.table(paste("../logs/", prefix, "_Rate", sep=""))
colnames(rate)[1] <- "ClientId"
colnames(rate)[2] <- "Timestamp"
colnames(rate)[3] <- "ServerId"
colnames(rate)[4] <- "Rate"

normalizedRates <- data.frame(ClientId=character(),
                 Timestamp=integer(),
                 Rate=double(),
                 stringsAsFactors=FALSE)
clients <- unique(rate[,"ClientId"])
servers <- unique(rate[,"ServerId"])
for (i in 1:length(clients)) {
  clientrate <- rate[rate$ClientId==clients[i],]
  ht = 0
  sum = 0
  count = 0
  for (ii in 1:nrow(clientrate)) {
    oldHt = ht
    ht = as.integer(clientrate[ii, "Timestamp"] / 100) * 100

    if(ht != oldHt) {
      normalizedRates[nrow(normalizedRates)+1,] <- c(clientrate[1, "ClientId"], oldHt, sum / count)
      sum = 0
      count = 0
    }

    count = count + 1
    sum = sum + clientrate[ii, "Rate"]
  }
  if(ht != oldHt) {
    normalizedRates[nrow(normalizedRates)+1,] <- c(clientrate[1, "ClientId"], as.integer(clientrate[ii, "Timestamp"] / 100) * 100, sum / count)
    sum = 0
    count = 0
  }
}

############################# DEBUG #####################################
#dtrate <- data.table(rate)
#png(paste(prefix, "_rates_anfang.png", sep=""), height=8000, width=8000)
#ggplot(dtrate) +
#	geom_line(aes(y=Rate, x=Timestamp), size=1) +
#	# geom_point(aes(y=Rate, x=Timestamp, colour=ClientId), size=2) +
#	# geom_smooth(aes(y=V1, x=Timestamp), size=4) +
#	facet_grid(ClientId ~ .) +
#	ggtitle(paste(prefix, "rate")) +
#	theme(text = element_text(size=90),
#		axis.text = element_text(size=90),
#		legend.key.size = unit(4, "cm"))
#dev.off()
############################# DEBUG #####################################

### prepare actual client rate from pending request log #################
rate.actual <- data.table(sent.requests)
rate.actual$Timegroup <- as.integer(rate.actual$Timestamp / 100)
rate.actual <- rate.actual[,length(SentRequests),by=list(Timegroup)] # oder sum
### TAKE CARE: the server rates are per 1 ms so here we have to divide the number of requests
### by 100 to get for the 100 timeunits interval the number of sent requests per time unit
rate.actual$V1 <- rate.actual$V1 / 100
setnames(rate.actual, "Timegroup", "Timestamp")
rate.actual$Timestamp <- rate.actual$Timestamp * 100
rate.actual[,role:=c('clients actual rate')]
#########################################################################


normalizedRates.agg <- data.table(normalizedRates)
normalizedRates.agg <- normalizedRates.agg[,sum(Rate),by=list(Timestamp)]
normalizedRates.agg$V1 <- normalizedRates.agg$V1 * length(servers)
normalizedRates.agg$V1 <- normalizedRates.agg$V1 / RATE_INTERVAL
normalizedRates.agg[,role:=c('clients allowed rate')]
server.rate.agg[,role:=c('servers service rate')]
normalizedRates.agg <- rbind(normalizedRates.agg, server.rate.agg)
normalizedRates.agg <- rbind(normalizedRates.agg, rate.actual)
png(paste(prefix, "_rates.png", sep=""), height=2096, width=4500)
if(grepl('01', prefix)) {
	print({
		ggplot(normalizedRates.agg) +
		geom_line(aes(y=V1, x=Timestamp, color=role), size=4) +
		ylim(c(0, 25)) +
		ggtitle(paste(prefix, "rate per ms")) +
		theme_bw() +
		theme(text = element_text(size=90),
			axis.text = element_text(size=90),
			legend.key.size = unit(4, "cm"))
	})
	dev.off()
} else if(grepl('02', prefix)) {
	print({
		ggplot(normalizedRates.agg) +
		geom_line(aes(y=V1, x=Timestamp, color=role), size=4) +
		ylim(c(0, 40)) +
		xlim(c(0, 12000)) +
		ggtitle(paste(prefix, "rate per ms")) +
		theme_bw() +
		theme(text = element_text(size=90),
			axis.text = element_text(size=90),
			legend.key.size = unit(4, "cm"))
	})
	dev.off()
} else if(grepl('03', prefix)) {
	print({
		ggplot(normalizedRates.agg) +
		geom_line(aes(y=V1, x=Timestamp, color=role), size=4) +
		ylim(c(0, 33)) +
		xlim(c(0, 38000)) +
		ggtitle(paste(prefix, "rate per ms")) +
		theme_bw() +
		theme(text = element_text(size=90),
			axis.text = element_text(size=90),
			legend.key.size = unit(4, "cm"))
	})
	dev.off()
} else if(grepl('04', prefix)) {
	print({
		ggplot(normalizedRates.agg) +
		geom_line(aes(y=V1, x=Timestamp, color=role), size=4) +
		ylim(c(0, 15)) +
		xlim(c(0, 45000)) +
		ggtitle(paste(prefix, "rate per ms")) +
		theme_bw() +
		theme(text = element_text(size=90),
			axis.text = element_text(size=90),
			legend.key.size = unit(4, "cm"))
	})
	dev.off()
}
