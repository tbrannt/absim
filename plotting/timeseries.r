require(ggplot2)
require(data.table)

args <- commandArgs(trailingOnly = TRUE)

prefix <- args[1]
CONCURRENCY <- 4
SHADOW_READ_RATION <- 0.1
REPLICATION_FACTOR <- 3

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

act.mon <- read.table(paste("../logs/", prefix, "_ActMon", sep=""))
colnames(act.mon)[1] <- "ServerId"
colnames(act.mon)[2] <- "Timestamp"
colnames(act.mon)[3] <- "ActiveRequests"

#trim <- 100 # TODO: delete
wait.mon <- read.table(paste("../logs/", prefix, "_WaitMon", sep=""))
colnames(wait.mon)[1] <- "ServerId"
colnames(wait.mon)[2] <- "Timestamp"
colnames(wait.mon)[3] <- "WaitingRequests"
#wait.mon <- wait.mon[wait.mon$Timestamp > trim,] # TODO: delete

pending.requests <- read.table(paste("../logs/", prefix, "_PendingRequests", sep=""))
colnames(pending.requests)[1] <- "ClientId"
colnames(pending.requests)[2] <- "Timestamp"
colnames(pending.requests)[3] <- "ServerId"
colnames(pending.requests)[4] <- "PendingRequests"

latency.samples <- read.table(paste("../logs/", prefix, "_LatencyTracker", sep=""))
colnames(latency.samples)[1] <- "ClientId"
colnames(latency.samples)[2] <- "Timestamp"
colnames(latency.samples)[3] <- "ServerId"
colnames(latency.samples)[4] <- "LatencySample"

server.rate <- read.table(paste("../logs/", prefix, "_serverRate", sep=""))
colnames(server.rate)[1] <- "Dummy"
colnames(server.rate)[2] <- "Timestamp"
colnames(server.rate)[3] <- "ServerId"
colnames(server.rate)[4] <- "ServerRate"

server.serviceTime <- read.table(paste("../logs/", prefix, "_serverServiceTime", sep=""))
colnames(server.serviceTime)[1] <- "ServerId"
colnames(server.serviceTime)[2] <- "Timestamp"
colnames(server.serviceTime)[3] <- "ServiceTime"

p1 <- ggplot(latency) +
	  geom_point(aes(y=Latency, x=Timestamp, colour=ClientId), size=2) +
	  facet_grid(ClientId ~ .) +
	  ggtitle(paste(prefix, "Latencies")) +
	  theme(text = element_text(size=15), 
			axis.text = element_text(size=20))
ggsave(p1, file=paste(prefix, "_latency.png", sep=""), width=15)

p1 <- ggplot(act.mon) +
	  geom_line(aes(y=ActiveRequests, x=Timestamp), size=2) + 
	  facet_grid(ServerId ~ .) +
	  ggtitle(paste(prefix, "Act")) +
	  theme(text = element_text(size=15), 
	  		axis.text = element_text(size=20))
ggsave(p1, file=paste(prefix, "_act.mon.png", sep=""), width=15)

p1 <- ggplot(wait.mon[wait.mon$ServerId < 5,]) + 
	  geom_line(aes(y=WaitingRequests, x=Timestamp), size=2) + 
	  facet_grid(ServerId ~ .) +
	  # ggtitle(paste(prefix, "Wait")) +
	  theme(text = element_text(size=15), 
	  		axis.text = element_text(size=20))
ggsave(p1, file=paste(prefix, "_wait.mon.png", sep=""), width=15)

p1 <- ggplot(pending.requests[pending.requests$ClientId == "Client0",]) + 
	  geom_point(aes(y=PendingRequests, x=Timestamp), size=4) + 
	  facet_grid(ServerId ~ ClientId) +
	  ggtitle(paste(prefix, "Pending")) +
	  theme(text = element_text(size=15), 
	  		axis.text = element_text(size=20))
ggsave(p1, file=paste(prefix, "_pending.requests.png", sep=""), width=15)

p1 <- ggplot(latency.samples) + 
	  geom_point(aes(y=LatencySample, x=Timestamp, colour=ClientId), size=2) + 
	  facet_grid(ServerId ~ .) +
	  ggtitle(paste(prefix, "Latency Samples")) +
	  theme(text = element_text(size=15), 
	  		axis.text = element_text(size=20))
ggsave(p1, file=paste(prefix, "_latency.samples.png", sep=""), width=15)

server.rate <- data.table(server.rate)
server.rate.agg <- server.rate[,sum(ServerRate * CONCURRENCY),by=list(Timestamp)]
server.rate.agg$V1 <- server.rate.agg$V1 / (SHADOW_READ_RATION * (REPLICATION_FACTOR - 1) + 1)

p1 <- ggplot(server.rate.agg[server.rate.agg$Timestamp < 10000,]) + 
	  geom_point(aes(y=V1, x=Timestamp), size=2) + 
	  ggtitle(paste(prefix, "Server Rate")) +
	  theme(text = element_text(size=15), 
	  		axis.text = element_text(size=20))
ggsave(p1, file=paste(prefix, "_server.rate.png", sep=""), width=15)

#hier server rate vs client rate..
#p1 <- ggplot(server.rate.agg[server.rate.agg$Timestamp < 10000,]) + 
#	  geom_line(aes(y=V1, x=Timestamp), size=2) + 
#	  ggtitle(paste(prefix, "Server Servicetimes")) +
#	  theme(text = element_text(size=15), 
#	  		axis.text = element_text(size=20))
#ggsave(p1, file=paste(prefix, "_server.serviceTime.pdf", sep=""), width=15)

p1 <- ggplot(server.serviceTime) +
	  geom_point(aes(y=ServiceTime, x=Timestamp, colour=ServerId), size=2) +
	  ggtitle(paste(prefix, "Server Actual Servicetimes")) +
	  theme(text = element_text(size=15),
			axis.text = element_text(size=20))
ggsave(p1, file=paste(prefix, "_server.actualServiceTime.png", sep=""), width=15)

rate <- read.table(paste("../logs/", prefix, "_Rate", sep=""))
colnames(rate)[1] <- "ClientId"
colnames(rate)[2] <- "Timestamp"
colnames(rate)[3] <- "ServerId"
colnames(rate)[4] <- "Rate"

trim <- 100
per_client.rate <- rate[rate$ServerId == 0,]
#per_client.rate <- per_client.rate[rate$Timestamp > trim,]
p1 <- ggplot(per_client.rate) +
	geom_line(aes(y=Rate, x=Timestamp, colour=ClientId), linetype='dashed',size=1) +
	geom_smooth(aes(y=Rate, x=Timestamp, colour=ClientId), size=4) +
	facet_grid(ClientId ~ .) +
	ggtitle(paste(prefix, "Server0 Rates")) +
	theme(text = element_text(size=15),
		axis.text = element_text(size=20))
ggsave(p1, file=paste(prefix, "_per_client_rate.png", sep=""), height=30, width=50, limitsize=FALSE)

per_server.rate <- rate[rate$ClientId == "Client0",]
#per_server.rate <- per_server.rate[rate$Timestamp > trim,]
p1 <- ggplot(per_server.rate) +
	geom_line(aes(y=Rate, x=Timestamp, colour=ServerId), linetype='dashed',size=1) +
	geom_smooth(aes(y=Rate, x=Timestamp, colour=ServerId), size=4) +
	facet_grid(ServerId ~ .) +
	ggtitle(paste(prefix, "Client0 Rates")) +
	theme(text = element_text(size=15),
		axis.text = element_text(size=20))
ggsave(p1, file=paste(prefix, "_per_server_rate.png", sep=""), height=30, width=50, limitsize=FALSE)

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

RATE_INTERVAL <- 20
normalizedRates.agg <- data.table(normalizedRates)
normalizedRates.agg <- normalizedRates.agg[,sum(Rate),by=list(Timestamp)]
normalizedRates.agg$V1 <- normalizedRates.agg$V1 * length(servers)
normalizedRates.agg$V1 <- normalizedRates.agg$V1 / RATE_INTERVAL
normalizedRates.agg[,role:=c('clients')]
server.rate.agg[,role:=c('servers')]
normalizedRates.agg <- rbind(normalizedRates.agg, server.rate.agg)
p1 <- ggplot(normalizedRates.agg) +
  	geom_line(aes(y=V1, x=Timestamp, color=role), size=5) +
 	# geom_point(aes(y=Rate, x=Timestamp, colour=ClientId), size=2) +
 	# geom_smooth(aes(y=V1, x=Timestamp), size=4) +
 	#facet_grid(ServerId ~ .) +
 	ggtitle(paste(prefix, "rate")) +
 	theme(text = element_text(size=15),
 		axis.text = element_text(size=20))
ggsave(p1, file=paste(prefix, "_rates.pdf", sep=""), height=30, width=50, limitsize=FALSE)


# rate <- read.table(paste("../logs/", prefix, "_ReceiveRate", sep=""))
# colnames(rate)[1] <- "ClientId"
# colnames(rate)[2] <- "Timestamp"
# colnames(rate)[3] <- "ServerId"
# colnames(rate)[4] <- "Rate"

# rate <- rate[rate$ClientId == "Client0",]

# p1 <- ggplot(rate) + 
# 	  geom_line(aes(y=Rate, x=Timestamp, colour=ClientId), size=1) + 
# 	  geom_point(aes(y=Rate, x=Timestamp, colour=ClientId), size=2) + 
# 	  geom_smooth(aes(y=Rate, x=Timestamp), size=2) + 
# 	  facet_grid(ServerId ~ ClientId) +
# 	  ggtitle(paste(prefix, "rate")) +
# 	  theme(text = element_text(size=15), 
# 	  		axis.text = element_text(size=20))
# ggsave(p1, file=paste(prefix, "_ReceiveRate.pdf", sep=""), height=30, width=50, limitsize=FALSE)


# rate <- read.table(paste("../logs/", prefix, "_serverRR", sep=""))
# colnames(rate)[1] <- "ServerId"
# colnames(rate)[2] <- "Timestamp"
# colnames(rate)[3] <- "Tick"
# colnames(rate)[4] <- "ClientId"

# rate <- data.table(rate)

# p1 <- ggplot(rate) +
# 	  geom_histogram(aes(x=Timestamp, alpha = 0.5), binwidth=20) +
# 	  ylim(c(0, 30)) +
# 	  ylab("Requests per 20ms") +
# 	  xlab("Time (ms)") +
# 	  facet_grid(ClientId ~ ServerId) + 
# 	  ggtitle(paste(prefix, "ServerSideRate")) +
# 	  theme(text = element_text(size=15), 
# 	  		axis.text = element_text(size=20))
# ggsave(p1, file=paste(prefix, "_ServerSideRate.pdf", sep=""), height=30, width=50, limitsize=FALSE)

# rate <- rate[rate$ClientId == "Client0",]

# p1 <- ggplot(rate) + 
# 	  geom_line(aes(y=Rate, x=Timestamp, colour=ClientId), size=1) + 
# 	  geom_point(aes(y=Rate, x=Timestamp, colour=ClientId), size=2) + 
# 	  geom_smooth(aes(y=Rate, x=Timestamp), size=2) + 
# 	  facet_grid(ServerId ~ ClientId) +
# 	  ggtitle(paste(prefix, "rate")) +
# 	  theme(text = element_text(size=15), 
# 	  		axis.text = element_text(size=20))
# ggsave(p1, file=paste(prefix, "_ReceiveRate.pdf", sep=""), height=30, width=50, limitsize=FALSE)



# tokens <- read.table(paste("../logs/", prefix, "_Tokens", sep=""))
# colnames(tokens)[1] <- "ClientId"
# colnames(tokens)[2] <- "Timestamp"
# colnames(tokens)[3] <- "ServerId"
# colnames(tokens)[4] <- "Tokens"


# p1 <- ggplot(tokens) + 
# 	  geom_line(aes(y=Tokens, x=Timestamp, colour=ClientId), size=1) + 
# 	  geom_point(aes(y=Tokens, x=Timestamp, colour=ClientId), size=2) +
# 	  geom_smooth(aes(y=Tokens, x=Timestamp), size=2) + 
# 	  facet_grid(ServerId ~ ClientId) +
# 	  ggtitle(paste(prefix, "tokens")) +
# 	  theme(text = element_text(size=15), 
# 	  		axis.text = element_text(size=20))
# ggsave(p1, file=paste(prefix, "_tokens.pdf", sep=""), height=30, width=50, limitsize=FALSE)

# edScore <- read.table(paste("../logs/", prefix, "_EdScore", sep=""))
# colnames(edScore)[1] <- "ClientId"
# colnames(edScore)[2] <- "Timestamp"
# colnames(edScore)[3] <- "ServerId"
# colnames(edScore)[4] <- "QSZ"
# colnames(edScore)[5] <- "MU"
# colnames(edScore)[6] <- "Theta"
# colnames(edScore)[7] <- "Score"

# edScore$FloorTimestamp <- (floor(edScore$Timestamp))
# edScore <- data.table(edScore)
# edScore[,sum()]

# p1 <- ggplot() + 
# 	  geom_point(data=edScore[edScore$ServerId == 1 & edScore$Timestamp < 10000, ],
# 	  			 aes(y=Theta, x=Timestamp, colour=ClientId), size=2) +
# 	  geom_point(data=wait.mon[wait.mon$ServerId == 1  & wait.mon$Timestamp < 10000,],
# 	  			 aes(y=WaitingRequests, x=Timestamp), size=2) +
# 	  ggtitle(paste(prefix, "edScore")) +
# 	  theme(text = element_text(size=15), 
# 	  		axis.text = element_text(size=20))
# ggsave(p1, file=paste(prefix, "_edScore.pdf", sep=""), height=30, width=50, limitsize=FALSE)
