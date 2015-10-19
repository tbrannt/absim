require(ggplot2)
require(data.table)
require(grid) # unit function
require(gridExtra) # multiple plots in one

CONCURRENCY <- 4
SHADOW_READ_RATIO <- 0.1
REPLICATION_FACTOR <- 3
RATE_INTERVAL <- 20

options(digits=16) # for summary to be less a pain in the a...

for (scenarioId in c('01', '02', '02b', '03', '03b', '04')) {
	plotList <- list()
	for (algo in c('AIMD', 'BIC', 'PISC', 'C3')) {
		listPos <- length(plotList) + 1
		prefix <- paste(scenarioId,'_',algo, sep="")

		latency <- read.table(paste("../logs/", prefix, "_Latency", sep=""))
		colnames(latency)[1] <- "ServerId"
		colnames(latency)[2] <- "Timestamp"
		colnames(latency)[3] <- "Latency"
		colnames(latency)[4] <- "ClientId"

		trim <- 10

		print(paste('Printing statistic for', scenarioId, algo))
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
				quantile(latency$Latency,c(0.5, 0.95, 0.99, 0.999, 0.9999, 0.99999)),
				range50, range95, range99))
		print(paste('Done with statistic for', scenarioId, algo))

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

		sent.requests <- read.table(paste("../logs/", prefix, "_SentRequests", sep=""))
		colnames(sent.requests)[1] <- "ClientId"
		colnames(sent.requests)[2] <- "Timestamp"
		colnames(sent.requests)[3] <- "ServerId"
		colnames(sent.requests)[4] <- "SentRequests"

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


		server.rate <- data.table(server.rate)
		servers <- unique(server.rate[,ServerId])
		presentRate <- server.rate[1,ServerRate]
		presentTimestamp <- server.rate[1,Timestamp]
		for (i in 1:nrow(server.rate)) {
			if(presentTimestamp != server.rate[i,Timestamp]) {
				for (ii in servers) {
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
		
		plotList[[listPos]] <- ggplot(act.mon) +
	  		geom_line(aes(y=ActiveRequests, x=Timestamp), size=2) + 
	  		facet_grid(ServerId ~ .) +
	  		ggtitle(algo) +
	  		theme_bw() +
	  		theme(text = element_text(size=60),
			axis.text = element_text(size=30),
			legend.key.size = unit(2, "cm"))
		
	}

	#png(paste(prefix, "_act.mon.png", sep=""), height=512, width=1024)
	png(paste(scenarioId, "_allactmons.png", sep=""), height=1536, width=3072)
	do.call("grid.arrange", c(plotList, ncol=2))
	dev.off()
}




