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

		if(grepl('03', prefix)) {
			latency <- latency[latency$ClientId=='Client3' | latency$ClientId=='Client4' | latency$ClientId=='Client5' | latency$ClientId=='Client6' | latency$ClientId=='Client7' | latency$ClientId=='Client8' | latency$ClientId=='Client9',]
		}

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

		### prepare actual client rate from pending request log #################
		rate.actual <- data.table(sent.requests)
		rate.actual$Timegroup <- as.integer(rate.actual$Timestamp / 100)
		rate.actual <- rate.actual[,length(SentRequests),by=list(Timegroup)] # oder sum
		### TAKE CARE: the server rates are per 1 ms so here we have to divide the number of requests
		### by 100 to get for the 100 timeunits interval the number of sent requests per time unit
		rate.actual$V1 <- rate.actual$V1 / 100
		setnames(rate.actual, "Timegroup", "Timestamp")
		rate.actual$Timestamp <- rate.actual$Timestamp * 100
		rate.actual[,role:=c('clients actual')]
		#########################################################################


		normalizedRates.agg <- data.table(normalizedRates)
		normalizedRates.agg <- normalizedRates.agg[,sum(Rate),by=list(Timestamp)]
		normalizedRates.agg$V1 <- normalizedRates.agg$V1 * length(servers)
		normalizedRates.agg$V1 <- normalizedRates.agg$V1 / RATE_INTERVAL
		normalizedRates.agg[,role:=c('clients target')]
		server.rate.agg[,role:=c('servers')]
		normalizedRates.agg <- rbind(normalizedRates.agg, server.rate.agg)
		normalizedRates.agg <- rbind(normalizedRates.agg, rate.actual)
		colnames(normalizedRates.agg)[2] <- "Rate" # renaming from V1 to Rate
		colnames(server.rate.agg)[2] <- "Rate" # renaming from V1 to Rate
		if(grepl('01', prefix)) {
			plotList[[listPos]] <- ggplot(normalizedRates.agg) +
			geom_line(aes(y=Rate, x=Timestamp, color=role), size=4) +
			ylim(c(0, 25)) +
			ggtitle(algo) +
			theme_bw() +
			theme(text = element_text(size=90),
				axis.text = element_text(size=90),
				legend.key.size = unit(4, "cm"))
		} else if(grepl('02', prefix)) {
			plotList[[listPos]] <- ggplot(normalizedRates.agg) +
			geom_line(aes(y=Rate, x=Timestamp, color=role), size=4) +
			ylim(c(0, 40)) +
			xlim(c(0, 12000)) +
			ggtitle(algo) +
			theme_bw() +
			theme(text = element_text(size=90),
				axis.text = element_text(size=90),
				legend.key.size = unit(4, "cm"))
		} else if(grepl('03b', prefix)) {
			plotList[[listPos]] <- ggplot(normalizedRates.agg) +
			geom_line(aes(y=Rate, x=Timestamp, color=role), size=4) +
			ylim(c(0, 30)) +
			xlim(c(0, 38000)) +
			ggtitle(algo) +
			theme_bw() +
			theme(text = element_text(size=90),
				axis.text = element_text(size=90),
				legend.key.size = unit(4, "cm"))
		} else if(grepl('03', prefix)) {
			plotList[[listPos]] <- ggplot(normalizedRates.agg) +
			geom_line(aes(y=Rate, x=Timestamp, color=role), size=4) +
			ylim(c(0, 33)) +
			xlim(c(0, 38000)) +
			ggtitle(algo) +
			theme_bw() +
			theme(text = element_text(size=90),
				axis.text = element_text(size=90),
				legend.key.size = unit(4, "cm"))
		} else if(grepl('04', prefix)) {
			plotList[[listPos]] <- ggplot(normalizedRates.agg) +
			geom_line(aes(y=Rate, x=Timestamp, color=role), size=4) +
			ylim(c(0, 15)) +
			xlim(c(0, 45000)) +
			ggtitle(algo) +
			theme_bw() +
			theme(text = element_text(size=90),
				axis.text = element_text(size=90),
				legend.key.size = unit(4, "cm"))
		}
	}
	png(paste(scenarioId, "_allrates.png", sep=""), height=2096, width=4500)
	do.call("grid.arrange", c(plotList, ncol=2))
	dev.off()
}




