require(ggplot2)
require(data.table)
require(grid) # unit function
require(gridExtra) # multiple plots in one

args <- commandArgs(trailingOnly = TRUE)

prefix <- '04_C3'

CONCURRENCY <- 4
SHADOW_READ_RATIO <- 0.1
REPLICATION_FACTOR <- 3
RATE_INTERVAL <- 20

plotList <- list()

descMap <- new.env(hash=T, parent=emptyenv())
descMap[['01_']] <- 'beta = 0.2, gamma = 0.000004'
descMap[['02_']] <- 'beta = 0.6, gamma = 0.000001'
descMap[['03_']] <- 'beta = 0.9, gamma = 0.000001'
descMap[['04_']] <- 'beta = 0.95, gamma = 0.0000005'

for (preprefix in c('01_', '02_', '03_', '04_')) {
	listPos <- length(plotList) + 1

	#trim <- 100 # TODO: delete
	wait.mon <- read.table(paste("../logs/", preprefix, prefix, "_WaitMon", sep=""))
	colnames(wait.mon)[1] <- "ServerId"
	colnames(wait.mon)[2] <- "Timestamp"
	colnames(wait.mon)[3] <- "WaitingRequests"
	#wait.mon <- wait.mon[wait.mon$Timestamp > trim,] # TODO: delete

	sent.requests <- read.table(paste("../logs/", preprefix, prefix, "_SentRequests", sep=""))
	colnames(sent.requests)[1] <- "ClientId"
	colnames(sent.requests)[2] <- "Timestamp"
	colnames(sent.requests)[3] <- "ServerId"
	colnames(sent.requests)[4] <- "SentRequests"

	server.rate <- read.table(paste("../logs/", preprefix, prefix, "_serverRate", sep=""))
	colnames(server.rate)[1] <- "Dummy"
	colnames(server.rate)[2] <- "Timestamp"
	colnames(server.rate)[3] <- "ServerId"
	colnames(server.rate)[4] <- "ServerRate"

	rate <- read.table(paste("../logs/", preprefix, prefix, "_Rate", sep=""))
	colnames(rate)[1] <- "ClientId"
	colnames(rate)[2] <- "Timestamp"
	colnames(rate)[3] <- "ServerId"
	colnames(rate)[4] <- "Rate"

	png(paste(prefix, "_wait.mon.png", sep=""), height=1024, width=1024)
	if(grepl('04', prefix)) {
		plotList[[listPos]] <- ggplot(wait.mon[wait.mon$ServerId < 5,]) +
			  geom_line(aes(y=WaitingRequests, x=Timestamp), size=2) +
			  facet_grid(ServerId ~ .) +
			  ylim(c(0, 7000)) +
			  theme_bw() +
			  theme(text = element_text(size=40),
					axis.text = element_text(size=40))
	} else {
		plotList[[listPos]] <- ggplot(wait.mon[wait.mon$ServerId < 5,]) +
			  geom_line(aes(y=WaitingRequests, x=Timestamp), size=2) +
			  facet_grid(ServerId ~ .) +
			  theme_bw() +
			  theme(text = element_text(size=40),
					axis.text = element_text(size=40))
	}
	listPos <- length(plotList) + 1

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
	if(grepl('01', prefix)) {
		plotList[[listPos]] <- ggplot(normalizedRates.agg) +
			geom_line(aes(y=V1, x=Timestamp, color=role), size=4) +
			ylim(c(0, 25)) +
			theme_bw() +
			theme(text = element_text(size=50),
				axis.text = element_text(size=50),
				legend.key.size = unit(2, "cm"))
	} else if(grepl('02', prefix)) {
		plotList[[listPos]] <- ggplot(normalizedRates.agg) +
			geom_line(aes(y=V1, x=Timestamp, color=role), size=4) +
			ylim(c(0, 40)) +
			xlim(c(0, 12000)) +
			theme_bw() +
			theme(text = element_text(size=50),
				axis.text = element_text(size=50),
				legend.key.size = unit(2, "cm"))
	} else if(grepl('03', prefix)) {
		plotList[[listPos]] <- ggplot(normalizedRates.agg) +
			geom_line(aes(y=V1, x=Timestamp, color=role), size=4) +
			ylim(c(0, 33)) +
			xlim(c(0, 38000)) +
			theme_bw() +
			theme(text = element_text(size=50),
				axis.text = element_text(size=50),
				legend.key.size = unit(2, "cm"))
	} else if(grepl('04', prefix)) {		
		plotList[[listPos]] <- ggplot(normalizedRates.agg) +
			geom_line(aes(y=V1, x=Timestamp, color=role), size=4) +
			ylim(c(0, 15)) +
			xlim(c(0, 45000)) +
			theme_bw() +
			theme(text = element_text(size=40),
				axis.text = element_text(size=40),
				legend.key.size = unit(2, "cm"))
	}

	png(paste("belegung_", preprefix, "_ratewait.png", sep=""), height=1125, width=4500)
	grid.arrange(plotList[[length(plotList) - 1]], plotList[[length(plotList)]], ncol=2, top=textGrob(descMap[[preprefix]],gp=gpar(fontsize=120,font=1)))
	dev.off()
}


