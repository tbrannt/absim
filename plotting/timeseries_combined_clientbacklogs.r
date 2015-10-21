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

		client.backlog <- read.table(paste("../logs/", prefix, "_clientBacklogs", sep=""))
		colnames(client.backlog)[1] <- "ClientId"
		colnames(client.backlog)[2] <- "Timestamp"
		colnames(client.backlog)[3] <- "Size"

		trim <- 2000

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

		
		
		ggplot(client.backlog) +
			  geom_line(aes(y=Size, x=Timestamp), size=2) +
			  facet_grid(ClientId ~ .) +
			  ggtitle(paste(prefix, "Client Backlogs")) +
			  theme_bw() +
			  theme(text = element_text(size=15),
					axis.text = element_text(size=20))
		

		if(grepl('02', prefix)) {
			plotList[[listPos]] <- ggplot(client.backlog) +
				geom_line(aes(y=Size, x=Timestamp), size=2) +
				facet_grid(ClientId ~ .) +
				ylim(c(0, 400)) +
				ggtitle(algo) +
				theme_bw() +
				theme(text = element_text(size=40),
					axis.text = element_text(size=30))		
		} else if(grepl('03b', prefix)) {
			plotList[[listPos]] <- ggplot(client.backlog) +
				geom_line(aes(y=Size, x=Timestamp), size=2) +
				facet_grid(ClientId ~ .) +
				ylim(c(0, 400)) +
				ggtitle(algo) +
				theme_bw() +
				theme(text = element_text(size=40),
					axis.text = element_text(size=30))	
		} else if(grepl('03', prefix)) {
			plotList[[listPos]] <- ggplot(client.backlog) +
				geom_line(aes(y=Size, x=Timestamp), size=2) +
				facet_grid(ClientId ~ .) +
				ylim(c(0, 400)) +
				ggtitle(algo) +
				theme_bw() +
				theme(text = element_text(size=40),
					axis.text = element_text(size=30))	
		} else {
			plotList[[listPos]] <- ggplot(client.backlog) +
				geom_line(aes(y=Size, x=Timestamp), size=2) +
				facet_grid(ClientId ~ .) +
				ylim(c(0, 400)) +
				ggtitle(algo) +
				theme_bw() +
				theme(text = element_text(size=40),
					axis.text = element_text(size=30))	
		}
	}

	#png(paste(prefix, "_act.mon.png", sep=""), height=512, width=1024)
	png(paste(scenarioId, "_allclientbacklogs.png", sep=""), height=3072, width=3072)
	do.call("grid.arrange", c(plotList, ncol=2))
	dev.off()
}




