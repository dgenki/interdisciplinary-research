#Script: diversity_measures_1.R
#Author: Diego Chavarro, Based on Rafols and Meyer(2010)
#Version: 0.96
#use: change your the line starting with setwd to the directory in which you put the script. 
#In windows it may require that you write in the form C:\\folder\\. Then, start R and write source('path_to_diversity_measures_1.R')
#last modified:21/6/2011

#for windows users: call the function flush.console() in R to update the output 

#set the working directory. Change to your directory

	setwd("C:\\Documents and Settings\\your user name\\path to\\your folder")

#import matrix, with each article identifier in rows and categories in columns, with the number of references in each column.
	print("reading data...")
	artCatData <- read.table("articles_sample.csv", header=TRUE, sep=",", row.names="id")
	artCatX <- as.matrix(artCatData)
	simData <-read.table("cosine_similarity_matrix_sc.csv", header=TRUE, sep=",")
	simX <- as.matrix(simData[, -1]);
	print("done")

#these are the categories(variety). This is a very fast way to count data meeting a specific criteria
	print("calculating variety...")
	variety <- rowSums(artCatX>0, na.rm=TRUE)
	print("done")
	write.table(t(t(variety)), "variety.txt", quote = TRUE, sep = "\t", na = "0", row.names=TRUE, col.names=NA)

#these are the proportions
	print("calculating proportions...")
	propX <- artCatX / rowSums(artCatX, na.rm=TRUE)
	write.table(propX, "proportions.txt", quote = TRUE, sep = "\t", na = "0", row.names=FALSE)
	print("done")

#this is Shannon's entropy
	print("calculating shannon's entropy...")
	shannon <- -1 * rowSums(propX * log(propX), na.rm=TRUE)
	write.table(t(t(shannon)), "shannon.txt", quote = TRUE, sep = "\t", na = "0", row.names=TRUE, col.names=NA)
	print("done")

#evenness
	print("calculating evenness...")
	evenness <- shannon/log(t(t(variety)))
	write.table(t(t(evenness)), "evenness.txt", quote = TRUE, sep = "\t", na = "0", row.names=TRUE, col.names=NA)
	print("done")

#this is Simpson's diversity
	print("calculating Simpson's diversity...")
	simpson <- 1 - rowSums(propX ^ 2, na.rm=TRUE)
	write.table(t(t(simpson)), "simpson.txt", quote = TRUE, sep = "\t", na = "0", row.names=TRUE, col.names=NA)
	print("done")

#this is Stirling's diversity. todo: if simX and propX don't have the same columns, print an error
	print("calculating Rao-Stirling's diversity...")
	stirling <- rowSums(propX %*% (1 - simX) * propX)
	write.table(stirling, "stirling.txt", quote = TRUE, sep = "\t", na = "0", row.names=TRUE, col.names=NA)
	print("done")
#disparity
	print("calculating disparity...")
	propXa <- propX
#change >0 to 1
	propXa[propXa>0] <- 1
	disparity <- rowSums((propXa %*% (1 - simX) * propXa)/(variety*(variety-1)))
	write.table(disparity, "disparity.txt", quote = TRUE, sep = "\t", na = "0", row.names=TRUE, col.names=NA)
	print("done")
fieldNames <- c("variety","shannon","simpson","evenness", "stirling", "disparity")
result <- cbind(t(t(variety)),t(t(shannon)),t(t(simpson)), t(t(evenness)), stirling, disparity)
table <-rbind(fieldNames, result)

write.table(table, "results.txt", quote = TRUE, sep = "\t", na = "0", row.names=TRUE, col.names=NA)
