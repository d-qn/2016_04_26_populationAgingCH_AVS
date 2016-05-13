############################################################################################
###		SETTINGS
############################################################################################
library(plyr)
source("~/swissinfo/_helpers/helpers.R")
font <- "Open Sans"

library(animation)

data.file <- 'contenate_allData.csv'

############################################################################################
###		Load all data in one data.frame
############################################################################################

data <- read.csv(data.file, stringsAsFactors = F)
colnames(data)[2:ncol(data)] <- gsub("^X", "", colnames(data)[2:ncol(data)])

data <- data.frame(Age = data[,1], melt(data[,2:ncol(data)]))
colnames(data) <- c('Age', 'Annee', "value")
data$Annee <- as.numeric(as.character(data$Annee))
datan <- data
data$Age <- reorder(data$Age, as.numeric(gsub("\\+$", "", as.character(data$Age))))
datan$Age <- as.numeric(gsub("\\+$", "", as.character(datan$Age)))

# compute for each year, the proportion of by age group
prop <- plyr::ddply(data, .(Annee), summarize,
	Age = Age,
	value = value,
	prop = value / sum(value) * 100)


#ggplot(data = datan) + geom_histogram(aes(Age, value)) + facet_wrap( ~ Annee) + theme_minimal()


#ggplot(data = prop) + geom_bar(aes(Age, prop), stat = "identity") + facet_wrap( ~ Annee) + theme_minimal()
xlabel <- rep('', nlevels(data$Age))
idx.x <- c(seq(min(as.numeric(data$Age)), max(as.numeric(data$Age)), 10), nlevels(data$Age))
xlabel[idx.x]<- levels(data$Age)[idx.x]
descr <- paste("proportion de la population suisse par âge (", paste(range(data$Annee), collapse ="-"), ")", sep="")
title <- 'Vieillissement de la population'

# load logo
g <- rasterGrob(swi_logo, interpolate=TRUE)


### Get some key numbers
sum(dplyr::filter(datan, Annee == 1860, Age > 65)$value) / sum(dplyr::filter(datan, Annee == 1860, Age >= 20, Age <= 64)$value)
sum(dplyr::filter(datan, Annee == 1901, Age > 65)$value) / sum(dplyr::filter(datan, Annee == 1901, Age >= 20, Age <= 64)$value)

sum(dplyr::filter(datan, Annee == 2012, Age > 65)$value) / sum(dplyr::filter(datan, Annee == 2012, Age >= 20, Age <= 64)$value)

plotayear <- function(data, a, title = "", descr = "") {

	dd <- prop[prop$Annee == a,]
	ghist <- ggplot(data = dd) + geom_bar(aes(Age, prop), size =0.01, stat = "identity",
		color = swi_9palette[4], fill = swi_9palette[5]) + ggtheme  + scale_x_discrete("Age", xlabel) +
		scale_y_continuous(name = "%", limits = c(0, max(prop$prop)), expand = c(0.01,0.01)) +
		# the year in big
		geom_text(data = data.frame(x = levels(prop$Age)[nlevels(prop$Age)-5], y = max(prop$prop)-0.7, label = as.character(a)),
		aes(label = label, x = x, y = y), family = font, alpha = 0.5, size = 50,  color = swi_9palette[9], hjust = 1) +
		# the title
		geom_text(data = data.frame(x = levels(prop$Age)[1],
		y = max(prop$prop)-0.05, label = title), aes(label = label, x = x, y = y), family = font, alpha = 1, size = 7, hjust = 0, vjust = 0,
		fontface ="bold") +
		# the description
		geom_text(data = data.frame(x = levels(prop$Age)[1],
		y = max(prop$prop)-0.15, label = descr), aes(label = label, x = x, y = y), family = font, alpha = 1, size = 4, hjust = 0,vjust =0) +
		theme(axis.text = element_text(size = 14, family = font, lineheight = 0), plot.margin = unit(c(0.7,1,1.1,0), "lines"))
	ghista <- ghist + annotation_custom(grob = g, xmin = nlevels(prop$Age)-nlevels(prop$Age)/10, xmax = nlevels(prop$Age),
	ymin = -0.23, ymax = -0.3)
    gt <- ggplot_gtable(ggplot_build(ghista))
    gt$layout$clip[gt$layout$name=="panel"] <- "off"
	grid.newpage()
    grid.draw(gt)
}
a <- unique(prop$Annee)[10]
plotayear(data, a, title, descr)

#filter the data to only have even years
data.sub <- data[data$Annee %% 4 == 0,]




saveGIF({
	for(a in unique(data.sub$Annee)) {
		plotayear(data.sub, a, title = title, descr)
	}
}, movie.name = "populationAge_new.gif", interval = 0.2, nmax = 50, ani.width = 640, ani.height = 570, loop = TRUE, outdir = getwd())













plotayear2 <- function(data, a, title = "", descr = "") {

	dd <- prop[prop$Annee == a,]
	ghist <- ggplot(data = dd) + geom_bar(aes(Age, prop), size =0.01, stat = "identity",
		color = swi_9palette[4], fill = swi_9palette[5]) + ggtheme  + scale_x_discrete("Age", xlabel) +
		scale_y_continuous(name = "%", limits = c(0, max(prop$prop)), expand = c(0.005,0.005)) +
		# the year in big
		geom_text(data = data.frame(x = levels(prop$Age)[nlevels(prop$Age)-5], y = max(prop$prop)-0.67, label = as.character(a)),
		aes(label = label, x = x, y = y), family = font, alpha = 0.6, size = 60,  color = swi_9palette[9], hjust = 1) +
		# the title
		geom_text(data = data.frame(x = levels(prop$Age)[1],
		y = max(prop$prop)-0.05, label = title), aes(label = label, x = x, y = y), family = font, alpha = 1, size = 9, hjust = 0, vjust = 0,
		fontface ="bold") +
		# the description
		geom_text(data = data.frame(x = levels(prop$Age)[1],
		y = max(prop$prop)-0.15, label = descr), aes(label = label, x = x, y = y), family = font, alpha = 0.8, size = 6, hjust = 0,vjust =0) +
		# theme
		theme(axis.text = element_text(size = rel(1), lineheight = 0), plot.margin = unit(c(0.7,1,1.1,0), "lines"),
		axis.title =  element_text(size = rel(1.5)))
	ghista <- ghist + annotation_custom(grob = g, xmin = nlevels(prop$Age)-nlevels(prop$Age)/8, xmax = nlevels(prop$Age),
	ymin = -0.15, ymax = -0.22)
    gt <- ggplot_gtable(ggplot_build(ghista))
    gt$layout$clip[gt$layout$name=="panel"] <- "off"
	grid.newpage()
    grid.draw(gt)
}
a <- unique(prop$Annee)[10]
plotayear2(data, a, title, descr)


saveGIF({
	for(a in c(unique(data.sub$Annee), 2012)) {
		plotayear2(data.sub, a, title = title, descr)
	}
}, movie.name = "populationAge_long.gif", interval = 0.35, nmax = 50, ani.width = 640*1.1, ani.height = 640*1.1, loop = TRUE, outdir = getwd())