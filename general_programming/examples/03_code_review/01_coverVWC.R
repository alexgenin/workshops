require(ggplot2)
require(reshape2)

sp <- read.csv('sp.csv')
atts <- read.csv('Attributes.csv')

# melt species data frame
spm <- melt(sp, id.vars= 1, measure.vars= 2:287, variable.name= 'species', value.name= 'cover')

# merge water content
spm <- merge(spm, atts[,c('unqID','meanVWC')])

# drop water content NAs
spm <- spm[!is.na(spm$meanVWC),]

# reorder species factor by inverse of mean (so most abundant is first)
spm$speciesOrder <- reorder(spm$species, spm$cover, function(x) 1/mean(x))

# bin water content by 10% increments
spm$waterBin <- cut(spm$meanVWC, breaks= seq(0, 100, by= 10), include.lowest= TRUE)

# calculate mean species cover in water bins
spmMean <- ddply(spm, ~speciesOrder + waterBin, summarise, cover= mean(cover))

# stacked area of cover for top 12 species
ggplot(subset(spmMean, speciesOrder %in% levels(speciesOrder)[1:12]), aes(waterBin, cover, group= speciesOrder, fill= speciesOrder)) + geom_area() + scale_fill_brewer(type= 'qual', palette= 'Paired')

# line plot cover for top 12 species
ggplot(subset(spmMean, speciesOrder %in% levels(speciesOrder)[1:12]), aes(waterBin, cover, group= speciesOrder, color= speciesOrder)) + geom_line(size= 1.5) + scale_color_brewer(type= 'qual', palette= 'Paired')

# fit some smooth lines to the raw cover and water content data
ggplot(subset(spm, speciesOrder %in% levels(speciesOrder)[1:12]), aes(meanVWC, cover, color= speciesOrder)) + stat_smooth(se= FALSE, size= 1.5) + scale_color_brewer(type= 'qual', palette= 'Paired')

# fit 2nd-order polynomial logistic curves to species presence
spm$presence <- ifelse(spm$cover == 0, 0, 1)

ggplot(subset(spm, speciesOrder %in% levels(speciesOrder)[1:12]), aes(meanVWC, presence, color= speciesOrder)) + stat_smooth(se= FALSE, size= 1.5, method= 'glm', family= 'binomial', formula= 'y ~ poly(x, 2)') + scale_color_brewer(type= 'qual', palette= 'Paired')