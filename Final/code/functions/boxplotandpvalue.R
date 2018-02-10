# ---------------------------------------------
#        --- ggplot2 Tutorial Series ---
#        ----------- BOXPLOT -----------           
#
# Nazanin z. Kermani, Damiano Fantini <damiano.fantini@gmail.com>
# ---------------------------------------------
#
boxplotandpvalue <- function(data, outputFilePath)
{
  if (missing(outputFilePath))
    stop("Need to specify output file path to save the plots.")
  
# Load required libraries
#
library(ggplot2)
#
# 
# Custom functions
#
make_contrast_coord <- function(n) {
  tmp <- do.call(rbind,lapply(1:n, (function(i){
    do.call(rbind,lapply(1:n, (function(j){
      if(j > i) {
        c(i,j)  
      }
    })))
  })))
  tmp <- data.frame(tmp)
  colnames(tmp) <- c("str", "end")
  tmp$ave <- apply(tmp, 1, mean)
  tmp$len <- apply(tmp, 1, (function(vct){ max(vct) - min(vct) }))
  return(tmp)
}
pval_to_asterisks <- function(p_vals) {
  astk <- sapply(as.numeric(as.character(p_vals)), (function(pv){
    if(pv >= 0 & pv < 0.0001) {
      "****"
    } else if (pv >= 0 & pv < 0.001) {
      "***"
    } else if (pv >= 0 & pv < 0.01) {
      "**"
    }  else if (pv >= 0 & pv < 0.05) {
      "*"
    } else {
      NA
    }
  }))
  return(astk)
}

#
# Let's make sure that data are in the correct format
# values: numeric
# group: factor
#
colLabel = colnames(data)[1]
colnames(data) = c('value', 'group')
# Now plot a basic boxplot with ggplot2
#
bp <- ggplot(data, aes(x = group, y = as.numeric(value), fill = factor(group)))
bp <- bp + geom_boxplot(notch = F)
#
#
# define ylim, set colors and impose jitter
#
my_colors = c('red', 'green', 'yellow', 'blue')
bp <- bp + ylim(c(min(data$value),(max(data$value)+(max(data$value)/2))))
bp <- bp + labs(y = colLabel)
bp <- bp + scale_fill_manual(values=my_colors[1:length(levels(data$group))])
bp <- bp + geom_point(position = position_jitter(width = 0.25))
png(paste(outputFilePath, colLabel, '.png', sep=""))
print(bp)
dev.off()
#
#

#
#
# Let's complete this analysis by running an ANOVA significance test
# and adding annotation to the plot: which are the significan differences?
# What about the p-values? Let's start with the ANOVA
#
my_anova <- aov(value~group, data = data)
my_anova <- TukeyHSD(my_anova)
my_anova <- data.frame(cbind(my_anova$group, 
                             make_contrast_coord(length(levels(data$group)))))
my_anova$astks <- pval_to_asterisks(my_anova$p.adj)
#
#
# my_anova includes information about the ANOVA test on our dataset.
# Let's remove non-significant differences, as those won't be plotted.
# The filtered data frame is called tiny_anova. We also order the
# rows according to the distance between boxes (in the boxplot).
# Differences between boxes far away will be annotated at the top
#
tiny_anova <- my_anova[my_anova$p.adj < 0.05,]
tiny_anova <- tiny_anova[order(tiny_anova$len, decreasing = FALSE),]
#
#
# Let's set some parameters for the annotation. Change this depending to the specific boxplot
#
lowest.y <- max(data$value)
highest.y <- max(data$value)+( max(data$value)/3)
margin.y <-5
#
#
# Define the positions where the p-value segments will be drawn and then annotate the p-vals
#
actual.ys <- seq(lowest.y, highest.y, length.out = nrow(tiny_anova))
tiny_anova$ys <- actual.ys
bp_ask <- bp + annotate("segment", x = tiny_anova$str, y = tiny_anova$ys, 
                        xend = tiny_anova$end, yend = tiny_anova$ys, 
                        colour = "black", size = 0.95)
png(paste(outputFilePath, colLabel, 'PVALUE.png', sep=""))
print(bp_val <- bp_ask + annotate("text", x = tiny_anova$ave, y = (tiny_anova$ys + margin.y) , 
                            xend = tiny_anova$end, yend = tiny_anova$ys, 
                            label = paste ("p-val =", format(round(tiny_anova$p.adj, 4), nsmall = 4))))
dev.off()
png(paste(outputFilePath, colLabel, 'ASTERix.png', sep=""))
print(bp_ask <- bp_ask + annotate("text", x = tiny_anova$ave, y = (tiny_anova$ys + (margin.y/3)) , 
                            xend = tiny_anova$end, yend = tiny_anova$ys,
                            label = tiny_anova$astks, size = 10))
dev.off()
#
#
# Here are three plots:
# 1) boxplot without annotations
# 2) boxplot with p-val annotations (up to 4 decimal places)
# 3) boxplot with *** annotations
#


}