library(readxl)
source("styleguide.R")
### Graphic 1
#### CLEAN 
# Read in data
data = read_excel("EasyDataForSeth.xlsx")
colnames(rawdf)[1] <- "House"

# Convert to ggplot readable
data[,-1]
data.long = t(data[,-1]) 
d <- data.long
names <- rownames(d)
rownames(d) <- NULL
data <- as.data.frame(cbind(names,d))

# Factors to numerics
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
cols.num <- c("V2","V3")
data[cols.num] <- sapply(data[cols.num],as.numeric.factor)
sapply(data, class)

# Order factors 
data$names <- factor(data$names, levels = data$names)

#### PLOT 

# Colors
color_scheme <- c("#485976",
                  "#88c2d2",
                  "#eea241",
                  "#fc3839",
                  "#8c1717",
                  "#5e5e5e",
                  "#485976",
                  "#88c2d2",
                  "#eea241",
                  "#fc3839",
                  "#8c1717",
                  "#5e5e5e",
                  "#485976",
                  "#88c2d2")

data$V3[2] <- NA

p <- ggplot(data = data) +
  geom_bar( aes(x=names, y=V2), stat="identity", fill=color_scheme) +
  geom_errorbar(aes(x=names, ymin=V2-V3, ymax=V2+V3), width=0.4, colour="black", alpha=0.9, size=.3) + 
  scale_y_continuous(breaks=seq(0,1,.1)) +
  labs(title="Thank You Frequency", subtitle = "N = 525",caption="Error Bars represent 95% CI's, Annenberg error bar omitted due to small sample size.") +
  xlab("House") +
  ylab("Frequency") +
  theme_hodp() +
  theme(axis.text.x = element_text(angle = 0))
p

# Add logo
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))



### Graphic 2 

# Clean
# Read data and clean 
data = read.csv("graphic2.csv", header = F)
colnames(data) <- c("names", "n", "count", "p", "se2") 
data$names <- factor(data$names, levels = data$names)


### Plot

# color scheme 
primary <- c('#EE3838', '#FA9E1C', '#78C4D4', '#4B5973', '#E2DDDB')

# GGplot object
p <- ggplot(data = data) +
  geom_bar( aes(x=names, y=p), stat="identity", fill=primary) +
  geom_errorbar(aes(x=names, ymin=p-se2, ymax=p+se2), width=0.4, colour="black", alpha=0.9, size=.3) + 
  scale_y_continuous(breaks=seq(0,1,.1)) +
  labs(title="Thank You Frequency by House Neighborhood", subtitle = "N = 525",caption="\nError Bars represent 95% CI's, Annenberg error bar omitted due to small sample size.") +
  xlab("House Neighborhood") +
  ylab("Frequency") +
  theme_hodp() +
  theme(axis.text.x = element_text(angle = 0),
        plot.caption = element_text(hjust = 1))
p

# Add logo
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))


# Alternative subtitle
# subtitle = "N = 525",caption="\nError Bars represent 95% CI's,\nAnnenberg error bar omitted  \n due to small sample size.   ") +
