## Making a basic boxplot of alpha diversity
# Alex Alexiev

# the point of this code is to show how to make a basic R boxplot using ggplot
# It uses input data from https://github.com/aalexiev/FungalDevData/tree/master/02_PublishedAnalysis/02_hypothesisbasedanalsysis
# and the output was published as figure 2A in this publication:10.1371/journal.pone.0256328

# libraries
library(ggplot2)
library(RColorBrewer)

# input_all$data_loaded is a data frame that has all the metadata associated with my microbiome samples, 
# organized per sample with sample ID's in a column, and with alpha diversity per 
# sample previously calculated and added to the metadata data frame (see AlphaDiversity.Rmd)

## make color palette
# how many colors do I need?
nlevels(input_all$map_loaded$Life_Stage_Simplified)
# display hexcodes for the pallette I'm using but want to change slightly; n is above number
brewer.pal(n = 7, name = "Dark2")
# now set my manual pallette
samptype_cols <- c("#A6761D", 
                   "#3a6b94", "#66A61E", 
                   "#E7298A", "#7570B3", 
                   "#D95F02", "#1B9E77")

# make OTU richness figure
# note: in this boxplot, there are two fill parameters because the fill color had less values than the y-axis ticks
# biologically, this was because we had the "tadpole" stage but that stage actually has 7 mini-stages within it
OTUrich <- ggplot(MB_input, aes(x = Gosner_Stage, y = rich)) + # y is the per sample alpha diversity
  geom_boxplot(alpha = 0.6, outlier.shape = NA, # alpha changes opacity so points can stand out; if geom_point is added, outlier should be removed to prevent duplicate points
               aes(fill = Life_Stage_Simplified)) + xlab("") + # fill category will correspond with the color palette
  ylab("Fungal OTU Richness") + ggtitle("A") +
  geom_point(aes(fill = Life_Stage_Simplified), size = 3, shape = 21, 
             position = position_jitter(
               width = 0.25 # makes points visible while also still seeing box and whisker lines
             )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"), # changes the angle of the x axis text
        axis.text.y = element_text(colour = "black"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 30),
        legend.position = "none") +
  labs(fill = "Sample Type") +
  scale_fill_manual(values = samptype_cols) +
  scale_x_discrete(labels = c("Sediment", "Water", # this is to rename the legend labels to something human-readable
                              "Eggs", "Tadpole (20-22)",
                              "Tadpole (23-25)", "Tadpole (25-27)",
                              "Tadpole (27-29)", "Tadpole (29-31)",
                              "Tadpole (31-35)", "Tadpole (36-39)",
                              "Metamorph (40-46)", "Subadult", "Adult"))

# ggsave(paste0(work_dir,"Figs_Tables/AlphaDiv.png"),
#        OTUrich, dpi = 300,
#        width = 10, height = 7)

# The thing I like about this particular boxplot is that all the data points are
# visible but the box and whiskers, which show summary metrics, are also there.
# Note: ggplot displays median for the line in boxplots, as the default, so if you 
# notice your mean is different than that line, that's why.

