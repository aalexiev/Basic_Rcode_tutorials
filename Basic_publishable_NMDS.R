## Making a basic NMDS plot of beta diversity
# Alex Alexiev; adapted from coding sessions with Hannah Holland-Moritz (https://github.com/hhollandmoritz)

# the point of this code is to show how to make a basic NMDS plot using ggplot
# It uses input data from https://github.com/aalexiev/FungalDevData/tree/master/02_PublishedAnalysis/02_hypothesisbasedanalsysis
# and the output was published as figure 2D in this publication:10.1371/journal.pone.0256328

# libraries
library(vegan)
library(ggplot2)
library(dplyr)

# input_all$data_loaded is a data frame that has all the metadata associated with my microbiome samples, 
# organized per sample with sample ID's in a column, and with alpha diversity per 
# sample previously calculated and added to the metadata data frame (see BetaDiversity.Rmd)


# make NMDS plot with all samples, toad vs. env
# create distance matrix
md_transformed <- t(sqrt(input_all$data_loaded))
dm <- vegdist(md_transformed, method = "bray")

# run NMDS
md.nmds <- metaMDS(dm, k = 3, trymax = 1000) #solution reached, stress=0.195

#prepare NMDS points and values for graphing
md.nmds.points <- md.nmds$points %>% # take the NMDS points
  data.frame() %>% # make a data frame
  rownames_to_column(var = "mysamples") %>%
  mutate(SampleID = mysamples) %>%
  column_to_rownames(var = "mysamples")
meta <- input_all$map_loaded %>%
  rownames_to_column("SampleID")
md.nmds.metadata <- inner_join(x = md.nmds.points, y = meta, 
                               by = "SampleID") %>%
  group_by(Type) %>%
  ungroup() %>%
  dplyr::select(SampleID, MDS1, MDS2, everything()) # select these columns only; sometimes you have to specify this is dplyr's select function if you loaded other packages that also have a select() function
md.nmds.metadata.unq <- md.nmds.metadata %>%
  dplyr::select(Type) %>%
  unique()
md.nmds.metadata.unq$Type <- 
  factor(md.nmds.metadata.unq$Type, 
         levels = c("Toad", "Environment"))

# get stress value
stress.md = paste("stress =", round(md.nmds$stress, digits = 4))
# this will be printed onto the graph
# stress should be as close to 0 as possible, which is a measure of how the data fits the ordination
# however, stress at 0 is too perfect of a fit and shouldn't happen with biological data...which might
# mean the data set is too small

# get NMDS hulls
group.chulls <- plyr::ddply(md.nmds.metadata, "Type", 
                            function(df) df[chull(df$MDS1, df$MDS2), ])

# make color pallete
samptype_cols_TE <- c("#4E72E2", "#E2A14E")

# Plot NMDS
nmds.plot <- ggplot(md.nmds.metadata, aes(x = MDS1, y = MDS2, color = Type)) +
  geom_polygon(data = group.chulls, 
               aes(fill = Type), 
               alpha = 0.15, linetype = 0) +
  geom_point(size = 3) +
  annotate("text", x = Inf, y = Inf, label = stress.md, hjust = 1, vjust = 1) +
  ggtitle("C") + 
  labs(colour = "Sample Type") +
  scale_colour_manual(values = samptype_cols_TE) +
  scale_fill_manual(values = samptype_cols_TE) +
  guides(fill = F, color = guide_legend(order = 1, 
                                        override.aes = list(shape = 15, 
                                                            size = 6))) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 30))
nmds.plot

# ggsave(paste0(work_dir,"Figs_Tables/nmds.png"),
#        nmds.plot, dpi = 300,
#        width = 10, height = 7)

