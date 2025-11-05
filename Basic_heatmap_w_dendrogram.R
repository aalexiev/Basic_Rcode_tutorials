## how to make a heatmap with dendrogram on the side

library(ggplot2)

## make dendrogram for side of heatmap
heatmap <- read.csv()
# heatmap csv should be an object or file with all the factors/variables relevant to your heatmap
# and all the significance and coefficients that are relevant, as columns

# filter otu table by pathways or OTU's that are in the significant associations file
otu_tab_heatfilt <- otu_table %>%
  rownames_to_column("OTUs") %>%
  dplyr::filter(OTUs %in% unique(heatmap$otu)) %>%
  column_to_rownames("OTUs")
# run clustering
dendro <- as.dendrogram(hclust(d = vegdist(x = otu_tab_heatfilt,
                                           method = "bray")))

library(ggdendro)
dendro_plot <- ggdendrogram(data = dendro, rotate = TRUE) + # I rotate so that this can go on the y-axis of my heatmap
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_blank()) +
  scale_y_reverse() # the y axis scale has to be reversed in order for it to line up right with the final heatmap
dendro_plot

# can save it and read it back in, in case you need to reference later
# ggsave("dendro_plot.png",
#        dendro_plot,
#        width = 2,
#        height = 20,
#        dpi = 300)

# can save this then add relevant metadata in excel for colored and cleaned up labels
# for example, in my case I added the type of pathway - xenobiotic, redox reaction, etc.
# write.table(labels(dendro), file = "dendro_labels.txt")


## make heatmap graph

# read in metadata for colors and categories
graphing_indics <- read.csv("input_files_pub/pwindic_colors.csv") %>%
  inner_join(heat2_comb, by = "pathway", multiple = "all")
# Order the levels according to their position in the cluster
graphing_indics$label <- factor(x = graphing_indics$pathway,
                                levels = labels(pw_dendro))
# for set up of the dendrogram with metadata variable (colors) on the y-axis
library(ggnewscale)

# whole heatmap plot
heat_plot <- ggplot(data = graphing_indics, aes(x = term, y = label)) +
  geom_tile(aes(fill = log(IRR)), colour = "white", linetype = 1) + # here I use IRR but more often people use lm coefficients or significance
  theme_classic() +
  scale_fill_gradient2(low = "tan", mid = "white", high = "darkgreen",
                       midpoint = 0, na.value = "white") +
  labs(y = "", x = "Model term") +
  geom_text(aes(label = p.value.adj.signif)) +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y.left = element_text(angle = 0),
        axis.title.y = element_blank()) +
  scale_x_discrete(labels = c("BaP Exposure", "AHR2 Morpholino", "Interaction (BaP and AHR2)")) +
  facet_grid(cols = vars(Generation))

# ggsave("heatmap_plot.png",
#        width = 8,
#        height = 14,
#        dpi = 300)


# make a scale bar of categories
# first iteration of this figure, I kept the label names to make sure they were indeed in the same order as the heatmap
library("RColorBrewer")
coul <- brewer.pal(10, "Set3")
Scale_cats <- ggplot(data = graphing_indics,
                     aes(x = 1, y = label,
                         fill = category)) +
  geom_tile() +
  coord_fixed() +
  labs(fill = "Category", x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_manual(values = coul)

# ggsave("heatmap_cats.png",
#        width = 8,
#        height = 14,
#        dpi = 300)

## then, in illustrator, I put heatmap plot and heatmap categories together next to each other

