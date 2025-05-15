### Finding relative abundance from an OTU table

library(dplyr)
library(tibble)

# assuming you have an OTU table file, OTU_tab,  that has OTU's as rows and sample ID's as columns
OTU_tab <- read.delim("seqtab_wTax_mctoolsr_PR2018.txt", header = T) %>% # OTU table with PR18 16S data
    column_to_rownames("ESV_ID")
# if you need to, make taxonomy the rownames
rownames(OTU_tab) <- c()
OTU_tab <- OTU_tab %>%
    column_to_rownames("taxonomy")

# calculate relative abundances for OTU table and remove singletons
str(OTU_tab)
OTU_tab[] <- lapply(OTU_tab, function(x) as.numeric(x)) # sometimes have to make all the numbers numeric if didn't read in right
OTU_tab$sums <- rowSums(OTU_tab) # add rowsums to end of column
OTU_tab <- OTU_tab %>% # filter out sums that are zero or one (no reads or singletons)
    dplyr::filter(sums > 1)
OTU_tab_rel <- OTU_tab[1:480]/OTU_tab$sums # calculate relative abundance
OTU_tab_filt <- as.data.frame(t(OTU_tab_rel)) %>% # need to be a data frame after transposing (ESV's as columns, sample ID's as rows)
    rownames_to_column("Sample_ID") %>% # make sample ID column for rownames
    dplyr::filter(Sample_ID %in% as.list(meta_tab$SampleID)) %>% # filter Sample ID's in OTU table to match those in the metadata table
    column_to_rownames("Sample_ID") # return set rownames to data frame
