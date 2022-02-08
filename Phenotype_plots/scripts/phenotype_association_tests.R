################################################################################
# R script to merge phenotypic and genotypic data, perform linear regression
# models and plot the results.
################################################################################

library(ggplot2)
library(ggpubr)

## Load the data

phenotype_table <- read.csv("../data/phenotypic_data.csv",
                            sep = ",", header = TRUE, dec = ".")

genotype_table <- read.csv("../data/genotypic_data.csv",
                    header = TRUE, sep = ",", check.names = FALSE,
                    stringsAsFactors = FALSE)

## Create a new table by merging the phenotype and genotype tables

# The match function is used
# It is better to use column names (strings) instead of column numbers
# (integers) when subsetting for improved code readibility
phenotype_table$genotype <- genotype_table[match(phenotype_table$IDs,
                                                 genotype_table$Id), "Genotype"]

# Then we need to remove the NA values (those for which no match in
# genotype_table was found)
phenotype_table <- phenotype_table[!is.na(phenotype_table$genotype), ]

# Replacing the contents of the genotype column with the relevant genotype
# information only
# sapply is a version of lapply that simplifies the output into a vector of the
# appropriate type (when applicable)
# Here I am using `[` as a
# function to extract the first element of each vector returned by strsplit
phenotype_table$genotype <- sapply(strsplit(phenotype_table$genotype, ":"),
                                   `[`, 1)

# When computational efficiency is not a concern (like here), avoid relying on
# external packages

# It it prefereable to use the same object name throughout the analysis;
# this may or may not be desirable if the original data.frame need to be
# accessed later in the analysis.
# However, this is not the case here, therefore doing so reduces the number of
# distinct objects created

# Reorganize columns
# No need to use cbind when reordering columns; you can simply use the order of
# column indices.
# Here is one case where  using integer column indices is recommended;
# it's much easier than writing out names
phenotype_table <- phenotype_table[, c(1, 19, 2:18)]

# Change column names
colnames(phenotype_table) <- c("ids",
                       "Genotype",
                       "Arabinose",
                       "Galactose",
                       "Glucose",
                       "Xylose",
                       "Mannose",
                       "Insoluble.lignin",
                       "Soluble.lignin",
                       "Holocellulose",
                       "Alpha.cellulose",
                       "Hemicellulose",
                       "S.L.monomers",
                       "Fiber.length.LW",
                       "MFA.FGRFB",
                       "MFA.FGRFP",
                       "X.crystallinity",
                       "Average.WD",
                       "Total.lignin")

# Remove unknown genotypes and change genotype names
phenotype_table <- phenotype_table[phenotype_table$Genotype != "./.", ]
phenotype_table$Genotype <- as.character(phenotype_table$Genotype)
phenotype_table[phenotype_table$Genotype == "1/0", "Genotype"] <- "0/1"

## Output boxplots

# The lapply function is used to iterate over all values in a list
# Its first argument is a list:
#  Here we can pass a data.frame because a data.frame is actually a list
# Its second argument is a function that will be applied to each element of the
# list:
#  The function can be defined beforehand and its name written out.
#  Here, an anonymous function is used instead, and defined directly inside the
#  function call
#  Additional arguments to the function (here, the genotype data) are passed as
#  additional arguments to lapply
regressions <- lapply(phenotype_table[, 3:19],
                      function(x, genotypes) lm(x ~ genotypes),
                      genotypes = phenotype_table$Genotype)

# The value returned is a list with one element per element in the original list
# Here, regressions is therefore a list of regression results, with names
# matching those of the original columns
# Assigning such intermediate results inside a list is a better approach than
# using "assign" because it stores the results together and does not clutter the
# global environment; all regressions can be accessed from within a single list;
# it also avoids having to access the objects using "eval"

# Here a for loop is used to generate the plot
# However, the results will still be assigned into a list as lapply would have
# done
plots <- list()

# We iterate over the phenotype names, which can be used to access data columns,
# regression results, and to assign elements in the "plots" list
for (i in names(phenotype_table)[3:19]) {
  plots[[i]] <- ggplot(data = phenotype_table,
                       mapping = aes_string(x = "Genotype", y = i)) +
    geom_boxplot() +
    geom_abline(slope = coef(regressions[[i]])[[2]],
                intercept = coef(regressions[[i]])[[1]],
                colour = "red") +
    ylab(i)
}

# Arrange the plots in one figure
ggarrange(plotlist = plots, nrow = 5, ncol = 4)

# Alternative way of using ggarrange
# Preparing a list of arguments to ggarrange from the plots list
# We only need to add arguments for nrow and ncol
plots$nrow <- 5
plots$ncol <- 4
do.call("ggarrange", plots)
