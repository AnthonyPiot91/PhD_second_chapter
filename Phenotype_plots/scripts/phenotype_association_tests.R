################################################################################
# For one genetic variant, we want to plot phenotypic measurments for 17 wood
# property traits in function of three possible genotypes, homozygous for the
# reference allele, heterozygous, and homozygous for the alternative
# allele (a.k.a the genetic variant).
# For this, we will merge the phenotypic and genotypic data.
# Then, perform linear regression models.
# And finally, plot the results.
# Such a task has been accomplished by a R newbie in the following code.
# Can you see coding improvements to make in order to perform the required tasks 
# in a proper R manner?
################################################################################

library(ggplot2)
library(ggpubr)

## Load the data
# It is recommended to use the <- symbol for assignment in R, because it truly distinguishes
#  assignment from arguments to function calls. I will only make the change once in this
#  script so as not to make too many unnecessary changes
phenotype_table <- read.csv("../data/phenotypic_data.csv",
			    # preferably use full version (TRUE and FALSE) for logical values in scripts
			    # T and F are OK for interactive use
			    sep = ",", header = TRUE, dec = ".")

genotype_table = read.csv("../data/genotypic_data.csv", 
                    header = TRUE, sep = ",", check.names = FALSE, 
                    stringsAsFactors = FALSE)

## Create a new table by merging the phenotype and genotype tables

# Can the following for loop be replaced by a more R-esque way of doing things?
# Using apply functions for example?

# The match function can be used here to simplify things
# I prefer to use column names (strings) instead of column numbers (integers) when subsetting
#  because it makes it much clearer when reading the code
phenotype_table$genotype <- genotype_table[match(phenotype_table$IDs, genotype_table$Id), "Genotype"]

# Then we need to remove the NA values (those for which no match in genotype_table was found)
phenotype_table <- phenotype_table[!is.na(phenotype_table$genotype), ]

# Replacing the contents of then Genotype column with the genotype part only
# See more explanations below for the use of sapply; here I am using `[` as a function
#  to extract the first element of each vector returned by strsplit
phenotype_table$genotype <- sapply(strsplit(phenotype_table$genotype, ":"), `[`, 1)

# Note that I use the strsplit function of the base package instead of str_split from stringr
# I know that functions in stringr are often more computationally efficient for manipulating
# strings. However, when computational efficiency is not a concern (like here), I like to
# avoid relying on external packages. This is just a personal preference.

# I would also use the "phenotype_table" object throughout the analysis; this may or may not be
# desirable if you need to access the original data.frame later in the analysis. However,
# this does not seem to be the case here, therefore doing so reduces the number of distinct
# objects created
# Here, I will change the name back to final_df so I don't have to change all references to it below
final_df <- phenotype_table

# Reorganize columns
# You don't need to use cbind when reordering columns; you can simply use the order of column indices
# Here is one case where I recommend using integer column indices; it's much easier than writing out names
final_df <- final_df[, c(1, 19, 2:18)] 

# Change column names
colnames(final_df) = c("ids",
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
                       "Total.lignin" )

# Remove unknown genotypes and change genotype names
final_df = final_df[final_df$Genotype != "./.", ]
final_df$Genotype = as.character(final_df$Genotype)
final_df[final_df$Genotype == "1/0", "Genotype"] = "0/1"

## Output boxplots

# Can this for loop can also be replaced or improved to be cleaner and more code
# efficient?
# In addition, is there a more efficient way of dealing with variable names
# containing variables? Or can you see a way of avoiding them all together? 

# The lapply function can be used to iterate over all values in a list
# Its first argument is a list:
#  Here we can pass a data.frame because a data.frame is actually a list
# Its second argument is a function that will be applied to each element of the list:
#  The function can be defined beforehand and its name written out.
#  Here, I will instead use an anonymous function defined directly inside the function call
#  Additional arguments to the function (here, the genotype data)
#  can be passed as additional arguments to lapply
regressions <- lapply(final_df[, 3:19],
		      function(x, genotypes) lm(x ~ genotypes),
		      genotypes = final_df$Genotype)

# the value returned is a list with one element per element in the original list
# Here, regressions is therefore a list of regression results, with names matching
# those of the original columns
# Assigning such intermediate results inside a list is a better approach than
# using "assign" as below because it stores the results together and does not
# clutter the global environment; all regressions can be accessed from within
# a single list; it also avoids having to access the objects using "eval"

# sapply (used above) is a version of lapply that simplifies the output into
# a vector of the appropriate type (when applicable)

# I will use a slightly different approach (for loop) for generating the plots
# However, I will still assign the results into a list as lapply would have done
plots <- list()

# We iterate over the phenotype names, which can be used to access data columns,
#  regression results, and to assign elements in the "plots" list
for(i in names(final_df)[3:19]) {
	plots[[i]] <- ggplot(data = final_df,
			     mapping = aes_string(x = "Genotype", y = i)) +
			geom_boxplot() +
			geom_abline(slope = coef(regressions[[i]])[[2]],
				    intercept = coef(regressions[[i]])[[1]],
				    colour = "red") +
			ylab(i)
}

# Preparing a list of arguments to ggarrange from the plots list
# We only need to add arguments for nrow and ncol
plots$nrow <- 5
plots$ncol <- 4
do.call("ggarrange", plots)

# Last question: why is R such a horrible programming language?
# It's not, but I guess the reason why it is frustrating is that it was developed
# for data analysts/statisticians and not for programmers. But we can talk
# more about that later on!
