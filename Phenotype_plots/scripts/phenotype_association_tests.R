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
library(stringr)
library(ggpubr)

## Load the data
phenotype_table = read.csv("../data/phenotypic_data.csv",
                           sep = ",", header = T, dec = ".")

genotype_table = read.csv("../data/genotypic_data.csv", 
                    header = TRUE, sep = ",", check.names = FALSE, 
                    stringsAsFactors = FALSE)

## Create a new table by merging the phenotype and genotype tables

# Can the following for loop be replaced by a more R-esque way of doing things?
# Using apply functions for example?

count = 0
# For each line in phenotype table
for (i in 1:nrow(phenotype_table)){
  # Get id name
  id = phenotype_table[i, 1]
  # Get genotype information in the genotype table from id name
  line_number = grep(id, genotype_table$Id)
  # If the id from the phenotype table was found in the genotype table...
  if (!length(line_number) == 0){
    # ...only get the information of interest from the genotype table
    line = genotype_table[line_number, 2]
    genotype = str_split(line, ":")
    genotype = genotype[[1]][1]
    # Concatenate information from the phenotype and genotype tables
    new_line = cbind(phenotype_table[i,], genotype)
    # If it is the first line just create a new vector
    if (count == 0){
      new_df = new_line
    # If it is not the first line add to already created rows
    } else {
      new_df = rbind(new_df, new_line)
    }
    count = count + 1
  }
} 

# Reorganize columns
final_df = cbind(new_df$IDs, new_df$genotype, new_df[, 2:18])

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
final_df[final_df$Genotype == "1/0", 2] = "0/1"

## Output boxplots

# Can this for loop can also be replaced or improved to be cleaner and more code
# efficient?
# In addition, is there a more efficient way of dealing with variable names
# containing variables? Or can you see a way of avoiding them all together? 

# For a sequence number corresponding to the number of phenotypic traits analyzed
for (i in 3:19){
  # Create a variable name containing the current iteration number of the for loop
  reg_name = paste("reg", i, sep = "")
  # Perform a linear regression between the three genotypes and a phenotypic trait
  reg = lm(final_df[[i]] ~ final_df$Genotype)
  # Give the variable name with the current iteration number to the linear regression object
  assign(reg_name, reg)
  # Create a variable name containing the current iteration number of the for loop
  plot_name = paste("p", i, sep = "")
  # Get the name of the phenotypic trait currently analyzed
  col_name = colnames(final_df[i])
  # Plot the genotypes against the data for the currently analyzed phenotypic trait
  plot = ggplot(data = final_df,
                aes_string(x = "Genotype",
                    y = col_name)) +
    geom_boxplot() +
    geom_abline(slope = coef(eval(as.name(reg_name)))[[2]], 
                intercept = coef(eval(as.name(reg_name)))[[1]],
                colour = "red") +
    ylab(colnames(final_df[i]))
  # Give the variable name with the current iteration number to the plot object
  assign(plot_name, plot)
}

# Organize all plots in a single figure
ggarrange(p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15,
          p16, p17, p18, p19, nrow = 5, ncol = 4)

# Last question: why is R such a horrible programming language?
