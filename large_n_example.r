  # An example to show log-linear modeling with large N

  # Load necessary libraries
library(MASS)  # Provides functions for statistical modeling
library(Hmisc) # Includes miscellaneous functions useful for data analysis
library(xtable) # Allows for creating LaTeX tables in R

  # Create a large matrix with specified data
big <- matrix(data = c(10000, 10150, 10450, 100500, 100475, 100450, 80125, 80150, 80175), nrow = 3)

  # Saturated Model

  # Fit a saturated log-linear model, including all interactions
sat.model <- loglm(~ 1 * 2, data = big)

  # Calculate and print deviance for saturated model
deviance(sat.model)

  # Perform an ANOVA test on the saturated model
anova(sat.model)

  # Extract coefficients of the saturated model
coefficients(sat.model)

  # Independence Model

  # Fit an independence log-linear model, assuming no interaction
ind.model <- loglm(~ 1 + 2, data = big)

  # Calculate and print deviance for independence model
deviance(ind.model)

  # Perform an ANOVA test on the independence model
anova(ind.model)

  # Extract coefficients of the independence model
coefficients(ind.model)

  # Compute fitted values for the independence model
fitted(ind.model)

  # Create LaTeX table for original 'big' matrix
bigtab <- xtable(big)
  
  # Print the table without floating numbers and other formatting options
print(bigtab, floating = FALSE, tabular.environment = "bmatrix", 
      hline.after = NULL, include.rownames = FALSE, include.colnames = FALSE)

  # Create LaTeX table for fitted values of independence model
bigfit <- xtable(fitted(ind.model))

  # Print the fitted values in LaTeX format with similar formatting options
print(bigfit, floating = FALSE, tabular.environment = "bmatrix", 
      hline.after = NULL, include.rownames = FALSE, include.colnames = FALSE)

  # Create a smaller matrix by dividing the big matrix by 100
small <- big / 100

  # Fit an independence log-linear model on the smaller matrix
ind.model2 <- loglm(~ 1 + 2, data = small)

  # Calculate and print deviance for the smaller independence model
deviance(ind.model2)

  # Perform an ANOVA test on the smaller independence model
anova(ind.model2)

  # Create LaTeX table for the 'small' matrix
smalltab <- xtable(small)

  # Print the small table with similar formatting options
print(smalltab, floating = FALSE, tabular.environment = "bmatrix", 
      hline.after = NULL, include.rownames = FALSE, include.colnames = FALSE)

  # Create LaTeX table for fitted values of the smaller independence model
smallfit <- xtable(fitted(ind.model2))

  # Print the fitted values for the small model with similar formatting
print(smallfit, floating = FALSE, tabular.environment = "bmatrix", 
      hline.after = NULL, include.rownames = FALSE, include.colnames = FALSE)