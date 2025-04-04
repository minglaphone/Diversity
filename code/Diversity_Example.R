#Diversity_Example.R

#A script to learn about diversity indices

#a function
diversity <- function(x, index = "shannon", base = exp(1)) {
  # Calculate the diversity index based on the specified method
  if (index == "shannon") {
    p <- x / sum(x)
    -sum(p * log(p, base), na.rm = TRUE)
  } else if (index == "simpson") {
    p <- x / sum(x)
    1 - sum(p^2, na.rm = TRUE)
  } else {
    stop("Unknown index")
  }
}

#Example data
#A data frame with species counts

pop <- data.frame(
  species = c("Red", "Purple", "Orange"),
  count = c(3, 2, 1)
)
#Calculate the Shannon diversity index
shannon_index <- diversity(pop$count, index = "shannon")
#Calculate the Simpson diversity index
simpson_index <- diversity(pop$count, index = "simpson")
#Print the results
cat("Shannon Diversity Index:", shannon_index, "\n")
cat("Simpson Diversity Index:", simpson_index, "\n")
#Plot the data
library(ggplot2)
ggplot(pop, aes(x = species, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Species Counts", x = "Species", y = "Count") +
  theme_minimal()
#The code calculates the Shannon and Simpson diversity indices for a given set of species counts and visualizes the data using a bar plot.
#The Shannon index is a measure of the uncertainty in predicting the species of a randomly selected individual from a sample.
#The Simpson index is a measure of the probability that two individuals randomly selected from a sample will belong to the same species.

pops<-data.frame(pop,pop2,pop3,pop4)
library(purrr)
library(dplyr)
# Calculate diversity indices for each population
diversity_indices <- pops %>%
  select(-species) %>%
  map(~ c(shannon = diversity(.x, index = "shannon"),
          simpson = diversity(.x, index = "simpson")))
# Convert the list to a data frame
diversity_df <- bind_rows(diversity_indices, .id = "population")
# Print the diversity indices
print(diversity_df)
#The code calculates the Shannon and Simpson diversity indices for each population in the data frame and stores the results in a new data frame.
#The diversity indices are calculated using the `map` function from the `purrr` package, which applies the `diversity` function to each column of the data frame (excluding the species column).
#The results are then combined into a new data frame using the `bind_rows` function from the `dplyr` package.
#The resulting data frame contains the diversity indices for each population, which can be used to compare the diversity of different populations.
#The code uses the `purrr` and `dplyr` packages to calculate diversity indices for each population in the data frame and stores the results in a new data frame.