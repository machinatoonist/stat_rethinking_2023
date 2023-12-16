# Set seed for reproducibility
set.seed(123)

# Step 1: Create a simulated population
population_size <- 1000
cancer_status <- rbinom(population_size, 1, 0.1) # 10% chance of having cancer

# Step 2: Randomize exposure to a substance
substance_exposure <- rbinom(population_size, 1, 0.5) # 50% chance of exposure

# Step 3: Analyze the subset with cancer and exposure
subset_with_cancer_and_exposure <- which(cancer_status == 1 & substance_exposure == 1)
length_of_subset <- length(subset_with_cancer_and_exposure)

# Output the size of the subset
length_of_subset

subset_with_cancer_and_no_exposure <- which(cancer_status == 1 & substance_exposure == 0)
length_of_null <- length(subset_with_cancer_and_no_exposure)
length_of_null
