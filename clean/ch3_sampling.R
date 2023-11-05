# What is the probability that someone is a vampire given they tested positive

# If someone is a vampire the test will correctly detect this 95% of the time
pr_positive_vampire = 0.95

# If someone is not a vampire the test will incorrectly predict they are 1% of the time
pr_positive_mortal = 0.01

# Vampires are relatively rare making up only 0.1% of the population
pr_vampire = 0.001

# Sample 10000 people
N = 10000
num_vampires = N * pr_vampire
num_mortals = N * (1 - pr_vampire)

(num_positives = num_vampires * pr_positive_vampire + num_mortals * pr_positive_mortal)

# Probability someone is a vampire given a positive test
(pr_vampire_positive = num_vampires * pr_positive_vampire / num_positives)

# Equivalent to using probabilities instead of counts
(pr_positive = pr_vampire * pr_positive_vampire + (1 - pr_vampire) * pr_positive_mortal)
(pr_vampire * pr_positive_vampire/pr_positive)
