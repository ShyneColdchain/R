library(ggplot2)

######################################################################
#########Compare Cohens vs P with various sampling methods############
######################################################################
####################Shoutout to Donald Trump##########################
######################################################################

# set up cohens d function
cohens <- function(group1, group2){
  difference = mean(group1) - mean(group2)
  
  n1 = length(group1)
  n2 = length(group2)
  
  var1 = var(group1)
  var2 = var(group2)
  
  pooled_var = (n1 * var1 + n2 * var2) / (n1 + n2)
  d = abs(difference / sqrt(pooled_var))  
}

# sample sizes of 50 
iters = 50
sample_size = 50

# rnorm = random number on normal distribution with mean = 0.5
data = data.frame(cond = factor(rep(c('A', 'B'), each=10000)), 
                  rating = c(rnorm(10000), rnorm(10000, mean=0.5)))

# runif = uniform distribution 

# random package = true random number generator 

# view the distributions
dist = ggplot(data, aes(x=rating, fill=cond)) + geom_density(alpha=.3)
dist = dist + ggtitle("Data Distribution")
dist

true_d = cohens(data$rating[data$cond == 'A'], data$rating[data$cond == 'B'])
print(true_d)

# hella tiny p value (uber significant) because sample sizes are huge (p hacking alert!)
true_p = t.test(data$rating[data$cond == 'A'], data$rating[data$cond == 'B'])$p.value
print(true_p)

p_values = matrix(,nrow=iters)
cohens_d = matrix(,nrow=iters)

for (i in 1:iters) {
  sample_a = sample(data$rating[data$cond == 'A'], size = sample_size, replace=F)
  sample_b = sample(data$rating[data$cond == 'B'], size = sample_size, replace=F)
  
  d = cohens(sample_a, sample_b)
  cohens_d[i] = d - true_d
  
  p = t.test(sample_a, sample_b)$p.value
  p_values[i] = p - true_p
}

print('p values')
print(c(mean(p_values), sd(p_values)))
print('cohens d')
print(c(mean(cohens_d), sd(cohens_d)))

# Distribution of p and cohens values
p_types <- rep("p_values", length(p_values))
all_p <- data.frame(value=p_values, type=p_types)

c_types <- rep("cohens_d", length(cohens_d))
all_c <- data.frame(value=cohens_d, type=c_types)

df <- rbind(all_p, all_c)

# Plot distributions of p and cohen
base_plot <- ggplot(df, aes(x=value)) + ggtitle("Distribution of p-values and Cohens-d")
non_scaled <- base_plot + geom_density(aes(fill=factor(type)), alpha=0.3)
non_scaled

# and scale each distribution 
is_scaled <- base_plot + geom_density(aes(fill=factor(type), y=..scaled..), alpha=0.3)
is_scaled