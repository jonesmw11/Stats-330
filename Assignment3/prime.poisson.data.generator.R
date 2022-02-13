# R code for Finding prime numbers
# initialize number n
n=50000L
# arranging sequence
x = seq(1L, n)

# Creating an empty vector to store the numbers
prime_numbers = integer()

for (i in seq(2L, n)) {
  if (any(x == i)) {
    # prime numbers gets stored in a sequence order
    prime_numbers = c(prime_numbers, i)
    x = c(x[(x %% i) != 0L], i)
  }
}

# printing the series
cat("The first ten prime numbers:", 
    head(prime_numbers, 10L))

# length(prime_numbers)

# Poisson data --------------------------
# Creating Random intervals 
num_bin = 1000L
set.seed(123L) # so we have the same data set
xbin = sample(seq(2L, n-1L), num_bin-1L)
xbin = sort(xbin)
xbin = c(1L, xbin, n)

# head(xbin)

# Creating response for poisson 
y = integer(num_bin)

# Creating explanatory variables 
size = integer(num_bin)
variance = integer(num_bin)
center = integer(num_bin)
even = logical(num_bin)
cramer = logical(num_bin)
ratio = integer(num_bin)
PNT = double(num_bin)

for (i in 1L:num_bin){
  
  prime_bin = prime_numbers[which(prime_numbers > xbin[i] & prime_numbers <= xbin[i+1L])]
  
  y[i] = length(prime_bin)
  
  size[i] = xbin[i+1L] - xbin[i] 
  center[i] = median(xbin[i]:xbin[i+1L])
  variance[i] = var(xbin[i]:xbin[i+1L])
  even[i] = as.logical(size[i] %% 2L)
  cramer[i] = ((xbin[i+1] - xbin[i]) >= (log(xbin[i]))^2L)
  ratio[i] =  xbin[i+1L] / xbin[i] 
  noise = abs(rnorm(1L))
  PNT[i] = xbin[i+1L] / log(xbin[i+1L] + noise) -  xbin[i] / log(xbin[i] + noise)
  
}

# Data for poisson 
prime.poisson.df = data.frame(y, size, center,
                              variance, even, cramer, 
                              ratio, PNT)

# head(prime.poisson.df)

write.table(prime.poisson.df, file = "prime.poisson.txt", row.names = FALSE)

# 
set.seed(123L)
noise = abs(rnorm(1L))
new_xbin = c(100L, 500L)

new_prime_bin = prime_numbers[which(prime_numbers > new_xbin[1] & prime_numbers <= new_xbin[i+1L])]

new_data_df = data.frame(
  size = new_xbin[2L] - new_xbin[1L], 
  center = median(new_xbin[1L]:new_xbin[2L]), 
  even = as.logical((new_xbin[2L] - new_xbin[1L]) %% 2L),
  cramer = ((new_xbin[2L] - new_xbin[1L]) >= (log(new_xbin[1L]))^2L),
  ratio = new_xbin[2L] / new_xbin[1L],
  PNT = new_xbin[2L] / log(new_xbin[2L] + noise) - new_xbin[1L] / log(new_xbin[1L] + noise))


write.table(new_data_df, file = "new.poisson.df.txt", row.names = FALSE)

