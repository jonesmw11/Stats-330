n=5000L
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


# Creating Random intervals 
num_bin = 250L
num_rep = 50L
num_group = 10L
size = (1L:num_group) * num_rep
set.seed(123L) # so we have the same data set

xbin = sample(seq(2L, n-1L), num_bin)
x_begin = rep(xbin, times = num_group)
size = rep(size, each = num_bin)
x_end = xbin + size
index = x_end <= n

x_begin = x_begin[index]
x_end = x_end[index]
size = size[index]
# head(xbin)
num_bin = length(x_begin)
# Creating response for logistic 
y.binomial = integer(num_bin)
n.binomial = integer(num_bin)

# Creating explanatory variables 
center = integer(num_bin)
cramer = logical(num_bin)
ratio = integer(num_bin)
PNT = double(num_bin)

for (i in 1L:num_bin){
  
  prime_bin = prime_numbers[which(prime_numbers >= x_begin[i] & prime_numbers <= x_end[i])]
  
  center[i] = median(x_begin[i]:x_end[i])
  cramer[i] = (size[i] >= (log(x_begin[i]))^2L)
  ratio[i] =  x_end[i] / x_begin[i] 
  noise = abs(rnorm(1L))
  PNT[i] = x_end[i] / log(x_end[i] + noise) -  x_begin[i] / log(x_begin[i] + noise)
  
  y.binomial[i] = length(prime_bin)
  n.binomial[i] = size[i] + 1L
  
}

# Data for logistic 
prime.logistic.df = data.frame(y.binomial, n.binomial, center,
                               cramer, ratio, PNT)

# head(prime.logistic.df)
set.seed(123L)
noise = abs(rnorm(1L))
new_xbin = c(10L, 100L)

new_prime_bin = prime_numbers[which(prime_numbers >= new_xbin[1] & prime_numbers <= new_xbin[2L])]

new_data_df = data.frame(
  center = median(new_xbin[1L]:new_xbin[2L]), 
  cramer = ((new_xbin[2L] - new_xbin[1L]) >= (log(new_xbin[1L]))^2L),
  ratio = new_xbin[2L] / new_xbin[1L],
  PNT = new_xbin[2L] / log(new_xbin[2L] + noise) - new_xbin[1L] / log(new_xbin[1L] + noise))


write.table(prime.logistic.df, file = "prime.logistic.txt", row.names = FALSE)
write.table(new_data_df, file = "new.logistic.df.txt", row.names = FALSE)



