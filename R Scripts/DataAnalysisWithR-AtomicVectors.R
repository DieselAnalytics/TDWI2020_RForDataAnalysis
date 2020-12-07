# Atomic vector containing the numbers 1 to 5
int_vector <- seq.int(from=1,to=5, by = 1)
int_vector


# Converting what appears to be a integer vector to 
# an actual integer vector
typeof(int_vector)
int_vector <- as.integer(int_vector)
typeof(int_vector)


# Below is a simple example where we take advantage of 
# vectorization to multiple all of the elements in the 
# int_vector vector by 2. Vectorization means that R is 
# applying the multiplication below on all of the elements 
# at once. There is not looping structure needed.
int_vector * 2


# There are many things you can do with numeric based vectors. 
# For instance, we can easily sum up the elements in the vector 
# using the sum() function as illustrated below:
sum(int_vector)


# Another type of atomic vector is a "logical vector" which holds 
# "Boolean" values. Below is a "logical vector" that returns "TRUE" for 
# all elements in the "int_vector" that are even and "FALSE" otherwise. 
# It does so using the modulus "%%" operator to get the remainder after 
# dividing the given element in the vector by 2. If it is 0 then that 
# means the element is even if it is not 0 then that means the element 
# is odd. The results are printed below:
logical_vector <- int_vector %% 2 == 0
logical_vector


# Underneath the hood the TRUE's in a "logical vector" are stored 
# as a value of one and the FALSE's are stored as a value of zero. You 
# can take advantage of this property by summing up the element in the 
# vector to quickly get the number of TRUE's in the "logical vector" as 
# illustrated below:
sum(logical_vector)


# The last type of vector we will discuss is the character vector. It is 
# used to store data that are in a string format. Below is a character 
# vector that stores the last names of the previous 5 presidents:
character_vector <- c("Biden", "Trump", "Obama", "Bush", "Clinton")
character_vector


#  The type of operation you can perform on a vector is dependent on the 
# type of vector it is. One of the operations you can perform on a 
# "character vector" is sorting. Below we show how to use the sort() 
# function to sort a "character vector":
sort(character_vector)
sort(character_vector, decreasing = TRUE)


# Operations between two vectors happens in parallel. For example, below we 
# have two vectors, vector "int_vector_a" and vector "int_vector_b". Both 
# vectors are numeric vectors and have the same length. Vectorization allows 
# us to add both of these vectors with having to implement any looping logic. 
# The addition of the vectors will be position based so elements in vector 
# "int_vector_a" will be added to the element in vector "int_vector_b" that 
# is in the same position. The result will be a vector of the same that 
# contains the values of the positional summations.
int_vector_a <- seq.int(from=1,to=5, by = 1)
int_vector_b <- seq.int(from=6,to=10, by = 1)
int_vector_a + int_vector_b
