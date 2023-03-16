########################################################################################################################################################################
# Copyright [2022] <Meet Kothari> [legal/copyright]                                                                                                                    #
#                                                                                                                                                                      #      
# Assignment 1, Part 1                                                                                                                                                 #      
#                                                                                                                                                                      #      
# “Write a program to create an ordered collection A of functions of a real                                                                                            #  
# number. At least one function should be built-in and at least one should be userdefined; try using the sine, cosine, and cubing functions. Fill another collection B #
# with the inverse of each function in A. Implement function composition as in                                                                                         #
# Functional Composition. Finally, demonstrate that the result of applying the                                                                                         #
# value. (Within the limits of computational accuracy).”                                                                                                               #
#                                                                                                                                                                      #
########################################################################################################################################################################

import math # as per the assignment prompt, I will be using some of the built-in functions provided by the Python math library.

# as per the assignment prompt, I need to implement function composition. In Python, this is doable in one line.

def function_composition(f, g): 
    return lambda x : f(g(x))

# At least one function should be built-in and at least one should be user defined:
# I defined two of my own functions...
    
def find_cube(n):
    return (n * n *n)

def inverse_find_cube(n):
    return (n **(1/3))

def meet_cipher(n): 
    float(n) + 3.0
    n / 3.0
    n * 5.0
    return float(n)

def inverse_meet_cipher(n):
    float(n) - 3.0
    n * 3.0
    n / 5.0
    return float(n)

# Write a program to create an ordered collection A  of functions of a real number. 
# At least one function should be built-in and at least one should be userdefined; try using the sine, cosine, and cubing functions.

collectionA = [math.sinh, math.cosh, find_cube, meet_cipher]

# Fill another collection B with the inverse of each function in A. 

collectionB = [math.asinh, math.acosh, inverse_find_cube, inverse_meet_cipher]

# Test user input by throwing an error if the input is anything besides a number. 
#
#               |
#               |
#               v
#
# Implement function composition as in Functional Composition,
# I did this by using the native function zip() to take the collections and aggregate them in a tuple.
#        
#               |
#               |
#               v
# 
# Demonstrate that the result of applying the composition of 
# each function in A and its inverse in B to a value, is the original value.

try:
    n = float(input("Please enter a number to test: "))
    collection = zip(collectionA, collectionB) 
    print([function_composition(func, inversefunc)(n) for (func, inversefunc) in collection])

except ValueError:
    print("\nInvalid input, please enter a numerical value. \n")
