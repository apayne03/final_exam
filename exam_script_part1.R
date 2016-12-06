library(tidyverse)
library(pwr)

# 1 - determine df...
# u = 2 


# 2 - determine f2...

my.f2 <- .10 / (1 - .20)
print(my.f2) # 0.125


# 3 - calculate power...

pwr.f2.test(u=2, f2=0.125, power=.85)
N = 2 + 87 + 1
print(N) #90
