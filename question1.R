library(gtools)
output <- data.frame(permutations(n=6, r = 8, repeats.allowed = T))
names(output) <- letters[1:8]
output <- output %>%
 mutate(sum = a+b+c+d+e+f+g+h,
        product = a*b*c*d*e*f*g*h)
sum_24 <- output %>% filter(sum == 24)
mean(sum_24$product)
sd(sum_24$product)
