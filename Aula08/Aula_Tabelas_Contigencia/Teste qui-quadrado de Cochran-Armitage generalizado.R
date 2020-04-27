library(coin)
library(rcompanion)
# The chisq_test function in the coin package can be used to conduct a test of 
# association for a contingency table with one ordered nominal variable and 
# one non-ordered nominal variable.  
# The Cochran–Armitage test is a special case of this where the 
# non-ordered variable has only two categories.
# The scores option is used to indicate which variable should be treated as 
# ordered, and the spacing of the levels of this variable.
Job <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4,
              dimnames = list(income = c("< 15k", "15-25k", "25-40k", "> 40k"),
                              satisfaction = c("VeryD", "LittleD", 
                                               "ModerateS", "VeryS")))
Job <- as.table(Job)
print(coin::chisq_test(Job,  scores = list("income"= c(1, 2, 3, 4))))
# Teste post hoc
print(ph <- rcompanion::pairwiseOrdinalIndependence(Job,compare="column"))
try(rcompanion::cldList(p.value ~ Comparison, data = ph, threshold = 0.05))
