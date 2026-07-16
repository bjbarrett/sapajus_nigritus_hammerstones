library(dagitty)
dag1 <- dagitty("dag{
                  sex -> weight
                  weight -> time
                  age-> weight
                  U -> weight
                  U <- nut_type
                  age -> weight
                  nut_type -> time
                  nut_type -> force_nut
                  force_nut -> time
                  }
                   ")

dag1
plot(dag1)
adjustmentSets(dag1, 
               exposure = "age",
               outcome = "time", 
               effect = "direct")
# time ~ age + nut_type + weight
adjustmentSets(dag1, 
               exposure = "age",
               outcome = "time", 
               effect = "total")
# time ~ age


adjustmentSets(dag1, 
               exposure = "force_nut",
               outcome = "time", 
               effect = "total")