setwd("~/upstat-competition/")

mcas = read.csv("~/Documents/workspace/DataCompete/Student Data Competition/MCAS.csv")

correct.answers = read.csv("correct-answers-physics-biology.csv", stringsAsFactors=FALSE)
correct.answers$item.number = 1:45

# Numerical scored questions don't help, so remove them
correct.answers = correct.answers[correct.answers$Biology != 'Blank', ]

multiple.choice.column.names = paste0("sitem", correct.answers$item.number)

sum(mcas[1, multiple.choice.column.names] == correct.answers$Biology)

print(sum(mcas[1, multiple.choice.column.names] == correct.answers$Biology))
print(sum(mcas[1, multiple.choice.column.names] == correct.answers$Physics))
rm(correct.answers,mcas)
