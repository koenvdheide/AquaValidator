require(tree)
drinkwater = read.csv("oefeningen\\water_potability.csv")
attach(drinkwater)

drinkwater_tree = tree(Potability ~ .,data=drinkwater, method = 'class')

plot(drinkwater_tree)
text(drinkwater_tree)

summary(drinkwater_tree)

