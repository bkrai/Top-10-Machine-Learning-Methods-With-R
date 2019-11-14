# Reading the data file
mydata<-read.csv("C:\\Users\\brai\\Desktop\\Cosmetics.csv",header=T)

# Finding association rules
library(arules)
rules <- apriori(mydata)

# Rules with specified parameter valus
rules <- apriori(mydata,parameter = list(minlen=2, maxlen=10,supp=.7, conf=.8))

# Finding interesting rules-1
rules <- apriori(mydata,parameter = list(minlen=2, maxlen=3,supp=.01, conf=.7),appearance=list(rhs=c("Foundation=Yes"),lhs=c("Bag=Yes", "Blush=Yes"),default="lhs"))

# Finding interesting rules-2
rules <- apriori(mydata,parameter = list(minlen=2, maxlen=5,supp=.1, conf=.5),appearance=list(rhs=c("Foundation=Yes"),lhs=c("Bag=Yes", "Blush=Yes", "Nail.Polish=Yes", "Brushes=Yes", "Concealer=Yes", "Eyebrow.Pencils=Yes", "Bronzer=Yes", "Lip.liner=Yes", "Mascara=Yes", "Eye.shadow=Yes","Lip.Gloss=Yes", "Lipstick=Yes", "Eyeliner=Yes"),default="none"))
quality(rules)<-round(quality(rules),digits=3)
rules.sorted <- sort(rules, by="lift")

# Finding redundancy
redundant <- is.redundant(rules, measure="confidence")
which(redundant)
rules.pruned <- rules[!redundant]
rules.pruned <- sort(rules.pruned, by="lift")
inspect(rules.pruned)

# Graphs and Charts
library(arulesViz)
plot(rules.all)
plot(rules.all,method="grouped")
plot(rules.all,method="graph",control=list(type="items"))
