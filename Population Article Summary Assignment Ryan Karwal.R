myData = read.csv(url("https://raw.githubusercontent.com/RyanKarwal/Population-Assignment/master/Populations.csv"))

myData

head(myData)
G = myData$GUILDFORD
CC = myData$CITY_CENTRE

hist(G)
hist(CC)


nG = length(G)
nCC = length(CC)
dfG = nG-1
dfCC = nCC-1
df_total = dfG+dfCC
meanG = sum(G)/nG
meanCC = sum(CC)/nCC
mean_diff = meanG - meanCC
centeredG = G - meanG
centeredCC = CC - meanCC
ssG = sum(centeredG^2)
ssCC = sum(centeredCC^2)
vG = ssG/dfG
vCC = ssCC/dfCC
vp = (ssG+ssCC)/(dfG+dfCC)
se_p = sqrt(vp/nG + vp/nCC)
t = mean_diff/se_p
t

dscore = G - CC
dscore
dsquare = dscore^2
dsquare
dscoresum = sum(dscore)
dscoresum
dsquaresum = sum(dsquare)
dsquaresum

mDiff = dscoresum/nG
ss = dsquaresum - ((dscoresum)^2/nG)
ss
v = ss/dfG ##s^2 or sample variance
se = sqrt(v/nG) ##Standard error
v
se
se_p
trep = mDiff/se ##t-test for repeated measures data
trep

listofmeans = c(meanG,meanCC)
listofsem = c(se,se)

midpoints = barplot(listofmeans, main = "Populations", xlab = "GUILDFORD & CITY_CENTRE",ylab = "Population", ylim = c(0,63000))
arrows(
  midpoints, listofmeans-listofsem, ##Start point of lines
  midpoints, listofmeans+listofsem, ##End point of lines
  length=0.05,angle=90,code=3,
)

Fmax = vCC/vG
Fmax

r2 =  t^2/(t^2+df_total)
r2

p = 2*pt(t,df_total,lower.tail=FALSE)
p
