anno = read.delim("http://anakin.intelliseq.pl/insecure/zajecia-4/anno.csv", sep=",")
data = read.delim("http://anakin.intelliseq.pl/insecure/zajecia-4/data.csv", sep=",")
samples = read.delim("http://anakin.intelliseq.pl/insecure/zajecia-4/samples.csv", sep=",")
rownames(data) = data$ProbesetID
data = data[,-1]
substr(colnames(data),2,10000) # pozbycie sie X
colnames(data)<-substr(colnames(data),2,10000)  
data$ProbesetID  # to juz usunelismy!
rownames(data)   # to jest nasze poprzednie data$ProbesetID
anno$Probe.Set<-anno$Probe.Set[order(match(anno$Probe.Set,rownames(data)))]
str(samples)
data <- as.matrix(data)

# anova jednoczynnikowa dla pierwszej myszki
# to [1,5] to jest odczytanie p.value
anova(aov(data[1,]~samples$Treat))
anova(aov(data[1,]~samples$Treat))[1,5]
class(samples$Treat)
samples$Time <- as.factor(samples$Time)
# anova dwuczynnikowa dla jednej myszki

anova(aov(data[1,]~samples$Treat*samples$Time))

anova(aov(data[1,]~samples$Treat*samples$Time))[1:3,5]   #wyciagniecie p.value

# anova dla wszystkich myszek
class(anova(aov(data[1,]~samples$Treat*samples$Time))[1:3,5] )
(tab_pvalue = t(apply(data[1:114,],1, function(x) anova(aov(x~samples$Treat*samples$Time))[1:3,5])))

# adjust dziala na wektorach, nie macierzach
q_1 = p.adjust(tab_pvalue[,1],method= "fdr")

q_2 = p.adjust(tab_pvalue[,2],method= "fdr")

q_3 = p.adjust(tab_pvalue[,3],method= "fdr")

ost = data.frame()
