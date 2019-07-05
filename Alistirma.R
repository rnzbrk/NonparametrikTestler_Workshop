# Mann whitney
ProteinDeney <- data.frame(
Diyet=factor(c(rep(1:2, each = 5)), labels=c("YP","DP")), 
Kilo=c(134,146,104,119,124,70,118,101,85,107) )


ProteinDeney$Siralama <- rank(ProteinDeney$Kilo)


wilcox.test(ProteinDeney$Kilo ~ ProteinDeney$Diyet, alternative = "two.sided")



# Wilcoxon
PsiDeney <- data.frame(
TÖ=c(75	,83,	98,	62,	48,	51), 
TS=c(62,45,	87,	40,	78,	61) )

wilcox.test(PsiDeney$TÖ, PsiDeney$TS, paired=TRUE, alternative = "two.sided")



# Kruskal
HavaDeney <- data.frame(
Şehir=c(rep(c("A","B","C"), c(4,3,3))), 
Değer=c(23	,45,	32,	44,	12,	34,	75,	40,	78	,55) )


kruskal.test(HavaDeney$Değer ~ HavaDeney$Şehir)



# Kikare
Likert <- read.csv2("https://raw.githubusercontent.com/rnzbrk/Workshop/master/likert.csv")


table(Likert$Madde1)
prop.table(table(Likert$Madde1))

table(Likert$Cinsiyet)
prop.table(table(Likert$Cinsiyet))



## Çaprazlık tablosu (Contingecy Table) oluşturma
table (Likert$Madde1, Likert$Cinsiyet)

prop.table(table (Likert$Madde1, Likert$Cinsiyet))

prop.table(table (Likert$Madde1, Likert$Cinsiyet))*100



(t1 <- prop.table(table (Likert$Madde1, Likert$Cinsiyet), margin=1)*100) # satıra göre yüzdelik
(t2 <- prop.table(table (Likert$Madde1, Likert$Cinsiyet), margin=2)*100) # sütuna göre yüzdelik

rowSums(t1)
colSums(t2)


chisq.test(Likert$Madde1, Likert$Cinsiyet)

#  Chi-squared approximation may be incorrect uyarısı beklenen değerin 5 ten küçük olduğu anlamına gelir. Böyle bir durumda Fisher testi kullanılır.



fisher.test(Likert$Madde1, Likert$Cinsiyet) #birbirinden bağımsız değiller.






