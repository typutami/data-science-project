data_intro <- read.csv("https://storage.googleapis.com/dqlab-dataset/data_intro.csv",sep=";")

#check type data
data_intro$ID.Pelanggan <-as.character(data_intro$ID.Pelanggan)
#estimate value
mode(data_intro$Produk)
median(data_intro$Pendapatan)
mean(data_intro$Pendapatan)
max(data_intro$Jumlah)-min(data_intro$Jumlah)
var(data_intro$Pendapatan)
sd(data_intro$Jumlah)
summary(data_intro)

# Visualization of Descriptive analysis
plot(data_intro$Jenis.Kelamin)
hist(data_intro$Pendapatan)
plot(data_intro$Pendapatan,data_intro$Total)
cor.test(data_intro$Pendapatan,data_intro$Total)

#Inferensia analysis
table(data_intro$Produk,data_intro$Tingkat.Kepuasan)
chisq.test(table(data_intro$Produk,data_intro$Tingkat.Kepuasan))

#boxplot function
boxplot(Total~Jenis.Kelamin,data = data_intro)
t.test(Total~Jenis.Kelamin,data = data_intro)

#variable convert from plot/ggplot
plot.jakarta <- ggplot()
plot.jakarta <- plot.jakarta + labs(title="Luas Wilayah vs Kepadatan Penduduk DKI Jakarta - Periode 2013")
plot.jakarta

# read dataset Kependudukan 
penduduk.dki <- read.csv("https://storage.googleapis.com/dqlab-dataset/dkikepadatankelurahan2013.csv", sep=",")
penduduk.dki
penduduk.dki[c("NAMA.KECAMATAN","NAMA.KELURAHAN")]
aes(x = LUAS.WILAYAH..KM2., y=KEPADATAN..JIWA.KM2.,  color=NAMA.KABUPATEN.KOTA)
ggplot(data=penduduk.dki, aes(x = LUAS.WILAYAH..KM2.,  y=KEPADATAN..JIWA.KM2., color=NAMA.KABUPATEN.KOTA))
plot.dki <- ggplot(data=penduduk.dki, aes(x = LUAS.WILAYAH..KM2.,  y=KEPADATAN..JIWA.KM2.,  color=NAMA.KABUPATEN.KOTA))
plot.dki + geom_point() + theme(plot.title = element_text(hjust=0.5)) + […]

#Grouping
inflasi.indo.sing <- read.csv("https://storage.googleapis.com/dqlab-dataset/inflasi.csv", sep=",")
plot.inflasi <- ggplot(data=inflasi.indo.sing, aes(x = Bulan,  y=Inflasi,  color=Negara, […]))
plot.inflasi + geom_line()
factor(inflasi.indo.sing$Bulan, levels = c("Jan-2017", "Feb-2017", "Mar-2017", "Apr-2017", "May-2017", "Jun-2017", "Jul-2017", "Aug-2017", "Sep-2017", "Oct-2017"))
inflasi.indo.sing$Bulan <- factor(inflasi.indo.sing$Bulan, levels = c("Jan-2017", "Feb-2017", "Mar-2017", "Apr-2017", "May-2017", "Jun-2017", "Jul-2017", "Aug-2017", "Sep-2017", "Oct-2017"))

plot.dki <- ggplot(data=penduduk.dki, aes(x = JENIS.KELAMIN))
plot.dki + geom_bar()

aggregate(x=list(RATARATA=penduduk.dki$JUMLAH), FUN=mean, by = list(NAMA.KABUPATEN.KOTA=penduduk.dki$NAMA.KABUPATEN.KOTA, JENIS.KELAMIN=penduduk.dki$JENIS.KELAMIN))

#Data read function
pelanggan <- read.csv("https://storage.googleapis.com/dqlab-dataset/customer_segments.txt", sep="\t")
pelanggan[c("Nama.Pelanggan","Profesi")]
c("Jenis.Kelamin", "Umur", "Profesi", "Tipe.Residen")
data.matrix(pelanggan[c("Jenis.Kelamin", "Profesi")])
data.frame(pelanggan, pelanggan_matrix)
pelanggan[c("Profesi","Profesi.1")]
unique(pelanggan[c("Profesi","Profesi.1")]) 

#kmeans clustering
set.seed(100)
kmeans(x=pelanggan[c("Umur","Profesi.1")], centers=3)
kmeans(x=pelanggan[c("Umur","Profesi.1")], centers=3, nstart=25)

#Analysis size clustering
which(pelanggan$cluster == 1)
length(which(pelanggan$cluster == 1))
pelanggan[which(pelanggan$cluster == 1),]

#Simulation jumlah cluster
sse <- sapply(1:10,
function(param_k)
{kmeans(pelanggan[field_yang_digunakan], param_k, nstart=25)$tot.withinss})

#Elbow effects
jumlah_cluster_max <- 10
ssdata = data.frame(cluster=c(1:jumlah_cluster_max),sse)
ggplot(ssdata, aes(x=cluster,y=sse)) +
            	geom_line(color="red") + geom_point() +
            	ylab("Within Cluster Sum of Squares") + xlab("Jumlah Cluster") +
            	geom_text(aes(label=format(round(sse, 2), nsmall = 2)),hjust=-0.2, vjust=-0.5) +
  scale_x_discrete(limits=c(1:jumlah_cluster_max))


library(ggplot2)

#Bagian Data Preparation
pelanggan <- read.csv("https://storage.googleapis.com/dqlab-dataset/customer_segments.txt", sep="\t")
pelanggan_matrix <- data.matrix(pelanggan[c("Jenis.Kelamin", "Profesi", "Tipe.Residen")])
pelanggan <- data.frame(pelanggan, pelanggan_matrix)
Profesi <- unique(pelanggan[c("Profesi","Profesi.1")])
Jenis.Kelamin <- unique(pelanggan[c("Jenis.Kelamin","Jenis.Kelamin.1")])
Tipe.Profesi <- unique(pelanggan[c("Tipe.Residen","Tipe.Residen.1")])
pelanggan$NilaiBelanjaSetahun <- pelanggan$NilaiBelanjaSetahun/1000000
field_yang_digunakan = c("Jenis.Kelamin.1", "Umur", "Profesi.1", "Tipe.Residen.1","NilaiBelanjaSetahun")
#Bagian K-Means
set.seed(100)
sse <- sapply(1:10, function(param_k){kmeans(pelanggan[field_yang_digunakan], param_k, nstart=25)$tot.withinss})[…]


#Statistic from Dataset Penjualan

# Output Top 10
library(arules)
transaksi_tabular <- read.transactions(file="transaksi_dqlab_retail.tsv", format="single", sep="\t", cols=c(1,2), skip=1)
top10_item <- itemFrequency(transaksi_tabular,type="absolute")
top10_item <- sort(top10_item, decreasing=TRUE)[1:10]
top10_item <- data.frame("Nama.Produk"=names(top10_item),"Jumlah"=top10_item, row.names=NULL)
write.csv(top10_item, file="top10_item_retail.txt")

# Output Bottom 10
library(arules)
transaksi_tabular <- read.transactions(file="transaksi_dqlab_retail.tsv", format="single", sep="\t", cols=c(1,2), skip=1)
bottom10_item <- itemFrequency(transaksi_tabular,type="absolute")
bottom10_item <- sort(bottom10_item, decreasing=FALSE)[1:10]
bottom10_item <- data.frame("Nama.Produk"=names(bottom10_item),"Jumlah"=bottom10_item, row.names=NULL)
write.csv(bottom10_item, file="bottom10_item_retail.txt")

# Output All Combination
library(arules)
transaksi_tabular <- read.transactions(file="transaksi_dqlab_retail.tsv", format="single", sep="\t", cols=c(1,2), skip=1)
kombinasi <- apriori(transaksi_tabular, parameter=list(supp=10/length(transaksi_tabular), confidence=0.5,minlen=2,maxlen=3))
kombinasi <- sort(kombinasi,by='lift', decreasing=TRUE)
kombinasi <- head(kombinasi,10)
inspect(kombinasi)
write(kombinasi, file="kombinasi_retail.txt")

#Analysis Slow moving package product slow moving 
library(arules)
transaksi_tabular <- read.transactions(file="transaksi_dqlab_retail.tsv", format="single", sep="\t", cols=c(1,2), skip=1)
kombinasi_retail_slow_moving <- apriori(transaksi_tabular, parameter=list(supp=10/length(transaksi_tabular), confidence=0.1, minlen=2, maxlen=3))

c1 <- subset(kombinasi_retail_slow_moving, rhs %in% "Tas Makeup")
c2 <- subset(kombinasi_retail_slow_moving, rhs %in% "Baju Renang Pria Anak-anak")

c1 <- head(sort(c1, by="lift", decreasing=TRUE), n=3L)
c2 <- head(sort(c2, by="lift", decreasing=TRUE), n=3L)

final_result <- c(c1, c2)
write(final_result, file="kombinasi_retail_slow_moving.txt")

# External Data
#set library
library(ggplot2)
library(RColorBrewer)
data = read.csv("project.csv")

head(data)
colnames(data)
summary(data$OSL)
summary(data$OSL)
str(data)

data_reduce = data[-c(1,2)]
data_kategorik = data_reduce[,c("KONDISI_USAHA","KONDISI_JAMINAN","REKOMENDASI_TINDAK_LANJUT")]
chisq.test(data_kategorik$KONDISI_JAMINAN, data_kategorik$KONDISI_USAHA)
par(mfrow=c(2,2))
data_reduce$REKOMENDASI_TINDAK_LANJUT = as.factor(data_reduce$REKOMENDASI_TINDAK_LANJUT)

colnames(data_reduce)
data_select = data_reduce[,c("KARAKTER","KONDISI_USAHA","KONDISI_JAMINAN","STATUS","KEWAJIBAN","OSL","KOLEKTIBILITAS","REKOMENDASI_TINDAK_LANJUT")]
data_non_na = na.omit(data_select)

data_select_new = data_select
data_select_new$KEWAJIBAN = scale(data_select_new$KEWAJIBAN)[,1]
data_select_new$OSL = scale(data_select_new$OSL)[,1]
data_select_new$KEWAJIBAN = cut(data_select_new$KEWAJIBAN, breaks = c(-0.354107,5,15,30))
data_select_new$KEWAJIBAN = as.factor(data_select_new$KEWAJIBAN)
data_select_new$OSL = cut(data_select_new$OSL, breaks = c(-0.60383,3,10,15))
data_select_new$OSL = as.factor(data_select_new$OSL)
data_select_new = na.omit(data_select_new)

index = createDataPartition(data$REKOMENDASI_TINDAK_LANJUT, p = .95, list = FALSE)
library(caret)
index = createDataPartition(data_select_new$REKOMENDASI_TINDAK_LANJUT, p = .95, list = FALSE)
train = data_select_new[index,]
test = data_select_new[-index,]

train2 = train
#Setting the reference
train2$REKOMENDASI_TINDAK_LANJUT = relevel(train2$REKOMENDASI_TINDAK_LANJUT, ref = "Angsuran Biasa")
#training the model
require(nnet)
#Training the multinational model
multinom_model = multionom(REKOMENDASI_TINDAK_LANJUT ~ ., data = train2)
 
 
#Checking the model
summary(multinom_model)
#converting the coefficients to adds by taking the exponential of the coefficients.
exp(coef(multinom_model))
head(round(fitted(multinom_model), 2))
#Predicting the values for train dataset
train2$ClassPredicted = predict(multinom_model, newdata = train2, "class")
train_prob = predict(multinom_model, newdata = train2, "probs")
df = train_prob
df$max=apply(df,1, max)
train2$score = df$max
test_prob = predict(multinom_model, newdata = test, "probs")
df2 = test_prob
df2$max=apply(df2,1,max)
#Building classification table
tab_train2 = table(train2$REKOMENDASI_TINDAK_LANJUT, train2$ClassPredicted)
round((sum(diag(tab_train))/sum(tab_train))*100,4)
test$ClassPredicted - predict(multinom_model, newdata = test, "class")
test$score =df2$max
tab_test = table(test$REKOMENDASI_TINDAK_LANJUT, test$ClassPredict)
round((sum(diag(tab_test))/sum(tab_test))*100,4)

# Building classification table
tab_train = table(train2$REKOMENDASI_TINDAK_LANJUT, train$ClassPredicted)
round((sum(diag(tab_train))/sum(tab_train))*100,4)
test$ClassPredicted = predict(multinom_model, newdata = test, "class")
test$score = df2$max
tab_test = table(test$REKOMENDASI_TINDAK_LANJUT, ...)
round((sum(diag(tab_test))/sum(tab_test))*100,4)

#Data frame
df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))
df[,"b"]
df[1,"a"]
df[3,"b"]

x <- c("red","blue","yellow","orange","green","purple")
y <- x[2:4]
y


x <- factor(c("grape", "apples", "pear", "cherry", "mango", "panda"))
x[c(1, 2, 3, 4, 5, 6)]
x[6] <- "apples"
x[c(1, 2, 3, 4, 5, 6)]

add_numbers <- sum(x = 3, y = 3)
add_numbers 

df <- c(1,2,3,4,5,6,NA,7,8,9,NA)
df
mean_replace <- function(x){x[is.na(x)] <- mean (x, na.rm=TRUE);}
df[7] <- mean_replace(df)
df[11] <- mean_replace(df)
df

library(readr)
trees_df <- read_csv("trees.csv")
names(trees_df)
str(trees_df)
names(trees_df)[1] <- "diameter"
trees_df$diameter_ft <- trees_df$diameter*0.08333
head(trees_df)
summary(trees_df)
is.na(trees_df)

#shapiro test
shapiro.test(trees_df$diameter_ft)
shapiro.test(trees_df$Height)
shapiro.test(trees_df$Volume)

#Visualisasi Sederhana
plot(density(trees_df$Volume))
Mencari Hubungan
Bagaimanakah hubungan antara Volume batang pohon Cherry dengan diameter dan ketinggian (height)?
 
Gunakan pula visualisasi sederhana untuk menjelaskan hubungan tersebut.
lm(formula=Volume~Height+diameter_ft, data=trees_df)
plot(trees_df$diameter_ft, trees_df$Volume)
plot(trees_df$Height, trees_df$Volume)

#Analyzing effect sleep medicine
library(readr) #pre-defined
library(dplyr) #pre-defined
 
sleep_df <- read_csv('sleep.csv') #pre-defined
 
# Save the data in two different dataframe/vector
group1 <- filter(sleep_df, sleep_df$group == 1)
group2 <- filter(sleep_df, sleep_df$group == 2)
 
# Compute t-test
t_test <- t.test(group1$extra, group2$extra)
t_test

#Boxplot chart
library(ggplot2)
ggplot(sleep_df, aes(x=as.character(group), y=extra, fill=as.character(group))) + geom_boxplot()

#Make simple model
library(readr)
electric_bill <- read_csv("electric_bill.csv")
model <- lm(amount_paid ~ num_people + housearea, data=electric_bill)


#Training dan testing
library(readr)
library(caret)
set.seed(123)
iris <- read_csv("iris.csv")
 
trainIndex <- createDataPartition(iris$Species, p=0.8, list=FALSE)
training_set <- iris[trainIndex,]
testing_set <- iris[-trainIndex,]
 
dim(training_set)
dim(testing_set)


#Decision Tree
library(caret) #pre-defined 
library(rpart) #pre-defined
library(readr) #pre-defined
set.seed(123)  #pre-defined
 
suv_data <- read_csv("suv_data.csv") #pre-defined
 
#split data to training & testing set
trainIndex <- createDataPartition(suv_data$Purchased, p=0.8, list=FALSE)
training_set <- suv_data[trainIndex,]
testing_set <- suv_data[-trainIndex,]
 
#build model with decision tree
model_dt <- rpart(Purchased ~.,data=training_set, method="class")
predictions_dt <-predict(model_dt, newdata=testing_set, type="class")
 
#evaluate performance with new data/ testing_set
testing_purchased <- factor(testing_set$Purchased) #pre-defined 
 
#show the evaluation result 
evaluation_result <- confusionMatrix(predictions_dt, testing_purchased)
evaluation_result









