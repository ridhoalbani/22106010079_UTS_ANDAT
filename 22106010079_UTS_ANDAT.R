#2b(Read Data)
getwd()
setwd("C:/Users/USER/Documents/rdh")
uts_anadata=read.csv("Employee Attrition.csv",sep = ";",header=TRUE)
uts_anadata

#2c(Check the Packaging)
str(uts_anadata)
summary(uts_anadata)

#2d(Look at the top and the button of data)
head(uts_anadata)
tail(uts_anadata)

#2e(Check "n"s)
dim(uts_anadata)
colSums(is.na(uts_anadata))

#2f(Validate)
summary(uts_anadata$satisfaction_level)
quantile(uts_anadata$satisfaction_level)

#2g(Make a plot)
ggplot(uts_anadata,aes(x=average_montly_hours,y=satisfaction_level,
                       color=last_evaluation))+geom_point()+
  labs(x="Jam Kerja Bulanan",y="Tingkat kepuasaan karyawan",
       color="Hasil Evaluasi Terakhir")+theme_minimal()

#2h(try easy solution)
uts_anadata$average_montly_hours=factor(ifelse(uts_anadata$average_montly_hours<190,"Rendah","Tinggi"))
group_by(uts_anadata,average_montly_hours)%>%
  summarize(mean=mean(satisfaction_level, na.rm = T),
            median=median(satisfaction_level,na.rm = T))

#3a(Model as expectations)
model <- lm(satisfaction_level ~ average_montly_hours +
              last_evaluation, data = uts_anadata)
model
summary(model)
#3b(comparing model expectations to reality)
ggplot(uts_anadata, aes(x = satisfaction_level)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Distribusi Satisfaction Level dengan Kurva Normal") +
  xlab("Satisfaction Level") +
  ylab("Density") +
  theme_minimal()


