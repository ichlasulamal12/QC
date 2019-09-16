library(readxl)
xr=read_excel("D:/Dokumen/Pemrograman/R/QC/tugas1.xlsx", sheet=1, col_names=T)
xmr=read_excel("D:/Dokumen/Pemrograman/R/QC/tugas1.xlsx", sheet=2, col_names=T)

# Xbar-R
library(qcc)
rchart = qcc(xr, type = 'R') #membuat R Chart dari data xr
plot(rchart)
xbarchart = qcc(xr, type = 'xbar', std.dev = 'UWAVE-R') #membuat Xbar Chart dari data xr
plot(xbarchart)

xr1=xr[-c(1,4,6,8,12,13),] #menghapus data yang tidak terkendali, yaitu data 1,4,6,8,12,13
rchart1 = qcc(xr1, type = 'R')
plot(rchart1)
xbarchart1 = qcc(xr1, type = 'xbar', std.dev = 'UWAVE-R')
plot(xbarchart1)

#I-MR
library(ggplot2)
library(ggQC)

theme_update(plot.title = element_text(hjust = 0.5))

mR_Plot <- 
  ggplot(xmr, aes(x = Hari_pengolahan, y = I)) + #init ggplot
  stat_QC(method="mR",
          auto.label = T,
          label.digits = 4, 
  ) +
  xlab('Hari Pengolahan') +
  ylab('Moving Range Kadar Air') +
  ggtitle("MR Chart for Kadar Air") +
  scale_x_continuous(expand =  expand_scale(mult = .15))

mR_Plot

xmr1=xmr[-c(12,16),]

mR1_Plot <- 
  ggplot(xmr1, aes(x = Hari_pengolahan, y = I)) + #init ggplot
  stat_QC(method="mR",
          auto.label = T,
          label.digits = 4, 
  ) +
  xlab('Hari Pengolahan') +
  ylab('Moving Range Kadar Air') +
  ggtitle("MR Chart for Kadar Air") +
  scale_x_continuous(expand =  expand_scale(mult = .15))

mR1_Plot

Xbar1_Plot <- 
  ggplot(xmr1, aes(x = Hari_pengolahan , y = I)) + #init ggplot
  geom_point() + geom_line() + #menambah titik dan garis
  stat_QC(method = "XmR", #menentukan metode yang dipakai
          auto.label = T, #menggunakan autolabels
          label.digits = 4, #digit yang digunakan dalam label
          show.1n2.sigma = T  #menampilkan garis 1 dan 2 sigma
  )+
  xlab('Hari Pengolahan') +
  ylab('Kadar Air (%)') +
  ggtitle("Individual Chart for Kadar Air") +
  scale_x_continuous(expand =  expand_scale(mult = .15))  #expand sumbu-x

Xbar1_Plot

QC_Violations_xbar <- 
  ggplot(xmr1, aes(x = Hari_pengolahan, y = I)) + #init ggplot
  stat_qc_violations(method = "XmR" 
                     #show.facets = 4 #jika hanya ingin menampilkan facet ke-4
  ) +
  xlab('Hari Pengolahan') +
  ylab('Kadar Air (%)')

QC_Violations_xbar
