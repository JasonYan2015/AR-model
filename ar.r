# car包的box-cox变换
print("car包的box-cox变换");

# 载入car包 用于box-cox变换
library(car);

# 获取数据
data <- read.csv("donglianin.csv");

# 抽样　由excel图标得　4300到7300的数据相对稳定，由于不必要求过大，再抽取5000~6000之间，sample50用于最后对比预测数据
sample <- subset(data, id > 5000 & id < 6000);
sample50 <- subset(data, id > 5000 & id < 6050);

# 抽样　建立一元回归　抽样中华南入市下桥路口数据,因为(参考)[http://bbs.pinggu.org/thread-2741562-1-1.html]所说，数据不能有零，所以加上１偏移
zhonghnanindown <- sample$zhonghnanindown;
zhonghnanindown50 <- sample50$zhonghnanindown;
moveZhnindown <- sample$zhonghnanindown + 1;

# 获取lamda准备进行box-cox变换　为　正态/伽马　分布
summary(p1 <- powerTransform(moveZhnindown));
#-----------------------

# bcPower Transformation to Normality 
#               Est Power Rounded Pwr Wald Lwr Bnd Wald Upr Bnd
# moveZhnindown    0.3858        0.39       0.3487        0.423

# Likelihood ratio test that transformation parameter is equal to 0
#  (log transformation)
#                            LRT df       pval
# LR test, lambda = (0) 448.6252  1 < 2.22e-16

# Likelihood ratio test that no transformation is needed
#                            LRT df       pval
# LR test, lambda = (1) 916.3945  1 < 2.22e-16

#------------------------

# 得到lamda值为0.3858,p值为0.39，然后进行box-cox变换——公式：(y^λ-1)/λ  λ≠0
newZhnindown <- (I(zhonghnanindown^0.3858-1)/0.3858);

# 变换完数据集，进行自回归建模（TODO. 可以校验一下正态性），(自回归建模参考)[http://blog.fens.me/r-ar/]
# 使用ar函数默认的yule-walker方法进行参数估计
a <- ar(newZhnindown)
# -----------------------

# Call:
# ar(x = newZhnindown)

# Coefficients:
#       1        2        3        4        5        6        7        8  
#  0.2856   0.2147   0.1553   0.1463   0.1017   0.0492   0.0268   0.0713  
#       9       10       11       12       13       14       15       16  
#  0.0536  -0.0221  -0.0117   0.0426  -0.0410   0.0056  -0.0260   0.0109  
#      17       18       19       20       21       22       23       24  
# -0.0165   0.0352  -0.0420  -0.0123  -0.0038  -0.0025  -0.0113   0.0450  
#      25       26       27       28  
# -0.0294  -0.0310   0.0104  -0.0458  

# Order selected 28  sigma^2 estimated as  1.121

#-------------------------


# 使用predict函数进行模型预测
# 生成50个预测值 
tsp<-predict(a,n.ahead=50L)
#-------------------------

# $pred
# Time Series:
# Start = 3000 
# End = 3049 
# Frequency = 1 
#  [1] 10.984058 11.090837 11.375280 11.068381 11.261413 11.482649 11.353837
#  [8] 11.303862 11.473346 11.460151 11.429016 11.549893 11.411719 11.525515
# [15] 11.443431 11.422727 11.356580 11.273923 11.308299 11.171059 11.134529
# [22] 11.145582 10.947574 10.940129 10.895464 10.745846 10.733443 10.628885
# [29] 10.519076 10.455944 10.325950 10.234573 10.146498 10.031976  9.918139
# [36]  9.829721  9.707250  9.596979  9.489302  9.367548  9.254540  9.135571
# [43]  9.025676  8.901383  8.788789  8.677785  8.547479  8.439502  8.323825
# [50]  8.199141

# $se
# Time Series:
# Start = 3000 
# End = 3049 
# Frequency = 1 
#  [1] 1.058710 1.101035 1.144843 1.188421 1.241818 1.296325 1.345854 1.393389
#  [9] 1.456061 1.523336 1.576704 1.630618 1.699595 1.753089 1.814129 1.867452
# [17] 1.928819 1.985562 2.053202 2.105402 2.161735 2.218503 2.274628 2.328102
# [25] 2.393266 2.445921 2.496621 2.551286 2.595332 2.642767 2.689697 2.733606
# [33] 2.777936 2.820272 2.860948 2.900117 2.939113 2.974529 3.009931 3.043847
# [41] 3.076035 3.107266 3.137418 3.165687 3.192657 3.218842 3.243091 3.266561
# [49] 3.289146 3.310031

# ------------------------

# 预测数据进行box-cox逆变换，公式：y=(1+λy)^(1/λ)
result <- I((1 + 0.3858*tsp)^(1/λ))

# 把原数据画图 
plot(zhonghnanindown50)

# 把预测值和误差画出来
lines(result$pred,col='red')
lines(result$pred+result$se,col='blue')
lines(result$pred-result$se,col='blue')

# 图中，黑色线为原始数据的，红色线为预测值，蓝色线为预测值的范围。这样我们就利用AR(1)模型，实现了对规律的预测计算。



# ----------- 其他方法进行参数估计，暂时没用到 ------------------

# 进行自回归建模，默认的yule-walker方法
a <- ar(newZhnindown)
# 最小二乘法，进行参数估计
b <- ar(newZhnindown,method = "ols")
# 极大似然法，进行参数估计
d <- ar(newZhnindown,method = "mle")
# 使用AR(1)模型进行预测，并保留前5个预测点。
predict(a, 10, n.ahead = 5L)
# 上面结果中，变量$pred表示预测值，变量$se为误差。
