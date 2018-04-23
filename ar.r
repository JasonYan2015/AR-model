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
# moveZhnindown    0.5494         0.5        0.478       0.6207

# Likelihood ratio test that transformation parameter is equal to 0
#  (log transformation)
#                           LRT df       pval
# LR test, lambda = (0) 260.545  1 < 2.22e-16

# Likelihood ratio test that no transformation is needed
#                            LRT df       pval
# LR test, lambda = (1) 137.2455  1 < 2.22e-16

#------------------------

# 得到lamda值为0.5494,p值为0.5，然后进行box-cox变换——公式：(y^λ-1)/λ  λ≠0
newZhnindown <- (I(zhonghnanindown^0.3858-1)/0.3858);

# 变换完数据集，进行自回归建模（TODO. 可以校验一下正态性），(自回归建模参考)[http://blog.fens.me/r-ar/]
# 使用ar函数默认的yule-walker方法进行参数估计
a <- ar(newZhnindown)
# -----------------------

# Call:
# ar(x = newZhnindown)

# Coefficients:
#       1        2        3        4        5        6        7        8  
#  0.2303   0.2521   0.2184   0.1204   0.0949   0.0428  -0.0107   0.0068  
#       9       10       11       12       13       14       15       16  
#  0.0797  -0.0283   0.0131   0.0165   0.0213   0.0197  -0.0181   0.0318  
#      17       18       19       20       21       22       23       24  
# -0.0132   0.0486  -0.0734  -0.0040   0.0149  -0.0443  -0.0276   0.1181  
#      25       26       27       28  
# -0.0131  -0.0365  -0.0168  -0.0835  

# Order selected 28  sigma^2 estimated as  0.9986

#-------------------------


# 使用predict函数进行模型预测
# 生成50个预测值 
tsp<-predict(a,n.ahead=50L)
#-------------------------

# $pred
# Time Series:
# Start = 1000 
# End = 1049 
# Frequency = 1 
#  [1] 4.992833 4.808357 4.782427 4.407623 4.522080 4.601472 4.241795 4.392671
#  [9] 4.217956 4.251105 4.207698 4.052189 3.930483 4.008075 3.763288 3.718065
# [17] 3.845139 3.827739 3.777012 3.679977 3.755798 3.583884 3.765137 3.621687
# [25] 3.559396 3.670637 3.578380 3.586801 3.565715 3.592738 3.541759 3.610740
# [33] 3.557863 3.574196 3.634054 3.585924 3.611685 3.628129 3.633528 3.628649
# [41] 3.692040 3.690563 3.722204 3.759293 3.759824 3.780361 3.830680 3.851201
# [49] 3.857091 3.922971

# $se
# Time Series:
# Start = 1000 
# End = 1049 
# Frequency = 1 
#  [1] 0.9992997 1.0254519 1.0698204 1.1245145 1.1711518 1.2228555 1.2693817
#  [8] 1.3063005 1.3457278 1.4018759 1.4370759 1.4806713 1.5255583 1.5718853
# [15] 1.6216985 1.6647341 1.7182216 1.7652237 1.8280873 1.8668862 1.9167745
# [22] 1.9699666 2.0107499 2.0526243 2.1220648 2.1717137 2.2191777 2.2716401
# [29] 2.3053449 2.3514831 2.3946019 2.4323356 2.4713729 2.5107453 2.5475114
# [36] 2.5836871 2.6201663 2.6530061 2.6884431 2.7208583 2.7523369 2.7832237
# [43] 2.8148022 2.8424532 2.8695880 2.8975198 2.9209709 2.9456329 2.9702458
# [50] 2.9922947

# ------------------------

# 预测数据进行box-cox逆变换，公式：y=(1+λy)^(1/λ)
pred <- I((1 + 0.5494*tsp$pred)^(1/0.5494))
se <- I((1 + 0.5494*tsp$se)^(1/0.5494))

# 把原数据画图 
plot(zhonghnanindown50)

# 把预测值和误差画出来
lines(pred,col='red')
lines(pred+se,col='blue')
lines(pred-se,col='blue')

# 图中，黑色线为原始数据的，红色线为预测值，蓝色线为预测值的范围。这样我们就利用AR(1)模型，实现了对规律的预测计算。



# # ----------- 其他方法进行参数估计，暂时没用到 ------------------

# # 进行自回归建模，默认的yule-walker方法
# a <- ar(newZhnindown)
# # 最小二乘法，进行参数估计
# b <- ar(newZhnindown,method = "ols")
# # 极大似然法，进行参数估计
# d <- ar(newZhnindown,method = "mle")
# # 使用AR(1)模型进行预测，并保留前5个预测点。
# predict(a, 10, n.ahead = 5L)
# # 上面结果中，变量$pred表示预测值，变量$se为误差。
