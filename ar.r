# car包的box-cox变换
print("car包的box-cox变换");
# 载入car包 用于box-cox变换
library(car);
# 获取数据
data <- read.csv("donglianin.csv");
# 抽样　由excel图标得　4300到7300的数据相对稳定
sample <- subset(data, id > 4300 & id < 7300);
# 抽样　建立一元回归　抽样中华南入市下桥路口数据,因为(参考)[http://bbs.pinggu.org/thread-2741562-1-1.html]所说，数据不能有零，所以加上１偏移
zhonghnanindown <- sample$zhonghnanindown;
moveZhnindown <- sample$zhonghnanindown + 1;
# 获取lamda准备进行box-cox变换　为　正态/伽马　分布
summary(p1 <- powerTransform(moveZhnindown));
# 得到lamda值为0.3858,p值为0.39，然后进行box-cox变换——公式：(y^λ-1)/λ  λ≠0
newZhnindown <- (I(zhonghnanindown^0.3858-1)/0.3858);
# 变换完数据集，进行自回归建模（TODO. 可以校验一下正态性），(自回归建模参考)[http://blog.fens.me/r-ar/]
# 使用ar函数默认的yule-walker方法进行参数估计
a<-ar(newZhnindown)
# 使用predict函数进行模型预测
# 生成50个预测值 
> tsp<-predict(a,n.ahead=50L)

# 把原数据画图 
> plot(newZhnindown)

# 把预测值和误差画出来
> lines(tsp$pred,col='red')                
> lines(tsp$pred+tsp$se,col='blue')
> lines(tsp$pred-tsp$se,col='blue')

# 进行自回归建模
a <- ar(newZhnindown)
# 最小二乘法，进行参数估计
b <- ar(newZhnindown,method = "ols")
# 极大似然法，进行参数估计
d <- ar(newZhnindown,method = "mle")
# 使用AR(1)模型进行预测，并保留前5个预测点。
predict(a, 10, n.ahead = 5L)
# 上面结果中，变量$pred表示预测值，变量$se为误差。
# 我可以生成可视化的图，更直观的看到预测的结果。

# 生成50个预测值
tsp <- predict(a, n.ahead = 50L)

# 把原数据画图
plot(newZhnindown)

# 把预测值和误差画出来
lines(tsp$pred, col = 'red')
lines(tsp$pred + tsp$se, col = 'blue')
lines(tsp$pred - tsp$se, col = 'blue')

# 图中，黑色线为原始数据的，红色线为预测值，蓝色线为预测值的范围。这样我们就利用AR(1)模型，实现了对规律的预测计算。




# # MASS包的box-cox变换
# print("MASS包的box-cox变换");
# # 载入ＭASS包
# library(MASS);
# # 获取数据
# data <- read.csv("donglianin.csv");
# # 抽样　由excel图标得　4300到7300的数据相对稳定
# sample <- subset(data, id > 4300 & id < 7300);
# # 抽样　建立一元回归　抽样中华南入市下桥路口数据,(参考)[https://blog.csdn.net/fitzgerald0/article/details/75212215]
# zhonghnanindown <- sample$zhonghnanindown;
# # 准备进行box-cox变换　为　正态/伽马　分布
