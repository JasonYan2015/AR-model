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

# 获取lamda准备进行box-cox变换　为正态分布 TODO:变换为伽马分布
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
newZhnindown <- (I(zhonghnanindown ^ 0.5494 - 1) / 0.5494);

# 变换完数据集，进行自回归建模，(自回归建模参考)[http://blog.fens.me/r-ar/]
# 使用ar函数默认的yule-walker方法进行参数估计(TODO:滞后阶数过高，由acf和pacf得只需要５阶即可即AR(5)模型)
# 0514更新：新增调用参数　aic和order.max ,由《R语言核心技术手册》——　Joseph Adler　Ｐ521得，使用赤池信息准则来选择模型阶数，同事指定模型拟合的最大阶数为１０
a <- ar(newZhnindown, aic = TRUE, order.max = 10)
# -----------------------

# Call:R
# ar(x = newZhnindown)

# Coefficients:
#       1        2        3        4        5        6        7        8  
#  0.2598   0.3000   0.2001   0.1144   0.0987   0.0472  -0.0333   0.0049  
#       9       10       11       12       13       14       15       16  
#  0.0341  -0.0081   0.0195   0.0084   0.0204   0.0077  -0.0223   0.0356  
#      17       18       19       20       21       22       23       24  
# -0.0225   0.0476  -0.0615  -0.0093  -0.0189  -0.0488  -0.0238   0.1101  
#      25       26  
# -0.0387  -0.0551  

# Order selected 26  sigma^2 estimated as  2.123

#-------------------------


# 使用predict函数进行模型预测
# 生成50个预测值 
tsp <- predict(a, n.ahead = 50L)
#-------------------------

# $pred
# Time Series:
# Start = 1000 
# End = 1049 
# Frequency = 1 
#  [1] 6.717644 6.258714 6.321540 5.744826 6.142582 5.903301 5.528436 5.631741
#  [9] 5.550790 5.543312 5.425317 5.409350 5.330711 5.334547 5.093918 5.108945
# [17] 5.351206 5.253423 5.054048 5.030370 5.267283 5.036321 5.194374 5.043150
# [25] 5.111459 5.170172 5.144542 5.118725 5.202447 5.229128 5.192884 5.258703
# [33] 5.276997 5.327516 5.363783 5.382599 5.432289 5.473146 5.507431 5.523998
# [41] 5.633388 5.651041 5.676638 5.728269 5.806224 5.832015 5.899608 5.934370
# [49] 5.985089 6.058441

# $se
# Time Series:
# Start = 1000 
# End = 1049 
# Frequency = 1 
#  [1] 1.457213 1.505596 1.598027 1.688207 1.773891 1.872352 1.964764 2.034452
#  [9] 2.113211 2.198867 2.272435 2.355847 2.435375 2.521395 2.608154 2.686650
# [17] 2.783543 2.865604 2.973031 3.049626 3.138375 3.220286 3.290446 3.359534
# [25] 3.463278 3.531729 3.596865 3.668005 3.732873 3.800792 3.865298 3.924514
# [33] 3.984622 4.043092 4.099452 4.153032 4.206168 4.256091 4.305251 4.350496
# [41] 4.395841 4.438055 4.481176 4.519193 4.556923 4.593524 4.626638 4.658935
# [49] 4.692061 4.721306

# ------------------------

# 预测数据进行box-cox逆变换，公式：y=(1+λy)^(1/λ)
# tsp预测数据中　pred为预测值,se为误差范围
pred <- I((1 + 0.5494 * tsp$pred) ^ (1 / 0.5494))
se <- I((1 + 0.5494 * tsp$se) ^ (1 / 0.5494))

# 把原数据画图 
plot(zhonghnanindown50)

# 把预测值和误差画出来
lines(pred, col='red')
lines(pred + se, col='blue')
lines(pred - se, col='blue')

# 图中，黑色线为原始数据的，红色线为预测值，蓝色线为预测值的范围。这样我们就利用AR(1)模型，实现了对规律的预测计算。



# --------------------------------------------------------------------
# ----------- 后续分析 ------------------
# 加载forecast包
library('forecast')
library('tseries')

# 查看自相关性和偏自相关性
# 自相关性图表明自相关系数长期大于０，说明序列间具有很强的长期相关性，为非平稳序列，需要差分处理
Acf(zhonghnanindown)
Pacf(zhonghnanindown)

# 对数据作一阶差分
dZhonghnanindown <- diff(zhonghnanindown)

# 再次做自相关性和偏自相关性
# 结果表明具有很强的短期相关性，是平稳序列了
# 自相关图显示出一阶截尾，偏自相关呈现出拖尾性，所以可以考虑用MA(1)模型拟合，即对原始数据建立ARIMA(0,1,1)模型
Acf(dZhonghnanindown)
Pacf(dZhonghnanindown)

# 一种自动的ARIMA模型参数判断
# 如果有备选模型，可以再计算AIC,比较AIC小的就是更好的模型
auto.arima(zhonghnanindown)

# 均值平稳性验证

# p值较大　无法拒绝原假设
adf.test(zhonghnanindown)

# p值较小　接受备择假设，均值平稳
adf.test(dZhonghnanindown)



# -------------------------------------------------------------
# ------------------- 伽马分布 ------------------------
# 由于源数据是非负整数分布，因此使用指数转换　log(x + 1)　转换到正实数，以满足伽马分布
# 伽马分布后续步骤可以使用 GAMLSS 或 GLM 做
# 新截取数据
testD <- subset(data, id > 5000 & id < 6000, select = c(id, time, zhonghnanindown))

# 泊松分布拟合泊松回归 TODO. formula　不知道怎么写，也因此　data　存在疑问
# 如果要换用伽马分布　只要把　family 的 poisson 换成 gamma 即可
fit <- glm(formula = , data = testD?, family = poisson())

# 由于在泊松回归中，因变量以条件均值的对数形式　ln(λ) 建模，所以为了直观表明回归系数，把系数指数化
# 可得截距和相关预测变量的系数，具体意义为：预测变量增加１，则因变量将乘以预测变量的系数变化（若不指数化那么系数对因变量的反映为对数形式）
exp(coef(fit))

# 检验过度离势
# 可以使用qcc包
install.packages('qcc')
library('qcc')

# 如果返回的p值小于0.05则说明存在过度离势
qcc.overdispersion.test(testD$predictData, type = "poisson")

# 如果存在过度离势，那么可以使用类泊松代替泊松进行拟合
fit.od <- glm(formula = , data = testD, family = "quasipoisson")




# # ----------------- 参考资料 ----------------------------
# - 《R语言核心技术手册》 Joseph Adler                                          p.s.函数参数解释很详细
# - 《R语言实战》 Robert I.Kabacoff                                           p.s.时间序列讲得很详细
#        "平稳性一般可以通过时序图直观判断（p.s.或者tseries包的adf.test()）。
#        如果方差不是常数，则需要对数据做变换（p.s.box-cox之类）；
#        如果数据中存在趋势项，则需要对其进行差分（p.s. 可以使用ndiffs()自动判断需要差分阶数）。"
#        ☆☆　第１３章广义线性模型　☆☆
#  - 《Ｒ语言：实用数据分析和可视化技术》 Jared P.Rander                          p.s.对acf和pacf的解释比较浅显易懂
#  - 《Ｒ语言数据分析与挖掘实战》  张良均，云伟标，王路，刘晓勇                      p.s.对平稳性检验解释详细
#  - 《数据科学中的R语言》 李舰，肖凯                                            p.s.同上，也讲到了forecast包的BoxCox.lambda
