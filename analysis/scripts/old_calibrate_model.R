load('output/BT_paramsweep.RData')

id = which(result[,2]<8 & result[,3]>50 & result[,3]<250)

summary(P[id,])

# V1              V2              V3               V4              V5                 V6                V7               V8        
# Min.   :1.719   Min.   :3.013   Min.   :0.2031   Min.   :4.010   Min.   :0.002815   Min.   :0.01987   Min.   :0.7508   Min.   :0.7504  
# 1st Qu.:2.892   1st Qu.:3.621   1st Qu.:0.3146   1st Qu.:4.922   1st Qu.:0.126518   1st Qu.:0.12260   1st Qu.:0.8038   1st Qu.:0.7977  
# Median :3.575   Median :4.353   Median :0.4181   Median :5.878   Median :0.240098   Median :0.16277   Median :0.8508   Median :0.8512  
# Mean   :3.691   Mean   :4.371   Mean   :0.4105   Mean   :5.933   Mean   :0.251896   Mean   :0.15858   Mean   :0.8538   Mean   :0.8516  
# 3rd Qu.:4.483   3rd Qu.:5.074   3rd Qu.:0.5071   3rd Qu.:6.841   3rd Qu.:0.388317   3rd Qu.:0.20241   3rd Qu.:0.9043   3rd Qu.:0.9049  
# Max.   :5.992   Max.   :5.986   Max.   :0.5992   Max.   :7.994   Max.   :0.499900   Max.   :0.24835   Max.   :0.9499   Max.   :0.9494  
# 
# V9              V10              V11              V12             V13               V14                V15                V16     
# Min.   : 1.580   Min.   :0.1003   Min.   :0.3002   Min.   :30.19   Min.   :0.05194   Min.   :0.005045   Min.   :0.001159   Min.   : NA  
# 1st Qu.: 8.926   1st Qu.:0.2518   1st Qu.:0.4266   1st Qu.:40.65   1st Qu.:0.13732   1st Qu.:0.048655   1st Qu.:0.027058   1st Qu.: NA  
# Median :13.040   Median :0.3609   Median :0.5377   Median :49.69   Median :0.22996   Median :0.098012   Median :0.048939   Median : NA  
# Mean   :12.514   Mean   :0.3588   Mean   :0.5468   Mean   :50.27   Mean   :0.22358   Mean   :0.098145   Mean   :0.049985   Mean   :NaN  
# 3rd Qu.:16.764   3rd Qu.:0.4668   3rd Qu.:0.6632   3rd Qu.:60.48   3rd Qu.:0.30859   3rd Qu.:0.147339   3rd Qu.:0.074956   3rd Qu.: NA  
# Max.   :19.953   Max.   :0.5957   Max.   :0.7972   Max.   :69.58   Max.   :0.39950   Max.   :0.199351   Max.   :0.099502   Max.   : NA  
# NA's   :209