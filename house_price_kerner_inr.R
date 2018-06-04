require(ggplot2) # for data visualization
require(stringr) #extracting string patterns
require(Matrix) # matrix transformations
require(glmnet) # ridge, lasso & elastinet
require(xgboost) # gbm
require(randomForest)
require(Metrics) # rmse
require(dplyr) # load this in last so plyr doens't overlap it
require(caret) # one hot encoding
require(scales) # plotting $$
require(e1071) # skewness
require(corrplot) # correlation plot
require(magrittr) # function '%>%'
library(dplyr) # function 'group by'

  # Data load
setwd('C:/Users/KANG/Desktop/House_price')
train <- read.csv('train.csv', stringsAsFactors = F)
test <- read.csv('test.csv', stringsAsFactors = F)

  # combine train + test for data processing
df.combined <- rbind(within(train, rm('Id','SalePrice')), within(test, rm('Id')))
dim(df.combined)

############## 1. NA 처리 ###############
na.cols <- which(colSums(is.na(df.combined)) > 0)
sort(colSums(sapply(df.combined[na.cols], is.na)), decreasing = TRUE)

  # 시각화
plot.categoric <- function(cols, df){
  for(col in cols){
    order.cols <- names(sort(table(df.combined[,col]), decreasing =TRUE))
    
    num.plot <- qplot(df[,col]) +
      geom_bar(fill = 'cornflowerblue') +
      geom_text(aes(label = ..count..), stat ='count', vjust=-0.5) +
      theme_minimal() +
      scale_y_continuous(limits = c(0, max(table(df[,col]))*1.1)) +
      scale_x_discrete(limits = order.cols) +
      xlab(col) +
      theme(axis.text.x = element_text(angle = 30, size=12))
    
    print(num.plot)
  }
}

  # PoolQC Na 처리

#대부분의 집은 pool이 존재하지 않음
plot.categoric('PoolQC', df.combined) 

# pool의 면적>0 이면서 Pool quality가 NA인 데이터는 결측으로 봄
df.combined[(df.combined$PoolArea >0) & is.na(df.combined$PoolQC), c('PoolQC', 'PoolArea')] 

# pool 면적에 맞춰서 pool quality에 class 부여
df.combined[,c('PoolQC', 'PoolArea')] %>% 
  group_by(PoolQC) %>%
  summarise(mean = mean(PoolArea), counts = n())

df.combined[2421,'PoolQC'] = 'Ex'
df.combined[2504,'PoolQC'] = 'Ex'
df.combined[2600,'PoolQC'] = 'Fa'
df.combined$PoolQC[is.na(df.combined$PoolQC)] = 'None'

  # Garage NA 처리

length(which(df.combined$GarageYrBlt == df.combined$YearBuilt))
# 차고제작년도 = 집건축연도 (대부분 집지을때 차고도 같이 지어졌다)

idx <- which(is.na(df.combined$GarageYrBlt))
df.combined[idx, 'GarageYrBlt'] <- df.combined[idx, 'YearBuilt']
#차고 제작년도의 NA값은 집건축 연도로 대체

garage.cols <- c('GarageArea', 'GarageCars', 'GarageQual', 'GarageFinish', 'GarageCond', 'GarageType')
df.combined[is.na(df.combined$GarageCond),garage.cols]
#나머지 6개 Garage관련 변수 확인
#2127 데이터의 garage면적이 360이고, 수용차량이 1대 이지만 나머지 garage 4개 변수는 NA(결측으로본다)

idx <- which(((df.combined$GarageArea < 370) & (df.combined$GarageArea > 350)) & (df.combined$GarageCars == 1))

names(sapply(df.combined[idx, garage.cols], function(x) sort(table(x), decreasing=TRUE)[1]))

df.combined[2127,'GarageQual'] = 'TA'
df.combined[2127, 'GarageFinish'] = 'Unf'
df.combined[2127, 'GarageCond'] = 'TA'
#2127 데이터와 같은 garage면적과 수용차량이 1대인 데이터들을 확인
#2127행의 NA값을 같은 조건의 garage변수 빈도수 기준으로 처리

for (col in garage.cols){
  if (sapply(df.combined[col], is.numeric) == TRUE){
    df.combined[sapply(df.combined[col], is.na), col] = 0
  }
  else{
    df.combined[sapply(df.combined[col], is.na), col] = 'None'
  }
}
#Garage변수가 numeric일 때 NA = 0
#Garage변수가 Character일 때 NA = 'None'

  # Kitchen, Electric 변수 NA 처리 

plot.categoric('KitchenQual', df.combined)

df.combined$KitchenQual[is.na(df.combined$KitchenQual)] = 'TA'
#빈도수 기준 NA 대체

plot.categoric('Electrical', df.combined)

df.combined$Electrical[is.na(df.combined$Electrical)] = 'SBrkr'
#빈도수 기준 NA 대체

  # Bsmt(Basement)관련 변수(11개)

bsmt.cols <- names(df.combined)[sapply(names(df.combined), function(x) str_detect(x, 'Bsmt'))]
df.combined[is.na(df.combined$BsmtExposure),bsmt.cols]

plot.categoric('BsmtExposure', df.combined)
#대부분 BsmtExposure은 없다(No)

df.combined[c(949, 1488, 2349), 'BsmtExposure'] = 'No'
#949, 1488, 2349데이터의 BsmtExposure NA값은 'No'로 처리

for (col in bsmt.cols){
  if (sapply(df.combined[col], is.numeric) == TRUE){
    df.combined[sapply(df.combined[col], is.na),col] = 0
  }
  else{
    df.combined[sapply(df.combined[col],is.na),col] = 'None'
  }
}
#나머지 bsmt관련 변수에 대해 numeric변수이면 NA를 0리 처리
#나머지 bsmt관련 변수에 대해 character변수이면 NA를 'None' 처리

  # Exterior1st
  # Exterior2nd 변수 NA

idx <- which(is.na(df.combined$Exterior1st) | is.na(df.combined$Exterior2nd))
df.combined[idx,c('Exterior1st', 'Exterior2nd')]
#Exterior데이터 중 하나라도 NA인 값 확인(2152 1개)

df.combined$Exterior1st[is.na(df.combined$Exterior1st)] = 'Other'
df.combined$Exterior2nd[is.na(df.combined$Exterior2nd)] = 'Other'
#다른 NA처리 방도가 없으므로 'other'로 처리

  # SaleType: Type of sale
  # Functional: Home functionality rating
  # Utilities: Type of utilities available 변수 NA

plot.categoric('SaleType', df.combined)
df.combined[is.na(df.combined$SaleType),c('SaleCondition')]

table(df.combined$SaleCondition, df.combined$SaleType)

df.combined$SaleType[is.na(df.combined$SaleType)] = 'WD'
#salecType의 NA값을 salecondition에 따른 saleType의 최빈값으로 대체

plot.categoric('Functional', df.combined)

df.combined$Functional[is.na(df.combined$Functional)] = 'Typ'
#functional은 단순 최빈값으로 대체

plot.categoric('Utilities', df.combined)

which(df.combined$Utilities == 'NoSeWa') # in the training data set

col.drops <- c('Utilities')
df.combined <- df.combined[,!names(df.combined) %in% c('Utilities')]
#Utilities는 train, test를 통틀어 1개의 데이터를 제외하고 Allpub이므로 변수 삭제

  # MSZoning: The general zoning classification
  # MSSubClass: The building class

df.combined[is.na(df.combined$MSZoning),c('MSZoning','MSSubClass')]

plot.categoric('MSZoning', df.combined)

table(df.combined$MSZoning, df.combined$MSSubClass)

df.combined$MSZoning[c(2217, 2905)] = 'RL'
df.combined$MSZoning[c(1916, 2251)] = 'RM'
#MSZoning, MSSubclass를 비교
#MSZoning의 NA를 MSSubclass에 따른 MSZoning의 최빈값으로 대체

  # MasVnrType: Masonry veneer type
  # MasVnrArea: Masonry veneer area in square feet

df.combined[(is.na(df.combined$MasVnrType)) | (is.na(df.combined$MasVnrArea)), c('MasVnrType', 'MasVnrArea')]
#MasVnrType의 NA는 24개, MasVnrArea NA는 23개
#1개의 NA차이는 2611데이터

na.omit(df.combined[,c('MasVnrType','MasVnrArea')]) %>%
  group_by(na.omit(MasVnrType)) %>%
  summarise(MedianArea = median(MasVnrArea,na.rm = TRUE), counts = n()) %>%
  arrange(MedianArea)

df.combined[2611, 'MasVnrType'] = 'BrkFace'
df.combined$MasVnrType[is.na(df.combined$MasVnrType)] = 'None'
df.combined$MasVnrArea[is.na(df.combined$MasVnrArea)] = 0
#2611행 MasVnrType는 BrkFace로 처리
#나머지 MasVnrType NA는'None'
#MasVnrArea NA는 0

  # LotFrontage: Linear feet of street connected to property

df.combined['Nbrh.factor'] <- factor(df.combined$Neighborhood, levels = unique(df.combined$Neighborhood))
lot.by.nbrh <- df.combined[,c('Neighborhood','LotFrontage')] %>%
  group_by(Neighborhood) %>%
  summarise(median = median(LotFrontage, na.rm = TRUE))
lot.by.nbrh

idx = which(is.na(df.combined$LotFrontage))

for (i in idx){
  lot.median <- lot.by.nbrh[lot.by.nbrh == df.combined$Neighborhood[i],'median']
  df.combined[i,'LotFrontage'] <- lot.median[[1]]
}
#LotFrontage는 neighborhood에 따라서 비슷한 경향을 보임
#LotFrontage의 NA를 class별 neighborhood의 중앙값으로 대체

  # Fence: Fence quality
  # MiscFeature

plot.categoric('Fence', df.combined)
df.combined$Fence[is.na(df.combined$Fence)] = 'None'
df.combined$MiscFeature[is.na(df.combined$MiscFeature)] = 'None'
#NA와 관련된 class가 없으므로 'None'으로 처리

  # Fireplaces: Number of fireplaces
  # FireplaceQu: Fireplace quality

plot.categoric('FireplaceQu', df.combined)
which((df.combined$Fireplaces > 0) & (is.na(df.combined$FireplaceQu)))

df.combined$FireplaceQu[is.na(df.combined$FireplaceQu)] = 'None'
#fireplaceQu가 없으면 'none'으로 처리

  # Alley: Type of alley access

plot.categoric('Alley', df.combined)

df.combined$Alley[is.na(df.combined$Alley)] = 'None'
#Alley NA는 None으로 처리
paste('There are', sum(sapply(df.combined, is.na)), 'missing values list')

############# NA 처리 끝 ###############

############# Adding custom numeric features ###################

  # categoric변수들을 순서형 numeric으로
  # numeric변수가 정보손실을 최소화 시키기 때문
  # categoric -> numeric(one-hot encoding)

# numeric자료형, character자료형 분리
num_features <- names(which(sapply(df.combined, is.numeric)))
cat_features <- names(which(sapply(df.combined, is.character)))

df.numeric <- df.combined[num_features]

group.df <- df.combined[1:1460,]
group.df$SalePrice <- train$SalePrice

# function that groups a column by its features and returns the mdedian saleprice for each unique feature. 
group.prices <- function(col) {
  group.table <- group.df[,c(col, 'SalePrice', 'OverallQual')] %>%
    group_by_(col) %>%
    summarise(mean.Quality = round(mean(OverallQual),2),
              mean.Price = mean(SalePrice), n = n()) %>%
    arrange(mean.Quality)
  
  print(qplot(x=reorder(group.table[[col]], -group.table[['mean.Price']]), y=group.table[['mean.Price']]) +
          geom_bar(stat='identity', fill='cornflowerblue') +
          theme_minimal() +
          scale_y_continuous(labels = dollar) +
          labs(x=col, y='Mean SalePrice') +
          theme(axis.text.x = element_text(angle = 45)))
  
  return(data.frame(group.table))
}

## functional to compute the mean overall quality for each quality
quality.mean <- function(col) {
  group.table <- df.combined[,c(col, 'OverallQual')] %>%
    group_by_(col) %>%
    summarise(mean.qual = mean(OverallQual)) %>%
    arrange(mean.qual)
  
  return(data.frame(group.table))
}


# function that maps a categoric value to its corresponding numeric value and returns that column to the data frame
map.fcn <- function(cols, map.list, df){
  for (col in cols){
    df[col] <- as.numeric(map.list[df.combined[,col]])
  }
  
  return(df)
}

  # 8개 변수의 class를 순서를 갖는 numeric으로 변경
  # map.fcn 함수를 이용하여 numeric.df에 새로운 column으로 붙임
qual.cols <- c('ExterQual', 'ExterCond', 'GarageQual', 'GarageCond', 'FireplaceQu', 'KitchenQual', 'HeatingQC', 'BsmtQual')
group.prices('FireplaceQu')
group.prices('BsmtQual')
group.prices('KitchenQual')
qual.list <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
df.numeric <- map.fcn(qual.cols, qual.list, df.numeric)
bsmt.list <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)
df.numeric = map.fcn(c('BsmtExposure'), bsmt.list, df.numeric)

#visualization for BsmtFinTyp2 instead of another table
df.combined[,c('BsmtFinType1', 'BsmtFinSF1')] %>%
  group_by(BsmtFinType1) %>%
  summarise(medianArea = median(BsmtFinSF1), counts = n()) %>%
  arrange(medianArea) %>%
  ggplot(aes(x=reorder(BsmtFinType1,-medianArea), y=medianArea)) +
  geom_bar(stat = 'identity', fill='cornflowerblue') +
  labs(x='BsmtFinType2', y='Median of BsmtFinSF2') +
  geom_text(aes(label = sort(medianArea)), vjust = -0.5) +
  scale_y_continuous(limits = c(0,850)) +
  theme_minimal()

#BsmtFinType2와 그에 따른 area의 median값을 확인, 높은 순서대로 높은 등급 부여
bsmt.fin.list <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2,'Rec'= 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
df.numeric <- map.fcn(c('BsmtFinType1','BsmtFinType2'), bsmt.fin.list, df.numeric)

  # Functional, GarageFinish, Fence, MSdwelling의 class별 mean saleprice확인, 높은여 순서대로 높은 등급 부여
group.prices('Functional')
functional.list <- c('None' = 0, 'Sal' = 1, 'Sev' = 2, 'Maj2' = 3, 'Maj1' = 4, 'Mod' = 5, 'Min2' = 6, 'Min1' = 7, 'Typ'= 8)
df.numeric['Functional'] <- as.numeric(functional.list[df.combined$Functional])
group.prices('GarageFinish')
garage.fin.list <- c('None' = 0,'Unf' = 1, 'RFn' = 1, 'Fin' = 2)
df.numeric['GarageFinish'] <- as.numeric(garage.fin.list[df.combined$GarageFinish])
group.prices('Fence')
fence.list <- c('None' = 0, 'MnWw' = 1, 'GdWo' = 1, 'MnPrv' = 2, 'GdPrv' = 4)
df.numeric['Fence'] <- as.numeric(fence.list[df.combined$Fence])
MSdwelling.list <- c('20' = 1, '30'= 0, '40' = 0, '45' = 0,'50' = 0, '60' = 1, '70' = 0, '75' = 0, '80' = 0, '85' = 0, '90' = 0, '120' = 1, '150' = 0, '160' = 0, '180' = 0, '190' = 0)
df.numeric['NewerDwelling'] <- as.numeric(MSdwelling.list[as.character(df.combined$MSSubClass)])

  # 상관계수 확인
  # 0.5 이상이거나, -0.5이하의 관계를 갖는 것만 확인
corr.df <- cbind(df.numeric[1:1460,], train['SalePrice'])
correlations <- cor(corr.df)
corr.SalePrice <- as.matrix(sort(correlations[,'SalePrice'], decreasing = T))

corr.idx <- names(which(apply(corr.SalePrice, 1, function(x) (x > 0.5 | x < -0.5))))
corrplot(as.matrix(correlations[corr.idx,corr.idx]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)

  # binary
  # 그래프 확인, 최빈값 1, 나머지 0
  # 데이터 특성으로 고려하여 1, 0을 설정

plot.categoric('LotShape', df.combined)
df.numeric['RegularLotShape'] <- (df.combined$LotShape == 'Reg') * 1
plot.categoric('LandContour', df.combined)
df.numeric['LandLeveled'] <- (df.combined$LandContour == 'Lvl') * 1
plot.categoric('LandSlope', df.combined)
df.numeric['LandSlopeGentle'] <- (df.combined$LandSlope == 'Gtl') * 1
plot.categoric('Electrical', df.combined)
df.numeric['ElectricalSB'] <- (df.combined$Electrical == 'SBrkr') * 1
plot.categoric('GarageType', df.combined)
df.numeric['GarageDetchd'] <- (df.combined$GarageType == 'Detchd') * 1

plot.categoric('PavedDrive', df.combined)
df.numeric['HasPavedDrive'] <- (df.combined$PavedDrive == 'Y') * 1
df.numeric['HasWoodDeck'] <- (df.combined$WoodDeckSF > 0) * 1
df.numeric['Has2ndFlr'] <- (df.combined$X2ndFlrSF > 0) * 1
df.numeric['HasMasVnr'] <- (df.combined$MasVnrArea > 0) * 1

plot.categoric('MiscFeature', df.combined)
df.numeric['HasShed'] <- (df.combined$MiscFeature == 'Shed') * 1

#많은 집들이 건축연도와 리모델 연도가 같기 때문에 두개 연도가 같지 않으면 1, 같으면 0
#실제 리모델 여부의 변수를 추가함
df.numeric['Remodeled'] <- (df.combined$YearBuilt != df.combined$YearRemodAdd) * 1

#최근에 리모델 된 집이 팔린 연도보다 늦어지면 1 아니면 0
df.numeric['RecentRemodel'] <- (df.combined$YearRemodAdd >= df.combined$YrSold) * 1

#건축 연도와 팔린 연도가 같으면 1, 아니면 0(집의 인기도 파악)
df.numeric['NewHouse'] <- (df.combined$YearBuilt == df.combined$YrSold) * 1


  # one-hot encoding

#면적 관련 변수들이 0이면 해당 변수들의 특징이 없는 집이라 판단
#one-hot encoding
#cols.binary <- c('X2ndFlrSF', 'MasVnrArea', 'WoodDeckSF')
cols.binary <- c('X2ndFlrSF', 'MasVnrArea', 'WoodDeckSF', 'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch')

for (col in cols.binary){
  df.numeric[str_c('Has',col)] <- (df.combined[,col] != 0) * 1
}

#집이 팔린 달(hot season)을 보고 싶음
#5,6,7월에 가장 많이 팔림, 이 달은 1, 아니면 0
ggplot(df.combined, aes(x=MoSold)) +
  geom_bar(fill = 'cornflowerblue') +
  geom_text(aes(label=..count..), stat='count', vjust = -.5) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:12)

df.numeric['HighSeason'] <- (df.combined$MoSold %in% c(5,6,7)) * 1

#Neighborhood, Median Price 관계 확인
train[,c('Neighborhood','SalePrice')] %>%
  group_by(Neighborhood) %>%
  summarise(median.price = median(SalePrice, na.rm = TRUE)) %>%
  arrange(median.price) %>%
  mutate(nhbr.sorted = factor(Neighborhood, levels=Neighborhood)) %>%
  ggplot(aes(x=nhbr.sorted, y=median.price)) +
  geom_point() +
  geom_text(aes(label = median.price, angle = 45), vjust = 2) +
  theme_minimal() +
  labs(x='Neighborhood', y='Median price') +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=45))

#'StoneBr', 'NoRidge','NridgHt' nrbh가 가장 높은 SalePrice를 보였음(rich.nbrh)
#rich.nbrh와 나머지 nbrh의 GrLivArea, SalePrice를 확인 
#rich.nbrh이면 1, 나머지 0
other.nbrh <- unique(df.combined$Neighborhood)[!unique(df.combined$Neighborhood) %in% c('StoneBr', 'NoRidge','NridgHt')]

nbrh.rich <- c('Crawfor', 'Somerst, Timber', 'StoneBr', 'NoRidge', 'NridgeHt')
df.numeric['NbrhRich'] <- (df.combined$Neighborhood %in% nbrh.rich) *1

#Neighborhood의 그래프를 보고 nbrh에 따른 순서형 numeric으로 변환
group.prices('Neighborhood')

nbrh.map <- c('MeadowV' = 0, 'IDOTRR' = 1, 'Sawyer' = 1, 'BrDale' = 1, 'OldTown' = 1, 'Edwards' = 1, 
              'BrkSide' = 1, 'Blueste' = 1, 'SWISU' = 2, 'NAmes' = 2, 'NPkVill' = 2, 'Mitchel' = 2,
              'SawyerW' = 2, 'Gilbert' = 2, 'NWAmes' = 2, 'Blmngtn' = 2, 'CollgCr' = 2, 'ClearCr' = 3, 
              'Crawfor' = 3, 'Veenker' = 3, 'Somerst' = 3, 'Timber' = 3, 'StoneBr' = 4, 'NoRidge' = 4, 
              'NridgHt' = 4)
df.numeric['NeighborhoodBin'] <- as.numeric(nbrh.map[df.combined$Neighborhood])

#SaleCondition : 최빈class 1, 나머지 0
group.prices('SaleCondition')
df.numeric['PartialPlan'] <- (df.combined$SaleCondition == 'Partial') * 1

#HeatingQC : class별 mean Saleprice가 높은 순서대로 높은 등급 부여
group.prices('HeatingQC')
heating.list <- c('Po' = 0, 'Fa' = 1, 'TA' = 2, 'Gd' = 3, 'Ex' = 4)
df.numeric['HeatingScale'] <- as.numeric(heating.list[df.combined$HeatingQC])

#1stFlrsf + 2ndFlrsf + LowQualFinSF = GrLivArea
#모든 면적 관련 함수의 합 : TotalArea생성
area.cols <- c('LotFrontage', 'LotArea', 'MasVnrArea', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF',
               'TotalBsmtSF', 'X1stFlrSF', 'X2ndFlrSF', 'GrLivArea', 'GarageArea', 'WoodDeckSF', 
               'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch', 'LowQualFinSF', 'PoolArea')

df.numeric['TotalArea'] <- as.numeric(rowSums(df.combined[,area.cols]))

#1stFlrsf + 2ndFlrsf 변수 : AreaInside 변수 생성
df.numeric['AreaInside'] <- as.numeric(df.combined$X1stFlrSF + df.combined$X2ndFlrSF)

#위에서 최근에 지어질수록 집값에 영향을 크게 미치기 때문에 2010기준 최근성 변수 생성 : Age
df.numeric['Age'] <- as.numeric(2010 - df.combined$YearBuilt)
df.numeric['TimeSinceSold'] <- as.numeric(2010 - df.combined$YrSold)

# how many years since the house was remodelled and sold 
df.numeric['YearSinceRemodel'] <- as.numeric(df.combined$YrSold - df.combined$YearRemodAdd)

#OverallQual변수와 모든 변수와의 상관관계 확인

corr.OverallQual <- as.matrix(sort(correlations[,'OverallQual'], decreasing = TRUE))

corr.idx <- names(which(apply(corr.OverallQual, 1, function(x) (x > 0.5 | x < -0.5))))

corrplot(as.matrix(correlations[corr.idx, corr.idx]), type = 'upper',
         method = 'color', addCoef.col = 'black', tl.cex =.7, cl.cex = .7,
         number.cex = .7)

#PCA를 하면 좋겠으나 커널에서는 생략

  # Outliers

train.test.df <- rbind(dplyr::select(train,-SalePrice), test)
train.test.df$type <- c(rep('train',1460),rep('test',1459))

#GrLivArea의 면적별 빈도수 확인, 이상치의 존재여부 확인
#GrLivArea의 면적이 4000 이상이면 이상치로 판단
ggplot(train, aes(x=GrLivArea)) +
  geom_histogram(fill='lightblue',color='white') +
  theme_minimal()

outlier_values <- boxplot.stats(train$GrLivArea)$out  # outlier values.
boxplot(train$GrLivArea, main="GrLivArea", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values[outlier_values>4000], collapse=", ")), cex=0.6)

ggplot(train.test.df, aes(x=type, y=GrLivArea, fill=type)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(breaks = c("test", "train"), values = c("indianred", "lightblue"))

#train, test의 GrLivArea box-plot확인 결과 train:3, test:1개의 이상치가 존재
#train에서 이러한 이상치는 가치의 효용을 증가시키지 못함
#왜도를 유발하며, 4000이상의 이상치는 SalePrice와의 상관계수를 떨어뜨림 : 이상치 제거
idx.outliers <- which(train$GrLivArea > 4000)
df.numeric <- df.numeric[!1:nrow(df.numeric) %in% idx.outliers,]
df.combined <- df.combined[!1:nrow(df.combined) %in% idx.outliers,]

require(psych)
# linear models assume normality from dependant variables 
# transform any skewed data into normal
skewed <- apply(df.numeric, 2, skewness)
skewed <- skewed[(skewed > 0.8) | (skewed < -0.8)]

kurtosis <- apply(df.numeric, 2, kurtosi)
kurtosis <- kurtosis[(kurtosis > 3.0) | (kurtosis < -3.0)]

# not very useful in our case
ks.p.val <- NULL
for (i in 1:length(df.numeric)) {
  test.stat <- ks.test(df.numeric[i], rnorm(1000))
  ks.p.val[i] <- test.stat$p.value
}

for(col in names(skewed)){
  if(0 %in% df.numeric[, col]) {
    df.numeric[,col] <- log(1+df.numeric[,col])
  }
  else {
    df.numeric[,col] <- log(df.numeric[,col])
  }
}

scaler <- preProcess(df.numeric)
df.numeric <- predict(scaler, df.numeric)

#character변수들 dummy화
dummy <- dummyVars(" ~ .",data=df.combined[,cat_features])
df.categoric <- data.frame(predict(dummy,newdata=df.combined[,cat_features]))

#SalePrice와 상관계수가 높은 3개(GarageYrBlt, YearBuilt, YearRemodAdd)
#연도관련 함수 생성 : 20년 기준으로 연도 변수 등급 부여(1~7)
year.map = function(col.combined, col.name) {
  for (i in 1:7) {
    year.seq = seq(1871+(i-1)*20, 1871+i*20-1)
    idx = which(df.combined[,col.combined] %in% year.seq)
    df.categoric[idx,col.name] = i
  }
  return(df.categoric)
}

#3개 변수에대한 새로운 이름의 변수 생성(Bin변수 생성)
df.categoric['GarageYrBltBin'] = 0
df.categoric <- year.map('GarageYrBlt', 'GarageYrBltBin')
df.categoric['YearBuiltBin'] = 0
df.categoric <- year.map('YearBuilt','YearBuiltBin')
df.categoric['YearRemodAddBin'] = 0
df.categoric <- year.map('YearRemodAdd', 'YearRemodAddBin')

bin.cols <- c('GarageYrBltBin', 'YearBuiltBin', 'YearRemodAddBin')
for (col in bin.cols) {
  df.categoric <- cbind(df.categoric, model.matrix(~.-1, df.categoric[col]))
}

df.categoric <- df.categoric[,!names(df.categoric) %in% bin.cols]

#numeric 데이터셋, categoric데이터셋 합침
df <- cbind(df.numeric, df.categoric)

require(WVPlots)
y.true <- train$SalePrice[which(!1:1460 %in% idx.outliers)]

#SalePrice의 분포 정규성 확인
qplot(y.true, geom='density') +# +(train, aes(x=SalePrice)) +
  geom_histogram(aes(y=..density..), color='white', 
                 fill='lightblue', alpha=.5, bins = 60) +
  geom_line(aes(y=..density..), color='cornflowerblue', lwd = 1, stat = 'density') + 
  stat_function(fun = dnorm, colour = 'indianred', lwd = 1, args = 
                  list(mean(train$SalePrice), sd(train$SalePrice))) +
  scale_x_continuous(breaks = seq(0,800000,100000), labels = dollar) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  annotate('text', label = paste('skewness =', signif(skewness(train$SalePrice),4)),
           x=500000,y=7.5e-06)

qqnorm(train$SalePrice)
qqline(train$SalePrice)

#SalePrice에 skew취함 log(saleprice+1)
y_train <- log(y.true+1)

qplot(y_train, geom = 'density') +
  geom_histogram(aes(y=..density..), color = 'white', fill = 'lightblue', alpha = .5, bins = 60) +
  scale_x_continuous(breaks = seq(0,800000,100000), labels = comma) +
  geom_line(aes(y=..density..), color='dodgerblue4', lwd = 1, stat = 'density') + 
  stat_function(fun = dnorm, colour = 'indianred', lwd = 1, args = 
                  list(mean(y_train), sd(y_train))) +
  #scale_x_continuous(breaks = seq(0,800000,100000), labels = dollar) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  annotate('text', label = paste('skewness =', signif(skewness(y_train),4)),
           x=13,y=1) +
  labs(x = 'log(SalePrice + 1)') 

qqnorm(y_train)
qqline(y_train)

paste('The dataframe has', dim(df)[1], 'rows and', dim(df)[2], 'columns')

#과적합을 유발하는 near-zero-variance제거
nzv.data <- nearZeroVar(df, saveMetrics = TRUE)

# take any of the near-zero-variance perdictors
drop.cols <- rownames(nzv.data)[nzv.data$nzv == TRUE]

df <- df[,!names(df) %in% drop.cols]

  # Modeling
  # Xgboost

#train, test데이터 분리
x_train <- df[1:1456,]

x_test <- df[1457:nrow(df),]

#y skew취함
dtrain <- xgb.DMatrix(as.matrix(x_train), label = y_train) 

#y raw
dtrain_realY <- xgb.DMatrix(as.matrix(x_train), label =y.true) 

dtest <- xgb.DMatrix(as.matrix(x_test))

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 4, 
                        allowParallel=T)

#xgb.grid <- expand.grid(nrounds = 750,
#                        eta = c(0.01,0.005,0.001),
#                       max_depth = c(4,6,8),
#                       colsample_bytree=c(0,1,10),
#                      min_child_weight = 2,
#                     subsample=c(0,0.2,0.4,0.6),
#                    gamma=0.01)

set.seed(45)

#xgb_tune <- train(as.matrix(x_train),
#       y_train,
#      method="xgbTree",
#     trControl=cv.ctrl,
#    tuneGrid=xgb.grid,
#   verbose=T,
#  metric="RMSE",
# nthread =3)

xgb_params <- list(
  booster = 'gbtree',
  objective = 'reg:linear',
  colsample_bytree=1,
  eta=0.005,
  max_depth=4,
  min_child_weight=3,
  alpha=0.3,
  lambda=0.4,
  gamma=0.01, # less overfit
  subsample=0.6,
  seed=5,
  silent=TRUE)

#xgb.cv는 cross validation 하고 싶을 때
#bst.cv <- xgb.cv(xgb_params, dtrain, nrounds = 5000, nfold = 4, early_stopping_rounds = 500) 

## test_rmse기준으로 최소값을 찾다가 값이 올라가면 멈춘다. 최소점을 찾은 후 500개 더 해보고 없으면 스탑
bst <- xgb.train(xgb_params, dtrain, nrounds = 10000)
bst_realY <- xgb.train(xgb_params, dtrain_realY, nrounds =10000)

rmse_eval <- function(y.true, y.pred){
  mse_eval <- sum((y.true - exp(y.pred)-1)^2) / length(y.true)
  return(sqrt(mse_eval))
} # y skew했을 때 skew 풀고 rmse

rmse_eval1 <- function(y.true, y.pred){
  mse_eval <- sum((y.true - y.pred)^2) / length(y.true)
  return(sqrt(mse_eval))
} # 원래의 Rmse

#train 정확도 확인
# bst는 skew취한 y값으로 만든 모델, dtrain의 라벨은 skew한 집값(y)
y_pred.xgb <- predict(bst, dtrain) 
rmse_eval(y.true , y_pred.xgb)

# bst_y_true는 y raw로 만든 모델, dtrain_y.true라벨은 원래 집값
y_pred.xgb_realY <- predict(bst_realY, dtrain_realY) 
rmse_eval1(y.true , y_pred.xgb_realY)

model.names <- dimnames(dtrain)[[2]]

  # Regularizaion
  # ridge, Lasso, elastic-net

#these won't run on kaggle uncomment these in your own R-evn
glm.cv.ridge <- cv.glmnet(as.matrix(x_train), y_train, alpha=0)
glm.cv.lasso <- cv.glmnet(as.matrix(x_train), y_train, alpha=1)
glm.cv.net <- cv.glmnet(as.matrix(x_train), y_train, alpha=0.001)

#use the lambda that minimize the error
penalty.ridge <- glm.cv.ridge$lambda.min
penalty.lasso <- glm.cv.lasso$lambda.min
penalty.net <- glm.cv.net$lambda.min

#make models
glm.ridge <- glmnet(x=as.matrix(x_train), y=y_train, alpha = 0, lambda = penalty.ridge)
glm.lasso <- glmnet(x=as.matrix(x_train), y=y_train, alpha = 1, lambda = penalty.lasso)
glm.net <- glmnet(x=as.matrix(x_train), y=y_train, alpha = 0.001, lambda = penalty.net)

#predict train
y_pred.ridge <- as.numeric(predict(glm.ridge, as.matrix(x_train)))
y_pred.lasso <- as.numeric(predict(glm.lasso, as.matrix(x_train)))
y_pred.net <- as.numeric(predict(glm.net,as.matrix(x_train)))

#rmse eval
rmse_eval(y.true, y_pred.ridge)
rmse_eval(y.true, y_pred.lasso)
rmse_eval(y.true, y_pred.net)

y_pred_all <- (y_pred.lasso+y_pred.net+y_pred.ridge+y_pred.xgb )/4

rmse_eval(y.true , y_pred_all)

#model apply to test
y_pred.ridge <- as.double(predict(glm.ridge, as.matrix(x_test)))
y_pred.lasso <- as.double(predict(glm.lasso, as.matrix(x_test)))
y_pred.net <- as.double(predict(glm.net, as.matrix(x_test)))

y_pred.ridge <- as.double(exp(y_pred.ridge) - 1)
y_pred.lasso <- as.double(exp(y_pred.lasso) - 1)
y_pred.net <- as.double(exp(y_pred.net) - 1)

y_pred.xgb <- as.double(predict(bst, dtest))
y_pred.xgb <- as.double(exp(y_pred.xgb)-1)

#take the average of our predictions for our ensemble
y_pred <- (y_pred.ridge + y_pred.lasso + y_pred.net + y_pred.xgb) /4
head(y_pred)

submit_Yskew <- data.frame('ID'=test$Id, 'SalePrice'=as.numeric(y_pred))

write.csv(submit_Yskew, file = 'submit_Yskew.csv',row.names = F)
#------------------------------------------------------------------
# y = y.true

glm.cv.ridge_realY <- cv.glmnet(as.matrix(x_train), y=y.true, alpha=0)
glm.cv.lasso_realY <- cv.glmnet(as.matrix(x_train), y=y.true, alpha=1)
glm.cv.net_realY <- cv.glmnet(as.matrix(x_train), y=y.true, alpha=0.001)

penalty.ridge_realY <- glm.cv.ridge_realY$lambda.min
penalty.lasso_realY <- glm.cv.lasso_realY$lambda.min
penalty.net_realY <- glm.cv.net_realY$lambda.min

glm.ridge_realY <- glmnet(x=as.matrix(x_train), y=y.true, alpha = 0, lambda = penalty.ridge_realY)
glm.lasso_realY <- glmnet(x=as.matrix(x_train), y=y.true, alpha = 1, lambda = penalty.lasso_realY)
glm.net_realY <- glmnet(x=as.matrix(x_train), y=y.true, alpha = 0.001, lambda = penalty.net_realY)

y_pred.ridge_realY <- as.numeric(predict(glm.ridge_realY, as.matrix(x_train)))
y_pred.lasso_realY <- as.numeric(predict(glm.lasso_realY, as.matrix(x_train)))
y_pred.net_realY <- as.numeric(predict(glm.net_realY, as.matrix(x_train)))

rmse_eval1(y.true, y_pred.lasso_realY)
rmse_eval1(y.true, y_pred.ridge_realY)
rmse_eval1(y.true, y_pred.net_realY)

y_pred_all_realY <- (y_pred.ridge_realY+y_pred.lasso_realY+y_pred.net_realY+y_pred.xgb_realY)/4

rmse_eval1(y.true, y_pred_all_realY)

#model apply to test
y_pred.ridge_realY <- as.double(predict(glm.ridge_realY, as.matrix(x_test)))
y_pred.lasso_realY <- as.double(predict(glm.lasso_realY, as.matrix(x_test)))
y_pred.net_realY <- as.double(predict(glm.net_realY, as.matrix(x_test)))

y_pred.xgb_realY <- as.double(predict(bst_realY, dtest))

#take the average of our predictions for our ensemble
y_pred_realY <- (y_pred.ridge_realY + y_pred.lasso_realY + y_pred.net_realY + y_pred.xgb_realY) /4
head(y_pred_realY)

submit_Yraw <- data.frame('ID'=test$Id, 'SalePrice'=as.numeric(y_pred_realY))

write.csv(submit_Yraw, file = 'submit_Yraw.csv',row.names = F)
