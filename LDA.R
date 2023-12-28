library(openxlsx)
library(dplyr)
library(MASS)

rm(list = ls())

OSUHCB <- read.xlsx("ruvcorrected_merge.xlsx")
rownames(OSUHCB) <- OSUHCB[1,]
OSUHCB <- OSUHCB[-1,]
rownames(OSUHCB) <- OSUHCB[,1]
OSUHCB <- OSUHCB[,-1]
OSUHCB <- OSUHCB %>% mutate_if(is.character, as.numeric)

clinical <- read.xlsx("clinical_OSU HCB merge.xlsx")
rownames(clinical) <- make.unique(clinical$y)
cli <- as.data.frame(clinical$Type, rowNames=T)
rownames(cli)<- rownames(clinical)

OSUHCB <- merge(OSUHCB, cli, by=0)
rownames(OSUHCB) <- OSUHCB$Row.names
OSUHCB <- OSUHCB[,-1]
OSUHCB$Type <-OSUHCB$`clinical$Type`
OSUHCB$`clinical$Type` <- NULL
#code for my discriminant analysis
hab.lda <- lda(Type ~ ., data=OSUHCB)

hab.lda.values <- predict(hab.lda, OSUHCB)
hab.class <- predict(hab.lda)$class


#create a histogram of the discriminant function values
#ldahist(data = hab.lda.values$x[,1], g=class)

#create a scatterplot of the discriminant function values
plot(hab.lda.values$x[,1], type="n", ylab=c("LDA Axis 1"), ylim = c(-10, 10))
text(hab.lda.values$x[,1], rownames(OSUHCB), col=c(as.numeric(hab.class)+10))
abline(v=0, lty="dotted")
abline(h=0, lty="dotted")



####jose playing with data
jose_df <- OSUHCB
head(jose_df)
jose_df2 <- data.frame()
for(i in 1:1000) {
  jose_df2 <- rbind(jose_df2, jose_df[4,])
}
range(OSUHCB$CDK7)
jose_df2$CDK7 <- seq(from = min(OSUHCB$CDK7 ), to = max(OSUHCB$CDK7 ), length.out = nrow(jose_df2))

jose_hdac3_predict <- predict(hab.lda, jose_df2)
jose_hdac3_predict$posterior

plot(y = jose_hdac3_predict$posterior[,2], x = jose_df2$CDK7, ylab = "OSU Prob", xlab = "CDK7", las = 1
)

t.test(MGMT ~ Type, data = jose_df)
plot(y = jose_hdac3_predict)

jose_scale <- hab.lda$scaling %>% as.data.frame
jose_scale$Gene <- rownames(jose_scale)
barplot(jose_scale$LD1 ~jose_scale$Gene, las= 2)


