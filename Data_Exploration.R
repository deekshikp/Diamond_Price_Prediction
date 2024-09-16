library(ggplot2)

#Basic Statistics

summary(diamonds)

#Specific statistics for the price of the diamonds
#Mean
mean=mean(diamonds$price)

#Median
median=median(diamonds$price)

#Minimum
minimum=min(diamonds$price)

#Maximum
maximum=max(diamonds$price)

#Top 5 high prices
head(sort(diamonds$price, decreasing = TRUE), n=5)

#Top 5 low prices
head(sort(diamonds$price, decreasing = FALSE), n=5)

#Price distribution of diamonds
ggplot(data=diamonds, aes(x=price,y="" )) + geom_point()

#Univariate Analysis
# price
ggplot(aes(price), data=diamonds) +
  geom_histogram(aes(y=..density..),
                 color="green",
                 fill="darkgreen") +
  ggtitle('price')

#Carat
ggplot(aes(carat), data=diamonds) +
  geom_histogram(aes(y=..density..),
                 color="green",
                 fill="darkgreen") +
  ggtitle('carat')

# depth
ggplot(aes(depth), data=diamonds) +
  geom_histogram(aes(y=..density..),
                 color="green",
                 fill="darkgreen") +
  ggtitle('depth')

#Bivariate Analysis
# price vs carat
price_carat <- ggplot(aes(x=carat, y=price), data=clean) +
  geom_point(fill=I("#f77a20"), color=I("black"), shape=21) +
  stat_smooth(method="lm") +
  scale_x_continuous(lim = c(0, quantile(clean$carat, 0.99)) ) +
  scale_y_continuous(lim = c(0, quantile(clean$price, 0.99)) ) +
  ggtitle("price vs. carat") +
  theme(plot.title = element_text(hjust = 0.5))

# price vs depth
price_depth <- ggplot(aes(x=depth, y=price), data=clean) +
  geom_point(fill=I("#f77a20"), color=I("black"), shape=21) +
  stat_smooth(method="lm") +
  scale_x_continuous(lim = c(0, quantile(clean$depth, 0.99)) ) +
  scale_y_continuous(lim = c(0, quantile(clean$price, 0.99)) ) +
  ggtitle("price vs. depth") +
  theme(plot.title = element_text(hjust = 0.5))

# price vs table
price_table <- ggplot(aes(x=table, y=price), data=clean) +
  geom_point(fill=I("#f77a20"), color=I("black"), shape=21) +
  stat_smooth(method="lm") +
  scale_x_continuous(lim = c(0, quantile(clean$table, 0.99)) ) +
  scale_y_continuous(lim = c(0, quantile(clean$price, 0.99)) ) +
  ggtitle("price vs. table") +
  theme(plot.title = element_text(hjust = 0.5))

# price vs x
price_x <- ggplot(aes(x=x, y=price), data=clean) +
  geom_point(fill=I("#f77a20"), color=I("black"), shape=21) +
  stat_smooth(method="lm") +
  scale_x_continuous(lim = c(0, quantile(clean$x, 0.99)) ) +
  scale_y_continuous(lim = c(0, quantile(clean$price, 0.99)) ) +
  ggtitle("price vs. x") +
  theme(plot.title = element_text(hjust = 0.5))

# price vs y
price_y <- ggplot(aes(x=y, y=price), data=clean) +
  geom_point(fill=I("#f77a20"), color=I("black"), shape=21) +
  stat_smooth(method="lm") +
  scale_x_continuous(lim = c(0, quantile(clean$y, 0.99)) ) +
  scale_y_continuous(lim = c(0, quantile(clean$price, 0.99)) ) +
  ggtitle("price vs. y") +
  theme(plot.title = element_text(hjust = 0.5))

# price vs z
price_z <- ggplot(aes(x=z, y=price), data=clean) +
  geom_point(fill=I("#f77a20"), color=I("black"), shape=21) +
  stat_smooth(method="lm") +
  scale_x_continuous(lim = c(0, quantile(clean$z, 0.99)) ) +
  scale_y_continuous(lim = c(0, quantile(clean$price, 0.99)) ) +
  ggtitle("price vs. z") +
  theme(plot.title = element_text(hjust = 0.5))

# plot pairwise scatter plot in one page
grid.arrange(arrangeGrob(price_carat, 
                         price_depth, price_table, 
                         ncol=2, nrow=2, 
                         layout_matrix=rbind(c(1,1), c(2,3))),
             arrangeGrob(price_x, price_y, price_z, 
                         ncol=1, nrow=3), ncol=2)

