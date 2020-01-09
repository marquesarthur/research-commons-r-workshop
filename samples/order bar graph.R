library(ggplot2)
library(stringr)


convert$neighbourhood <- str_replace_all(convert$neighbourhood, "[\r\n]" , "")


p <- ggplot(convert, #dataframe
       aes(x=reorder(neighbourhood, -count), # will show neighbourhood ordering by decreasing y (decreasing = -count, increasing = count)
           y=count, 
           fill=factor(type,levels=c("participant","garden"))) # fill by type (levels determine the order of the bars in the chart)
       ) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(
    limits=c(0, max(convert$count) + 1), # defines the minimum and maximum value in the Y axis
    breaks = seq(0, max(convert$count) + 1, by = 10) # defines how many numbers the Y axis will have. e.g. by 10 means that Y starts at 0 and increments by multiples of 10
  ) +
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1)) + # rotates the labels in the X axis
  labs(fill = "Type", x="neighbourhood", y="number of responses") + # provides the names of each axis
  scale_fill_manual(values = c("cornflowerblue", "orange"))   # defines the colors for each bar. color names available at : https://www.google.com/url?sa=i&rct=j&q=&esrc=s&source=images&cd=&ved=2ahUKEwirj--0x_fmAhVVHjQIHbTYDjsQjRx6BAgBEAQ&url=https%3A%2F%2Fwww.datanovia.com%2Fen%2Fblog%2Fawesome-list-of-657-r-color-names%2F&psig=AOvVaw1kYBcffgEGE73eGkvR2tlf&ust=1578695027993723
  

p