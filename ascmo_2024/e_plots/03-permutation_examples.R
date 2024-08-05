library(ggplot2)
library(patchwork)

# plot to store plots
plot_dir = "./_plots"

set.seed(12)
x = 1:36
xpi = (1:36)/12 * 2 * pi
nsim = 2
y = 2 * sin(xpi) + 3 * cos(xpi) + rnorm(length(x))
ysim = sapply(seq_len(nsim), function (i) {
  y = 2 * sin(xpi) + 3 * cos(xpi) + rnorm(length(x))
})

# original data
df = data.frame(month = rep(x, nsim + 1), response = c(y, c(ysim)),
                group = c(rep("reanalysis", length(x)),
                          rep("model", length(x) * nsim)),
                observation = rep(seq_len(nsim + 1), each = length(x)),
                lwd = as.factor(rep(c(2, 1), times = c(length(x), length(x) * nsim))))
df$group = factor(df$group)
df$observation = factor(df$observation)

# create simple theme
simple =   theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black",
                                             size = 1,
                                             fill = NA),
                 legend.key = element_rect(fill = NA, color = NA))

a <- ggplot(df) + geom_line(aes(x = month, y = response, group = observation,
                           col = observation,
                           linetype = group),
                           show.legend = FALSE) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  ggtitle("(a) original data") +
  simple +
  theme(legend.position='bottom', legend.box = "horizontal") +
  scale_color_brewer(type = "qual", palette = "Dark2")
print(a)

# permute whole time series
dfp1 = df
dfp1$group = factor(c(rep("model", length(x) * 1),
                      rep("reanalysis", length(x)),
                      rep("model", length(x) * (nsim - 1))))
b <- ggplot(dfp1) + geom_line(aes(x = month, y = response, group = observation,
                                  col = observation,
                                  linetype = group), show.legend = FALSE) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  simple +
  ggtitle("(b) standard permutation") +
  theme(legend.position='bottom', legend.box = "horizontal") + 
  scale_color_brewer(type = "qual", palette = "Dark2")
print(b)

# permute within each year
dfp2 = df
dfp2$year = factor(rep(1:3, each = 12, times = nsim + 1))
# dfp2$oobservation = df$observation
s1 = c(2, 1, 3)
s2 = c(3, 2, 1)
s3 = c(1, 3, 2)
w1 = which(dfp2$year == 1)
split1 = split(w1, dfp2$observation[w1])
dfp2$group[w1] = dfp2$group[unlist(split1[s1])]
w2 = which(dfp2$year == 2)
split2 = split(w2, dfp2$observation[w2])
dfp2$group[w2] = dfp2$group[unlist(split2[s2])]
w3 = which(dfp2$year == 3)
split3 = split(w3, dfp2$observation[w3])
dfp2$group[w3] = dfp2$group[unlist(split3[s3])]

dfp2_1 = dfp2[dfp2$year == 1,]
dfp2_2 = dfp2[dfp2$year == 2,]
dfp2_3 = dfp2[dfp2$year == 3,]
# fix gaps
dfp2_1c = rbind(dfp2_1,
                dfp2_2[dfp2_2$month == 13,])
dfp2_2c = rbind(dfp2_2,
                dfp2_3[dfp2_3$month == 25,])
dfp2_1c$year = 1
dfp2_1c$group[38] = "reanalysis"
dfp2_1c$group[39] = "model"

dfp2_2c$year = 2
dfp2_2c$group[37] = "model"
dfp2_2c$group[39] = "reanalysis"

cee <- ggplot() +
  geom_line(data = dfp2_1c, 
            aes(x = month, y = response,
                group = observation,
                col = observation,
                linetype = group)) +
  geom_line(data = dfp2_2c, aes(x = month, y = response,
                                group = observation,
                                col = observation,
                                linetype = group), show.legend = FALSE) +
  geom_line(data = dfp2_3, aes(x = month, y = response, group = observation,
                             col = observation,
                             linetype = group), show.legend = FALSE) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  simple +
  ggtitle("(c) stratified permutation") +
  theme(legend.position='bottom', legend.box = "horizontal",
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1))) + 
  scale_color_brewer(type = "qual", palette = "Dark2")
  # +
  # geom_vline(xintercept = v)
print(cee)

pdf(file.path(plot_dir, "permutation_examples.pdf"),
    height = 10, width = 6)
(a/b/cee)
dev.off()


