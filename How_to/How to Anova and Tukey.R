# How to Anova and Tukey
# R 4.2.1
# @BragatteMAs

data (npk)

summary (npk)

resultado <- aov (npk$yield ~ npk$N, npk$P, npk$K)
resultado
 
TukeyHSD (resultado)

# Reference
# [r - How to obtain the results of a Tukey HSD post-hoc test in a table showing grouped pairs? - Cross Validated](https://stats.stackexchange.com/questions/31547/how-to-obtain-the-results-of-a-tukey-hsd-post-hoc-test-in-a-table-showing-groupe)