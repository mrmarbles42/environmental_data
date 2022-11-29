require(here)
require(ggplot2)

ginkgo <- read.csv(here("data", "ginkgo_data_2022.csv"))


length(unique(ginkgo$site_id))
#22 trees total

ginkgo_seed <- subset(ginkgo, ginkgo$seeds_present == T)
ginkgo_no_seed <- subset(ginkgo, ginkgo$seeds_present == F)


length(unique(ginkgo_seed$site_id))
#4 trees with seeds

boxplot(ginkgo$max_width ~ ginkgo$seeds_present,
        xlab = "Seeds present (T/F)",
        ylab = "Max leaf width (mm)")

ggplot(ginkgo, aes(seeds_present)) +
  geom_bar(after_stat(aes(max_width)))

           
ggplot(ginkgo, aes(max_depth, max_width)) +
  geom_point() +
  labs(x = "Max leaf depth(mm)",
       y = "Max leaf width (mm)")
