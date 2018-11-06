resetPars()
test_sim <- simulation(pars = pars, returnALL = T)

stripaxis <- theme(axis.text = element_blank(), axis.ticks = element_blank())

unifPlot <-
  ggplot(test_sim$counts, aes(safety, uniformBirds)) + geom_line(colour = "red") +
  stripaxis +
  labs(y = "", x = "")

# ggsave(filename = "../IOC_Poster/images/uniform_ex.png", bg='transparent', height=4,width=4)
areaMatchplt <-
  ggplot(test_sim$counts, aes(safety, areaMatchingBirds)) + geom_line(colour = "red") +
  stripaxis +
  labs(y = "", x = "")
# ggsave(filename = "../IOC_Poster/images/areaM_ex.png", bg='transparent', height=4,width=4)
dangermatchplt <-
  ggplot(test_sim$counts, aes(safety, dangerMatchingBirds)) + geom_line(colour = "red") +
  stripaxis +
  labs(y = "", x = "")
# ggsave(filename = "../IOC_Poster/images/dangerM_ex.png", bg='transparent', height=4,width=4)
hurdleplt <-
  ggplot(test_sim$counts, aes(safety, hurdleBirds)) + geom_line(colour = "red") +
  stripaxis +
  labs(y = "", x = "")

# ggsave(filename = "../IOC_Poster/images/hurdle_ex.png", bg='transparent', height=4,width=4)

explotsall <-
  plot_grid(unifPlot, areaMatchplt, dangermatchplt, hurdleplt, nrow = 1)
