#filtrar datos, si se desea
casen_rm3 <- casen_rm2

#graficar densidad de los ingresos
casen_rm3 %>%
      filter(!is.na(pobreza_multi_5d)) %>%
      ggplot(aes(x = ytotcor,
                 fill = pobreza_multi_5d,
                 col = pobreza_multi_5d)) +
      geom_density(kernel = "gaussian",
                   bw = 50000, n=20000,
                   alpha = 0.7) +
      scale_x_continuous(labels = scales::number,
                         limits = c(0, 4000000),
                         expand = c(0, 0),
                         breaks = c(0, 200000, 400000, 600000, 800000, 1000000, 1250000, 
                                    1500000, 1750000, 2000000, 2500000, 3000000, 3500000, 4000000)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5),
        legend.position = c(.7, .8),
        axis.text.y = element_blank()) +
  facet_wrap(~sexo, ncol=1)