df_all2<-df_all %>%
  mutate(halfhour=ceiling_date(Timestamp, unit="30 minutes"))%>% 
  group_by(halfhour)%>%
  summarise(cov_w_co2=cov(Uz, CO2),
            cov_w_h2o=cov(Uz, H2O),
            cov_w_T=cov(Uz, T_sonic))

df_all2<-mutate(df_all(halfhour=ceiling_date(Timestamp, unit="30 minutes")))
df_all2<-group_by(df_all2, halfhour)
df_all2<-summarise(df_all2,
                   cov_w_co2=cov(Uz, CO2),
                   cov_w_h2o=cov(Uz, H2O),
                   cov_w_T=cov(Uz, T_sonic))


df_all2<-summarise(group_by(mutate(df_all, halfhour=ceiling_date(Timestamp, unit="30 minutes")),
                    halfhour),
                    cov_w_co2=cov(Uz, CO2),
                    cov_w_h2o=cov(Uz, H2O),
                    cov_w_T=cov(Uz, T_sonic))

rot<-data.frame(df_all_rot1, df_all_rot2)
