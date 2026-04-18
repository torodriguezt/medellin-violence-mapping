# =====================================================
# 09_resultados_finales.R
# Compilación de resultados finales
# =====================================================

source("08_visualizaciones.R")

# --- Probabilidad de estar en el Top 3 ---
message("\n>>> Calculando probabilidad de top 3...")
S   <- INLA::inla.posterior.sample(2000, result = best_fit, seed = 1)
idx <- grep("^Predictor", rownames(S[[1]]$latent))
ETA <- sapply(S, function(s) s$latent[idx])
r_nac <- sum(agg_depto_table$y_model) / sum(agg_depto_table$E_pop)
RRd <- exp(ETA) / r_nac
ranks <- apply(RRd, 2, function(x) rank(-x, ties.method = "average"))
P_top3 <- rowMeans(ranks <= 3)

topk_tbl <- depto_agg_sf %>% 
  st_drop_geometry() %>%
  transmute(
    depto, 
    cod_dpto, 
    P_top3 = scales::percent(P_top3, accuracy = 0.1)
  ) %>%
  arrange(desc(P_top3))

message("Top 10 departamentos con mayor probabilidad de estar en top 3:")
print(head(topk_tbl, 10))

# --- Resumen final ---
message("\n========================================")
message("RESUMEN DE RESULTADOS")
message("========================================")
message(sprintf("Total casos analizados: %d", sum(agg_depto_table$y_obs)))
message(sprintf("Población total: %s", format(sum(agg_depto_table$pop_fem), big.mark = ",")))
message(sprintf("Tasa cruda nacional: %.2f por 100,000", 
                100000 * sum(agg_depto_table$y_obs) / sum(agg_depto_table$pop_fem)))
message(sprintf("\nMejor modelo: %s", best_name))
message(sprintf("DIC: %.2f", best_fit$dic$dic))
message(sprintf("WAIC: %.2f", best_fit$waic$waic))
message(sprintf("LCPO: %.4f", mean(-log(best_fit$cpo$cpo), na.rm = TRUE)))

message("\n>>> Top 5 departamentos de mayor riesgo:")
print(rr_rank)
