brms_all_LFD_full <- brm(
  LFD~ 1 + (temp_mother+temp_father)*treat +
    (1 | gr(unique_id, cov = Amat_all)) + (1 | block) +
    (1|mother), # To account for maternal effects
  data = data_all,
  data2 = list(Amat_all = Amat_all),
  chains = 4, cores = 16, iter = 10000, warmup = 3000, thin = 5,
  control = list(adapt_delta = 0.99)
)
save(brms_all_LFD_full,
     file = "output/models/brms_all_LFD_full.rda")

brms_all_date50_full <- brm(
  date50~ 1 + (temp_mother+temp_father)*treat +
    (1 | gr(unique_id, cov = Amat_all)) + (1 | block) +
    (1|mother), # To account for maternal effects
  data = data_all,
  data2 = list(Amat_all = Amat_all),
  chains = 4, cores = 16, iter = 10000, warmup = 3000, thin = 5,
  control = list(adapt_delta = 0.99)
)
save(brms_all_date50_full,
     file = "output/models/brms_all_date50_full.rda")

brms_all_LFD_notemp <- brm(
  LFD~ 1 + treat +
    (1 | gr(unique_id, cov = Amat_all)) + (1 | block) +
    (1|mother), # To account for maternal effects
  data = data_all,
  data2 = list(Amat_all = Amat_all),
  family = gaussian(),
  chains = 4, cores = 16, iter = 15000, warmup = 6000, thin = 5,
  control = list(adapt_delta = 0.99)
)
save(brms_all_LFD_notemp,
     file = "output/models/brms_all_LFD_notemp.rda")

brms_all_date50_notemp <- brm(
  date50~ 1 + treat +
    (1 | gr(unique_id, cov = Amat_all)) + (1 | block) +
    (1|mother), # To account for maternal effects
  data = data_all,
  data2 = list(Amat_all = Amat_all),
  family = gaussian(),
  chains = 4, cores = 16, iter = 10000, warmup = 3000, thin = 5,
  control = list(adapt_delta = 0.99)
)
save(brms_all_date50_notemp,
     file = "output/models/brms_all_date50_notemp.rda")

brms_all_LFD_notreat_notemp <- brm(
  LFD~ 1 +
    (1 | gr(unique_id, cov = Amat_all)) + (1 | block) +
    (1|mother), # To account for maternal effects
  data = data_all,
  data2 = list(Amat_all = Amat_all),
  family = gaussian(),
  chains = 4, cores = 16, iter = 10000, warmup = 3000, thin = 5,
  control = list(adapt_delta = 0.99)
)
save(brms_all_LFD_notreat_notemp,
     file = "output/models/brms_all_LFD_notreat_notemp.rda")

brms_all_date50_notreat_notemp <- brm(
  date50~ 1 +
    (1 | gr(unique_id, cov = Amat_all)) + (1 | block) +
    (1|mother), # To account for maternal effects
  data = data_all,
  data2 = list(Amat_all = Amat_all),
  family = gaussian(),
  chains = 4, cores = 16, iter = 10000, warmup = 3000, thin = 5,
  control = list(adapt_delta = 0.99)
)
save(brms_all_date50_notreat_notemp,
     file = "output/models/brms_all_date50_notreat_notemp.rda")

