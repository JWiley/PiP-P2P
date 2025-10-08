library(JWileymisc)
library(readxl)
library(data.table)


d.hmst <- as.data.table(read_excel(
  "Data/PiP_Data.xlsx", sheet = "HMST", na = c("#NULL!", ".")))
d.hmst <- d.hmst[, .(
  study = "HMST", ID = 1:.N, child_age = child_age,
  PRADAS_t0 = Base_PRADAS, PRADAS_t1 = FU_PRADAS,
  SMFQ_t0 = Base_SMFQ, SMFQ_t1 = FU_SMFQ)]
str(d.hmst)

d.pip <- as.data.table(read_excel(
  "Data/PiP_Data.xlsx", sheet = "PiP"))
d.pip <- d.pip[, .(
  study = "PiP", ID = 1:.N, child_age = reg_child_age,
  PRADAS_t0 = base_PRADAS, PRADAS_t1 = `3M_PRADAS`, PRADAS_t2 = `12M_PRADAS`,
  SMFQ_t0 = base_SMFQ, SMFQ_t1 = `3M_SMFQ`, SMFQ_t2 = `12M_SMFQ`,
  SCAS_t0 = base_SCAS, SCAS_t1 = `3M_SCAS`, SCAS_t2 = `12M_SCAS`)]
str(d.pip)

d.pip_sp <- as.data.table(read_excel(
  "Data/PiP_Data.xlsx", sheet = "PiP_SP"))
d.pip_sp <- d.pip_sp[, .(
  study = "PiP_SP", ID = 1:.N, child_age = reg_child_age,
  PRADAS_t0 = base_PRADAS, PRADAS_t1 = post_PRADAS,
  RCADS_Dep_t0 = base_RCADS_Dx, RCADS_Dep_t1 = post_RCADS_Dx,
  RCADS_Anx_t0 = base_RCADS_Ax, RCADS_Anx_t1 = post_RCADS_Ax)]
str(d.pip_sp)


d.tops <- as.data.table(read_excel(
  "Data/PiP_Data.xlsx", sheet = "TOPS"))
d.tops <- d.tops[, .(
  study = "TOPS", ID = 1:.N, child_age = baseline1_child_age,
  PRADAS_tpre = base1_PRADAS, PRADAS_t0 = base2_PRADAS, PRADAS_t1 = post_PRADAS,
  SMFQ_tpre = base1_SMFQ, SMFQ_t0 = base2_SMFQ, SMFQ_t1 = post_SMFQ,
  SCAS_tpre = base1_SCAS, SCAS_t0 = base2_SCAS, SCAS_t1 = post_SCAS)]
str(d.tops)

d.covid_pip <- as.data.table(read_excel(
  "Data/PiP_Data.xlsx", sheet = "COVID_PiP"))
d.covid_pip <- d.covid_pip[, .(
  study = "COVID_PiP", ID = 1:.N, child_age = `Child Age`,
  PRADAS_t0 = `BL_PRADAS Total Score`, PRADAS_t1 = `FU_PRADAS Total Score`,
  RCADS_Dep_t0 = `BL_RCADS Depression Subscale Total`, RCADS_Dep_t1 = `FU_RCADS Depression Subscale Total`,
  RCADS_Anx_t0 = `BL_RCADS Anxiety Subscale`, RCADS_Anx_t1 = `FU_RCADS Anxiety Subscale`)]
str(d.covid_pip)

## T Scores
d.tscores <- fread("Data/RCADS_Raw and T Scores_CNEdited.csv",
  skip = 1, na = c("", "NA", "#NULL!", "N/A"))
d.tscores <-d.tscores[, .(
  study = "TScores", ID = 1:.N, child_age = `Child Age`,
  PRADAS_t0 = NA_real_, PRADAS_t1 = NA_real_,
  RCADS_Dep_t0 = `BL_Depression_T`, RCADS_Dep_t1 = `FU_Depression_T`,
  RCADS_Anx_t0 = `BL_Anxiety_T`, RCADS_Anx_t1 = `FU_Anxiety_T`)]

DT <- rbindlist(list(d.hmst, d.pip, d.pip_sp, d.tops, d.covid_pip, d.tscores), fill = TRUE)

## make strata
DT[, rcads_strata := fifelse(RCADS_Dep_t0 > quantile(RCADS_Dep_t0, probs = .7, na.rm = TRUE), 1, 0)]
DT[is.na(rcads_strata), rcads_strata := fifelse(SMFQ_t0 > quantile(SMFQ_t0, probs = .7, na.rm = TRUE), 1, 0)]
DT <- DT[!is.na(rcads_strata)] ## remove any missing baseline depression

DT[, age_strata := fifelse(child_age > 16, 1, 0)]
DT <- DT[!is.na(age_strata)] ## remove any missing baseline age

DT[, strata := paste0(rcads_strata, age_strata)]
setnames(DT, old = c("RCADS_Dep_t1", "RCADS_Anx_t1"), new = c("RCADS_Dep_t2", "RCADS_Anx_t2"))

## models
mp <- lm(PRADAS_t1 ~ PRADAS_t0, data = DT)
md <- lm(RCADS_Dep_t2 ~ RCADS_Dep_t0, data = DT[study == "TScores"])
ma <- lm(RCADS_Anx_t2 ~ RCADS_Anx_t0, data = DT[study == "TScores"])

## SDs
sdp <- sd(DT$PRADAS_t0, na.rm = TRUE)
sdd <- sd(DT[study == "TScores", RCADS_Dep_t0], na.rm = TRUE)
sda <- sd(DT[study == "TScores", RCADS_Anx_t0], na.rm = TRUE)

N <- 224

set.seed(123456)
sim <- data.table(
  ID = 1:N,
  cond = rbinom(N, 1, .5),
  PRADAS_t0 = density_inversion(na.omit(DT$PRADAS_t0), n = N, seed = 34235),
  RCADS_Dep_t0 = density_inversion(na.omit(DT[study == "TScores", RCADS_Dep_t0]), n = N, seed = 54532),
  RCADS_Anx_t0 = density_inversion(na.omit(DT[study == "TScores", RCADS_Anx_t0]), n = N, seed = 1235))

sim[, PRADAS_t1 := coef(mp)[1] + PRADAS_t0 * coef(mp)[2] + 
  density_inversion(resid(mp), n = N, seed = 87234)]

sim[, RCADS_Dep_t2 := coef(md)[1] + RCADS_Dep_t0 * coef(md)[2] + 
  density_inversion(resid(md), n = N, seed = 87234)]

sim[, RCADS_Anx_t2 := coef(ma)[1] + RCADS_Anx_t0 * coef(ma)[2] + 
  density_inversion(resid(ma), n = N, seed = 87234)]

## expecting minimal regression coefficient differences
coef(mp) - coef(update(mp, data = sim))
coef(md) - coef(update(md, data = sim))
coef(ma) - coef(update(ma, data = sim))


## half SD decline over time in intervention group
sim[cond == 1, PRADAS_t1 := PRADAS_t1 + (sdp / 2)]
sim[cond == 1, RCADS_Dep_t2 := RCADS_Dep_t2 - (sdd / 2)]
sim[cond == 1, RCADS_Anx_t2 := RCADS_Anx_t2 - (sda / 2)]

## set 25% attrition (random)
set.seed(1234)
sim[sample(.N, .N * .25), c("PRADAS_t1", "RCADS_Dep_t2", "RCADS_Anx_t2") := NA]

## create the strata
round(prop.table(table(DT$strata)) * N)
sim[, strata := sample(rep(c("00", "01", "10", "11"), times = c(154, 9, 54, 7)))]
sim[, age_strata := fifelse(strata %in% c("01", "11"), 1L, 0L)]
sim[, rcads_strata := fifelse(strata %in% c("10", "11"), 1L, 0L)]

sim[, strata := NULL]

## save synthetic dataset
fwrite(sim, file = "synthetic.csv")
