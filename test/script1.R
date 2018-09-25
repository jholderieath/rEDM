find_q_chg <- function(A,B,X,...) {
  ainv <- solve(A)
  ainv %*% B %*% X
}

number_of_commodities <- 5
imports <- TRUE
exports <- TRUE


ed <- matrix(data = c(
  -0.696, -0.289,	-0.094, 	-0.024, -0.172,
  -0.382,	-0.536,	0.031,		-0.021,	-0.046,
  -1.665,	0.498,	-0.202,		0.056,	0.591,
  -0.18,	-0.027,	0.114,		-0.065,	0.477,
  -0.191,	0.108,	0.127,		-0.008,	-0.235
),nrow = number_of_commodities,ncol = number_of_commodities)

esa <- matrix(data = c(
  0.201,	-0.108,	-0.004,	0,	0,
  -0.167,	0.153,	-0.005,	-0.001,	0,
  -0.155,	-0.110,	0.201,	-0.001,	0,
  -0.164,	-0.117,	-0.006,	0.238,	0,
  0,	0,	0,	0,	0.350
),nrow = number_of_commodities,ncol = number_of_commodities)

esw <- matrix(data = c(
  0.326,	-0.036,	-0.003,	-0.034,	0,
  -0.031,	0.191,	-0.008,	-0.095,	0,
  -0.016,	-0.047,	0.331,	-0.045,	0,
  -0.015,	-0.050,	-0.004,	0.473,	0,
  0,	0,	0,	0,	0.35
),nrow = number_of_commodities,ncol = number_of_commodities)

e0 <- matrix(data = 0,nrow = number_of_commodities,ncol = number_of_commodities)

ed0 <- cbind(ed,e0,e0)
esa0 <- cbind(e0,esa,e0)
esw0 <- cbind(e0,e0,esw)

A <- rbind(ed0,esa0,esw0)

rm(e0,ed0,esa0,esw0)

X <- matrix(data = c(0,0,0,0,0,
                     0,0,0,0,0,
                     0.0147,
                     0.005,
                     0.0166,
                     0.0046,
                     0.018),nrow=15,ncol = 1)






library(rSymPy)
EP_corn <- Var('EP_corn')
EP_soy <- Var('EP_soy')
EP_wheat <- Var('EP_wheat')
EP_rice <- Var('EP_rice')
EP_peanut <- Var('EP_peanut')



e_d_corn  <- Var('e_d_corn')
e_d_soy  <- Var('e_d_soy')
e_d_wheat  <- Var('e_d_wheat')
e_d_rice  <- Var('e_d_rice')
e_d_peanut  <- Var('e_d_peanut')






ss <- c('ss_AOS_corn', 'ss_FRS_corn',
        'ss_AOS_soy', 'ss_FRS_soy',
        'ss_AOS_wheat', 'ss_FRS_wheat',
        'ss_AOS_rice', 'ss_FRS_rice',
        'ss_AOS_peanuts', 'ss_FRS_peanuts',
        'ss_imports_corn', 'ss_imports_soy', 'ss_imports_wheat', 'ss_imports_rice', 'ss_imports_peanut',
        'ss_exports_corn', 'ss_exports_soy', 'ss_exports_wheat', 'ss_exports_rice', 'ss_exports_peanut'
)

for (i in ss) {
  assign(noquote(i),Var(i))
}


B <- c('B_AOS_c','B_AOS_s','B_AOS_w','B_AOS_r','B_AOS_p',
       'B_FRS_c', 'B_FRS_s', 'B_FRS_w', 'B_FRS_r', 'B_FRS_p')

for (i in B) {
  assign(noquote(i),Var(i))
}


elast <- c(
  'e_e_Corn', 'e_e_Soy', 'e_e_Wheat', 'e_e_Rice', 'e_e_Peanuts',
  'e_i_Corn', 'e_i_Soy', 'e_i_Wheat', 'e_i_Rice', 'e_i_Peanuts',

  'e_s_AOS_cc', 'e_s_AOS_cs', 'e_s_AOS_cw', 'e_s_AOS_cr', 'e_s_AOS_cp',
  'e_s_AOS_sc', 'e_s_AOS_ss', 'e_s_AOS_sw', 'e_s_AOS_sr', 'e_s_AOS_sp',
  'e_s_AOS_wc', 'e_s_AOS_ws', 'e_s_AOS_ww', 'e_s_AOS_wr', 'e_s_AOS_wp',
  'e_s_AOS_rc', 'e_s_AOS_rs', 'e_s_AOS_rw', 'e_s_AOS_rr', 'e_s_AOS_rp',
  'e_s_AOS_pc', 'e_s_AOS_ps', 'e_s_AOS_pw', 'e_s_AOS_pr', 'e_s_AOS_pp',

  'e_s_FRS_cc', 'e_s_FRS_cs', 'e_s_FRS_cw', 'e_s_FRS_cr', 'e_s_FRS_cp',
  'e_s_FRS_sc', 'e_s_FRS_ss', 'e_s_FRS_sw', 'e_s_FRS_sr', 'e_s_FRS_sp',
  'e_s_FRS_wc', 'e_s_FRS_ws', 'e_s_FRS_ww', 'e_s_FRS_wr', 'e_s_FRS_wp',
  'e_s_FRS_rc', 'e_s_FRS_rs', 'e_s_FRS_rw', 'e_s_FRS_rr', 'e_s_FRS_rp',
  'e_s_FRS_pc', 'e_s_FRS_ps', 'e_s_FRS_pw', 'e_s_FRS_pr', 'e_s_FRS_pp'
)

for (i in elast) {
  assign(noquote(i),Var(i))
}


##--------------------

e_d_corn = -0.696
e_d_soy = -0.065
e_d_wheat =-0.202
e_d_rice = -0.065
e_d_peanut = -0.235

ss_AOS_corn <-  0.94
ss_FRS_corn <-  0.06
ss_AOS_soy <-  0.88
ss_FRS_soy <-  0.12
ss_AOS_wheat <-  0.90
ss_FRS_wheat <-  0.10
ss_AOS_rice <-  0.24
ss_FRS_rice <-  0.76
ss_AOS_peanuts <-  0.03
ss_FRS_peanuts <-  0.97
ss_imports_corn <-  0.01
ss_imports_soy <-  0.02
ss_imports_wheat <-  0.12
ss_imports_rice <-  0.17
ss_imports_peanut <-  0.03
ss_exports_corn <-  0.14
ss_exports_soy <-  0.83
ss_exports_wheat <-  0.80
ss_exports_rice <-  0.84
ss_exports_peanut <-  0.23

B_AOS_c <- 0
B_AOS_s <- 0
B_AOS_w <- 0
B_AOS_r <- 0
B_AOS_p <- 0

B_FRS_c <- 0.0147
B_FRS_s <- 0.005
B_FRS_w <- 0.0166
B_FRS_r <- 0.0046
B_FRS_p <- 0.018

e_e_Corn <- -1.20
e_e_Soy <- -2.50
e_e_Wheat <- -0.85
e_e_Rice <- -2.62
e_e_Peanuts <- -1.00

e_i_Corn <- 0.50
e_i_Soy <- 0.50
e_i_Wheat <- 0.50
e_i_Rice <- 0.40
e_i_Peanuts <- 0.50

e_s_AOS_cc <- 0.20
e_s_AOS_cs <- -0.11
e_s_AOS_cw <- -0.00
e_s_AOS_cr <-0
e_s_AOS_cp <- 0
e_s_AOS_sc <- -0.17
e_s_AOS_ss <- 0.15
e_s_AOS_sw <- -0.01
e_s_AOS_sr <- -0.00
e_s_AOS_sp <- 0
e_s_AOS_wc <- -0.15
e_s_AOS_ws <- -0.11
e_s_AOS_ww <- 0.20
e_s_AOS_wr <- -0.00
e_s_AOS_wp <- 0
e_s_AOS_rc <- -0.16
e_s_AOS_rs <- -0.12
e_s_AOS_rw <- -0.01
e_s_AOS_rr <- 0.24
e_s_AOS_rp <- 0
e_s_AOS_pc <- 0
e_s_AOS_ps <- 0
e_s_AOS_pw <- 0
e_s_AOS_pr <- 0
e_s_AOS_pp <- 0.35


e_s_FRS_cc <- 0.33
e_s_FRS_cs <- -0.04
e_s_FRS_cw <- -0.00
e_s_FRS_cr <- -0.03
e_s_FRS_cp <- 0
e_s_FRS_sc <- -0.03
e_s_FRS_ss <- 0.19
e_s_FRS_sw <- -0.01
e_s_FRS_sr <- -0.10
e_s_FRS_sp <- 0
e_s_FRS_wc <- -0.02
e_s_FRS_ws <- -0.05
e_s_FRS_ww <- 0.33
e_s_FRS_wr <- -0.04
e_s_FRS_wp <- 0
e_s_FRS_rc <- -0.01
e_s_FRS_rs <- -0.05
e_s_FRS_rw <- -0.00
e_s_FRS_rr <- 0.47
e_s_FRS_rp <- 0
e_s_FRS_pc <- 0
e_s_FRS_ps <- 0
e_s_FRS_pw <- 0
e_s_FRS_pr <- 0
e_s_FRS_pp <- 0.35

sympy(
'eqns = [
Eq(EP_corn * e_d_corn + (EP_corn) * (ss_exports_corn * e_e_Corn - ss_imports_corn *e_i_Corn), ((ss_FRS_corn * (e_s_FRS_cc * EP_corn + e_s_FRS_cs * EP_soy + e_s_FRS_cw * EP_wheat + e_s_FRS_cr * EP_rice + e_s_FRS_cp * EP_peanut + B_FRS_c)) + (ss_AOS_corn * (e_s_AOS_cc * EP_corn + e_s_AOS_cs * EP_soy + e_s_AOS_cw * EP_wheat + e_s_AOS_cr * EP_rice + e_s_AOS_cp * EP_peanut + B_AOS_c)))),
Eq(EP_soy * e_d_soy + (EP_soy) * (ss_exports_soy * e_e_Soy - ss_imports_soy *e_i_Soy), ((ss_FRS_soy * (e_s_FRS_sc * EP_corn + e_s_FRS_ss * EP_soy + e_s_FRS_sw * EP_wheat + e_s_FRS_sr * EP_rice + e_s_FRS_sp * EP_peanut + B_FRS_s)) + (ss_AOS_soy * (e_s_AOS_sc * EP_corn + e_s_AOS_ss * EP_soy + e_s_AOS_sw * EP_wheat + e_s_AOS_sr * EP_rice + e_s_AOS_sp * EP_peanut + B_AOS_s)))),
Eq(EP_wheat * e_d_wheat + (EP_wheat) * (ss_exports_wheat * e_e_Wheat - ss_imports_wheat *e_i_Wheat), ((ss_FRS_wheat * (e_s_FRS_wc * EP_corn + e_s_FRS_ws * EP_soy + e_s_FRS_ww * EP_wheat + e_s_FRS_wr * EP_rice + e_s_FRS_wp * EP_peanut + B_FRS_w)) + (ss_AOS_wheat * (e_s_AOS_wc * EP_corn + e_s_AOS_ws * EP_soy + e_s_AOS_ww * EP_wheat + e_s_AOS_wr * EP_rice + e_s_AOS_wp * EP_peanut + B_AOS_w)))),
Eq(EP_rice * e_d_peanut + (EP_rice) * (ss_exports_rice * e_e_Rice - ss_imports_rice *e_i_Rice), ((ss_FRS_rice * (e_s_FRS_rc * EP_corn + e_s_FRS_rs * EP_soy + e_s_FRS_rw * EP_wheat + e_s_FRS_rr * EP_rice + e_s_FRS_rp * EP_peanut + B_FRS_r)) + (ss_AOS_rice * (e_s_AOS_rc * EP_corn + e_s_AOS_rs * EP_soy + e_s_AOS_rw * EP_wheat + e_s_AOS_rr * EP_rice + e_s_AOS_rp * EP_peanut + B_AOS_r)))),
Eq(EP_peanut * e_d_peanut + (EP_peanut) * (ss_exports_peanut * e_e_Peanuts - ss_imports_peanut *e_i_Peanuts), ((ss_FRS_peanuts * (e_s_FRS_pc * EP_corn + e_s_FRS_ps * EP_soy + e_s_FRS_pw * EP_wheat + e_s_FRS_pr * EP_rice + e_s_FRS_pp * EP_peanut + B_FRS_p)) + (ss_AOS_peanuts * (e_s_AOS_pc * EP_corn + e_s_AOS_ps * EP_soy + e_s_AOS_pw * EP_wheat + e_s_AOS_pr * EP_rice + e_s_AOS_pp * EP_peanut + B_AOS_p)))) ]'
)


sympy('corn = Eq(EP_corn * e_d_corn + (EP_corn) * (ss_exports_corn * e_e_Corn - ss_imports_corn *e_i_Corn), ((ss_FRS_corn * (e_s_FRS_cc * EP_corn + e_s_FRS_cs * EP_soy + e_s_FRS_cw * EP_wheat + e_s_FRS_cr * EP_rice + e_s_FRS_cp * EP_peanut + B_FRS_c)) + (ss_AOS_corn * (e_s_AOS_cc * EP_corn + e_s_AOS_cs * EP_soy + e_s_AOS_cw * EP_wheat + e_s_AOS_cr * EP_rice + e_s_AOS_cp * EP_peanut + B_AOS_c))))')
sympy('soy = Eq(EP_soy * e_d_soy + (EP_soy) * (ss_exports_soy * e_e_Soy - ss_imports_soy *e_i_Soy), ((ss_FRS_soy * (e_s_FRS_sc * EP_corn + e_s_FRS_ss * EP_soy + e_s_FRS_sw * EP_wheat + e_s_FRS_sr * EP_rice + e_s_FRS_sp * EP_peanut + B_FRS_s)) + (ss_AOS_soy * (e_s_AOS_sc * EP_corn + e_s_AOS_ss * EP_soy + e_s_AOS_sw * EP_wheat + e_s_AOS_sr * EP_rice + e_s_AOS_sp * EP_peanut + B_AOS_s))))')
sympy('wheat = Eq(EP_wheat * e_d_wheat + (EP_wheat) * (ss_exports_wheat * e_e_Wheat - ss_imports_wheat *e_i_Wheat), ((ss_FRS_wheat * (e_s_FRS_wc * EP_corn + e_s_FRS_ws * EP_soy + e_s_FRS_ww * EP_wheat + e_s_FRS_wr * EP_rice + e_s_FRS_wp * EP_peanut + B_FRS_w)) + (ss_AOS_wheat * (e_s_AOS_wc * EP_corn + e_s_AOS_ws * EP_soy + e_s_AOS_ww * EP_wheat + e_s_AOS_wr * EP_rice + e_s_AOS_wp * EP_peanut + B_AOS_w))))')
sympy('rice = Eq(EP_rice * e_d_peanut + (EP_rice) * (ss_exports_rice * e_e_Rice - ss_imports_rice *e_i_Rice), ((ss_FRS_rice * (e_s_FRS_rc * EP_corn + e_s_FRS_rs * EP_soy + e_s_FRS_rw * EP_wheat + e_s_FRS_rr * EP_rice + e_s_FRS_rp * EP_peanut + B_FRS_r)) + (ss_AOS_rice * (e_s_AOS_rc * EP_corn + e_s_AOS_rs * EP_soy + e_s_AOS_rw * EP_wheat + e_s_AOS_rr * EP_rice + e_s_AOS_rp * EP_peanut + B_AOS_r))))')
sympy('peanut = Eq(EP_peanut * e_d_peanut + (EP_peanut) * (ss_exports_peanut * e_e_Peanuts - ss_imports_peanut *e_i_Peanuts), ((ss_FRS_peanuts * (e_s_FRS_pc * EP_corn + e_s_FRS_ps * EP_soy + e_s_FRS_pw * EP_wheat + e_s_FRS_pr * EP_rice + e_s_FRS_pp * EP_peanut + B_FRS_p)) + (ss_AOS_peanuts * (e_s_AOS_pc * EP_corn + e_s_AOS_ps * EP_soy + e_s_AOS_pw * EP_wheat + e_s_AOS_pr * EP_rice + e_s_AOS_pp * EP_peanut + B_AOS_p))))')

sympy('corn = corn.subs([
      (e_d_corn,-0.696), (e_d_soy,-0.065), (e_d_wheat,-0.202), (e_d_rice,-0.065), (e_d_peanut,-0.235),
      (ss_AOS_corn,0.94), (ss_FRS_corn,0.06),
      (ss_AOS_soy,0.88), (ss_FRS_soy,0.12),
      (ss_AOS_wheat,0.90), (ss_FRS_wheat,0.10),
      (ss_AOS_rice,0.24), (ss_FRS_rice,0.76),
      (ss_AOS_peanuts,0.03), (ss_FRS_peanuts,0.97),
      (ss_imports_corn,0.01), (ss_imports_soy,0.02), (ss_imports_wheat,0.12), (ss_imports_rice,0.17), (ss_imports_peanut,0.03),
      (ss_exports_corn,0.14), (ss_exports_soy,0.83), (ss_exports_wheat,0.80), (ss_exports_rice,0.84), (ss_exports_peanut,0.23),
      (B_AOS_c,0),(B_AOS_s,0),(B_AOS_w,0),(B_AOS_r,0),(B_AOS_p,0),
      (B_FRS_c,0.0147),(B_FRS_s,0.005),(B_FRS_w,0.0166),(B_FRS_r,0.0046),(B_FRS_p,0.018),
      (e_e_Corn,-1.20),(e_e_Soy,-2.50),(e_e_Wheat,-0.85),(e_e_Rice,-2.62),(e_e_Peanuts,-1.00),
      (e_i_Corn,0.50),(e_i_Soy,0.50),(e_i_Wheat,0.50),(e_i_Rice,0.40),(e_i_Peanuts,0.50),
      (e_s_AOS_cc,0.20),(e_s_AOS_cs,-0.11),(e_s_AOS_cw,-0.00),(e_s_AOS_cr,0),(e_s_AOS_cp,0),
      (e_s_AOS_sc,-0.17),(e_s_AOS_ss,0.15),(e_s_AOS_sw,-0.01),(e_s_AOS_sr,-0.00),(e_s_AOS_sp,0),
      (e_s_AOS_wc,-0.15),(e_s_AOS_ws,-0.11),(e_s_AOS_ww,0.20),(e_s_AOS_wr,-0.00),(e_s_AOS_wp,0),
      (e_s_AOS_rc,-0.16),(e_s_AOS_rs,-0.12),(e_s_AOS_rw,-0.01),(e_s_AOS_rr,0.24),(e_s_AOS_rp,0),
      (e_s_AOS_pc,0),(e_s_AOS_ps,0),(e_s_AOS_pw,0),(e_s_AOS_pr,0),(e_s_AOS_pp,0.35),
      (e_s_FRS_cc,0.33),(e_s_FRS_cs,-0.04),(e_s_FRS_cw,-0.00),(e_s_FRS_cr,-0.03),(e_s_FRS_cp,0),
      (e_s_FRS_sc,-0.03),(e_s_FRS_ss,0.19),(e_s_FRS_sw,-0.01 ),(e_s_FRS_sr,-0.10 ),(e_s_FRS_sp,0),
      (e_s_FRS_wc,-0.02),(e_s_FRS_ws,-0.05 ),(e_s_FRS_ww,0.33),(e_s_FRS_wr,-0.04 ),(e_s_FRS_wp,0),
      (e_s_FRS_rc,-0.01 ), (e_s_FRS_rs,-0.05), (e_s_FRS_rw,-0.00 ), (e_s_FRS_rr,0.47),(e_s_FRS_rp,0),
      (e_s_FRS_pc,0),(e_s_FRS_ps,0),(e_s_FRS_pw,0),(e_s_FRS_pr,0),(e_s_FRS_pp,0.35)
      ])')

sympy('soy = soy.subs([
      (e_d_corn,-0.696), (e_d_soy,-0.065), (e_d_wheat,-0.202), (e_d_rice,-0.065), (e_d_peanut,-0.235),
      (ss_AOS_corn,0.94), (ss_FRS_corn,0.06),
      (ss_AOS_soy,0.88), (ss_FRS_soy,0.12),
      (ss_AOS_wheat,0.90), (ss_FRS_wheat,0.10),
      (ss_AOS_rice,0.24), (ss_FRS_rice,0.76),
      (ss_AOS_peanuts,0.03), (ss_FRS_peanuts,0.97),
      (ss_imports_corn,0.01), (ss_imports_soy,0.02), (ss_imports_wheat,0.12), (ss_imports_rice,0.17), (ss_imports_peanut,0.03),
      (ss_exports_corn,0.14), (ss_exports_soy,0.83), (ss_exports_wheat,0.80), (ss_exports_rice,0.84), (ss_exports_peanut,0.23),
      (B_AOS_c,0),(B_AOS_s,0),(B_AOS_w,0),(B_AOS_r,0),(B_AOS_p,0),
      (B_FRS_c,0.0147),(B_FRS_s,0.005),(B_FRS_w,0.0166),(B_FRS_r,0.0046),(B_FRS_p,0.018),
      (e_e_Corn,-1.20),(e_e_Soy,-2.50),(e_e_Wheat,-0.85),(e_e_Rice,-2.62),(e_e_Peanuts,-1.00),
      (e_i_Corn,0.50),(e_i_Soy,0.50),(e_i_Wheat,0.50),(e_i_Rice,0.40),(e_i_Peanuts,0.50),
      (e_s_AOS_cc,0.20),(e_s_AOS_cs,-0.11),(e_s_AOS_cw,-0.00),(e_s_AOS_cr,0),(e_s_AOS_cp,0),
      (e_s_AOS_sc,-0.17),(e_s_AOS_ss,0.15),(e_s_AOS_sw,-0.01),(e_s_AOS_sr,-0.00),(e_s_AOS_sp,0),
      (e_s_AOS_wc,-0.15),(e_s_AOS_ws,-0.11),(e_s_AOS_ww,0.20),(e_s_AOS_wr,-0.00),(e_s_AOS_wp,0),
      (e_s_AOS_rc,-0.16),(e_s_AOS_rs,-0.12),(e_s_AOS_rw,-0.01),(e_s_AOS_rr,0.24),(e_s_AOS_rp,0),
      (e_s_AOS_pc,0),(e_s_AOS_ps,0),(e_s_AOS_pw,0),(e_s_AOS_pr,0),(e_s_AOS_pp,0.35),
      (e_s_FRS_cc,0.33),(e_s_FRS_cs,-0.04),(e_s_FRS_cw,-0.00),(e_s_FRS_cr,-0.03),(e_s_FRS_cp,0),
      (e_s_FRS_sc,-0.03),(e_s_FRS_ss,0.19),(e_s_FRS_sw,-0.01 ),(e_s_FRS_sr,-0.10 ),(e_s_FRS_sp,0),
      (e_s_FRS_wc,-0.02),(e_s_FRS_ws,-0.05 ),(e_s_FRS_ww,0.33),(e_s_FRS_wr,-0.04 ),(e_s_FRS_wp,0),
      (e_s_FRS_rc,-0.01 ), (e_s_FRS_rs,-0.05), (e_s_FRS_rw,-0.00 ), (e_s_FRS_rr,0.47),(e_s_FRS_rp,0),
      (e_s_FRS_pc,0),(e_s_FRS_ps,0),(e_s_FRS_pw,0),(e_s_FRS_pr,0),(e_s_FRS_pp,0.35)
      ])')

sympy('wheat = wheat.subs([
      (e_d_corn,-0.696), (e_d_soy,-0.065), (e_d_wheat,-0.202), (e_d_rice,-0.065), (e_d_peanut,-0.235),
      (ss_AOS_corn,0.94), (ss_FRS_corn,0.06),
      (ss_AOS_soy,0.88), (ss_FRS_soy,0.12),
      (ss_AOS_wheat,0.90), (ss_FRS_wheat,0.10),
      (ss_AOS_rice,0.24), (ss_FRS_rice,0.76),
      (ss_AOS_peanuts,0.03), (ss_FRS_peanuts,0.97),
      (ss_imports_corn,0.01), (ss_imports_soy,0.02), (ss_imports_wheat,0.12), (ss_imports_rice,0.17), (ss_imports_peanut,0.03),
      (ss_exports_corn,0.14), (ss_exports_soy,0.83), (ss_exports_wheat,0.80), (ss_exports_rice,0.84), (ss_exports_peanut,0.23),
      (B_AOS_c,0),(B_AOS_s,0),(B_AOS_w,0),(B_AOS_r,0),(B_AOS_p,0),
      (B_FRS_c,0.0147),(B_FRS_s,0.005),(B_FRS_w,0.0166),(B_FRS_r,0.0046),(B_FRS_p,0.018),
      (e_e_Corn,-1.20),(e_e_Soy,-2.50),(e_e_Wheat,-0.85),(e_e_Rice,-2.62),(e_e_Peanuts,-1.00),
      (e_i_Corn,0.50),(e_i_Soy,0.50),(e_i_Wheat,0.50),(e_i_Rice,0.40),(e_i_Peanuts,0.50),
      (e_s_AOS_cc,0.20),(e_s_AOS_cs,-0.11),(e_s_AOS_cw,-0.00),(e_s_AOS_cr,0),(e_s_AOS_cp,0),
      (e_s_AOS_sc,-0.17),(e_s_AOS_ss,0.15),(e_s_AOS_sw,-0.01),(e_s_AOS_sr,-0.00),(e_s_AOS_sp,0),
      (e_s_AOS_wc,-0.15),(e_s_AOS_ws,-0.11),(e_s_AOS_ww,0.20),(e_s_AOS_wr,-0.00),(e_s_AOS_wp,0),
      (e_s_AOS_rc,-0.16),(e_s_AOS_rs,-0.12),(e_s_AOS_rw,-0.01),(e_s_AOS_rr,0.24),(e_s_AOS_rp,0),
      (e_s_AOS_pc,0),(e_s_AOS_ps,0),(e_s_AOS_pw,0),(e_s_AOS_pr,0),(e_s_AOS_pp,0.35),
      (e_s_FRS_cc,0.33),(e_s_FRS_cs,-0.04),(e_s_FRS_cw,-0.00),(e_s_FRS_cr,-0.03),(e_s_FRS_cp,0),
      (e_s_FRS_sc,-0.03),(e_s_FRS_ss,0.19),(e_s_FRS_sw,-0.01 ),(e_s_FRS_sr,-0.10 ),(e_s_FRS_sp,0),
      (e_s_FRS_wc,-0.02),(e_s_FRS_ws,-0.05 ),(e_s_FRS_ww,0.33),(e_s_FRS_wr,-0.04 ),(e_s_FRS_wp,0),
      (e_s_FRS_rc,-0.01 ), (e_s_FRS_rs,-0.05), (e_s_FRS_rw,-0.00 ), (e_s_FRS_rr,0.47),(e_s_FRS_rp,0),
      (e_s_FRS_pc,0),(e_s_FRS_ps,0),(e_s_FRS_pw,0),(e_s_FRS_pr,0),(e_s_FRS_pp,0.35)
      ])')

sympy('rice = rice.subs([
      (e_d_corn,-0.696), (e_d_soy,-0.065), (e_d_wheat,-0.202), (e_d_rice,-0.065), (e_d_peanut,-0.235),
      (ss_AOS_corn,0.94), (ss_FRS_corn,0.06),
      (ss_AOS_soy,0.88), (ss_FRS_soy,0.12),
      (ss_AOS_wheat,0.90), (ss_FRS_wheat,0.10),
      (ss_AOS_rice,0.24), (ss_FRS_rice,0.76),
      (ss_AOS_peanuts,0.03), (ss_FRS_peanuts,0.97),
      (ss_imports_corn,0.01), (ss_imports_soy,0.02), (ss_imports_wheat,0.12), (ss_imports_rice,0.17), (ss_imports_peanut,0.03),
      (ss_exports_corn,0.14), (ss_exports_soy,0.83), (ss_exports_wheat,0.80), (ss_exports_rice,0.84), (ss_exports_peanut,0.23),
      (B_AOS_c,0),(B_AOS_s,0),(B_AOS_w,0),(B_AOS_r,0),(B_AOS_p,0),
      (B_FRS_c,0.0147),(B_FRS_s,0.005),(B_FRS_w,0.0166),(B_FRS_r,0.0046),(B_FRS_p,0.018),
      (e_e_Corn,-1.20),(e_e_Soy,-2.50),(e_e_Wheat,-0.85),(e_e_Rice,-2.62),(e_e_Peanuts,-1.00),
      (e_i_Corn,0.50),(e_i_Soy,0.50),(e_i_Wheat,0.50),(e_i_Rice,0.40),(e_i_Peanuts,0.50),
      (e_s_AOS_cc,0.20),(e_s_AOS_cs,-0.11),(e_s_AOS_cw,-0.00),(e_s_AOS_cr,0),(e_s_AOS_cp,0),
      (e_s_AOS_sc,-0.17),(e_s_AOS_ss,0.15),(e_s_AOS_sw,-0.01),(e_s_AOS_sr,-0.00),(e_s_AOS_sp,0),
      (e_s_AOS_wc,-0.15),(e_s_AOS_ws,-0.11),(e_s_AOS_ww,0.20),(e_s_AOS_wr,-0.00),(e_s_AOS_wp,0),
      (e_s_AOS_rc,-0.16),(e_s_AOS_rs,-0.12),(e_s_AOS_rw,-0.01),(e_s_AOS_rr,0.24),(e_s_AOS_rp,0),
      (e_s_AOS_pc,0),(e_s_AOS_ps,0),(e_s_AOS_pw,0),(e_s_AOS_pr,0),(e_s_AOS_pp,0.35),
      (e_s_FRS_cc,0.33),(e_s_FRS_cs,-0.04),(e_s_FRS_cw,-0.00),(e_s_FRS_cr,-0.03),(e_s_FRS_cp,0),
      (e_s_FRS_sc,-0.03),(e_s_FRS_ss,0.19),(e_s_FRS_sw,-0.01 ),(e_s_FRS_sr,-0.10 ),(e_s_FRS_sp,0),
      (e_s_FRS_wc,-0.02),(e_s_FRS_ws,-0.05 ),(e_s_FRS_ww,0.33),(e_s_FRS_wr,-0.04 ),(e_s_FRS_wp,0),
      (e_s_FRS_rc,-0.01 ), (e_s_FRS_rs,-0.05), (e_s_FRS_rw,-0.00 ), (e_s_FRS_rr,0.47),(e_s_FRS_rp,0),
      (e_s_FRS_pc,0),(e_s_FRS_ps,0),(e_s_FRS_pw,0),(e_s_FRS_pr,0),(e_s_FRS_pp,0.35)
      ])')

sympy('peanut = peanut.subs([
      (e_d_corn,-0.696), (e_d_soy,-0.065), (e_d_wheat,-0.202), (e_d_rice,-0.065), (e_d_peanut,-0.235),
      (ss_AOS_corn,0.94), (ss_FRS_corn,0.06),
      (ss_AOS_soy,0.88), (ss_FRS_soy,0.12),
      (ss_AOS_wheat,0.90), (ss_FRS_wheat,0.10),
      (ss_AOS_rice,0.24), (ss_FRS_rice,0.76),
      (ss_AOS_peanuts,0.03), (ss_FRS_peanuts,0.97),
      (ss_imports_corn,0.01), (ss_imports_soy,0.02), (ss_imports_wheat,0.12), (ss_imports_rice,0.17), (ss_imports_peanut,0.03),
      (ss_exports_corn,0.14), (ss_exports_soy,0.83), (ss_exports_wheat,0.80), (ss_exports_rice,0.84), (ss_exports_peanut,0.23),
      (B_AOS_c,0),(B_AOS_s,0),(B_AOS_w,0),(B_AOS_r,0),(B_AOS_p,0),
      (B_FRS_c,0.0147),(B_FRS_s,0.005),(B_FRS_w,0.0166),(B_FRS_r,0.0046),(B_FRS_p,0.018),
      (e_e_Corn,-1.20),(e_e_Soy,-2.50),(e_e_Wheat,-0.85),(e_e_Rice,-2.62),(e_e_Peanuts,-1.00),
      (e_i_Corn,0.50),(e_i_Soy,0.50),(e_i_Wheat,0.50),(e_i_Rice,0.40),(e_i_Peanuts,0.50),
      (e_s_AOS_cc,0.20),(e_s_AOS_cs,-0.11),(e_s_AOS_cw,-0.00),(e_s_AOS_cr,0),(e_s_AOS_cp,0),
      (e_s_AOS_sc,-0.17),(e_s_AOS_ss,0.15),(e_s_AOS_sw,-0.01),(e_s_AOS_sr,-0.00),(e_s_AOS_sp,0),
      (e_s_AOS_wc,-0.15),(e_s_AOS_ws,-0.11),(e_s_AOS_ww,0.20),(e_s_AOS_wr,-0.00),(e_s_AOS_wp,0),
      (e_s_AOS_rc,-0.16),(e_s_AOS_rs,-0.12),(e_s_AOS_rw,-0.01),(e_s_AOS_rr,0.24),(e_s_AOS_rp,0),
      (e_s_AOS_pc,0),(e_s_AOS_ps,0),(e_s_AOS_pw,0),(e_s_AOS_pr,0),(e_s_AOS_pp,0.35),
      (e_s_FRS_cc,0.33),(e_s_FRS_cs,-0.04),(e_s_FRS_cw,-0.00),(e_s_FRS_cr,-0.03),(e_s_FRS_cp,0),
      (e_s_FRS_sc,-0.03),(e_s_FRS_ss,0.19),(e_s_FRS_sw,-0.01 ),(e_s_FRS_sr,-0.10 ),(e_s_FRS_sp,0),
      (e_s_FRS_wc,-0.02),(e_s_FRS_ws,-0.05 ),(e_s_FRS_ww,0.33),(e_s_FRS_wr,-0.04 ),(e_s_FRS_wp,0),
      (e_s_FRS_rc,-0.01 ), (e_s_FRS_rs,-0.05), (e_s_FRS_rw,-0.00 ), (e_s_FRS_rr,0.47),(e_s_FRS_rp,0),
      (e_s_FRS_pc,0),(e_s_FRS_ps,0),(e_s_FRS_pw,0),(e_s_FRS_pr,0),(e_s_FRS_pp,0.35)
      ])')

sympy('eqns = [corn,soy,wheat,rice,peanut]')

sympy('solve(eqns, EP_corn, EP_soy, EP_wheat, EP_rice, EP_peanut)')





