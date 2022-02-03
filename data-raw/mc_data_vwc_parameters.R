mc_data_vwc_parameters <- as.data.frame(tibble::tribble(
         ~soiltype,        ~a,           ~b,           ~c,        ~rho, ~clay, ~silt, ~sand,                                                                            ~ref,
            "sand",    -3e-09,  0.000161192, -0.109956505, 1.520949867,     0,     0,   100,                           "Wild et al. (2019), 10.1016/j.agrformet.2018.12.018",
    "loamy sand A",  -1.9e-08,   0.00026561, -0.154089291, 0.518664463,   3.2,  24.9,  71.9,                           "Wild et al. (2019), 10.1016/j.agrformet.2018.12.018",
    "loamy sand B",  -2.3e-08,  0.000282473, -0.167211156, 0.971747212,   5.3,  28.2,  66.5,                           "Wild et al. (2019), 10.1016/j.agrformet.2018.12.018",
    "sandy loam A",  -3.8e-08,  0.000339449, -0.214921782, 1.320691976,   5.1,  33.7,  61.2,                           "Wild et al. (2019), 10.1016/j.agrformet.2018.12.018",
    "sandy loam B",    -9e-10,  0.000261847, -0.158618303, 1.069284738,   7.6, 35.65, 56.75,                           "Wild et al. (2019), 10.1016/j.agrformet.2018.12.018",
            "loam",  -5.1e-08,  0.000397984, -0.291046437,  1.55085324,  24.1,  28.5,  47.4,                           "Wild et al. (2019), 10.1016/j.agrformet.2018.12.018",
       "silt loam",   1.7e-08,  0.000118119, -0.101168511, 1.294285714,    13,    66,    21,                           "Wild et al. (2019), 10.1016/j.agrformet.2018.12.018",
            "peat",  1.23e-07, -0.000144644,  0.202927906,          NA,    NA,    NA,    NA,                           "Wild et al. (2019), 10.1016/j.agrformet.2018.12.018",
           "water",         0,    0.0003067,   -0.1349279,           1,    NA,    NA,    NA,                           "Wild et al. (2019), 10.1016/j.agrformet.2018.12.018",
       "universal", -1.34e-08,  0.000249622, -0.157888813,          NA,    NA,    NA,    NA,                        "Kopecký et al. (2021), 10.1016/j.scitotenv.2020.143785",
       "sand TMS1",         0,      0.00026,     -0.13304,          NA,    NA,    NA,    NA, "Vlček (2010) Kalibrace vlhkostního čidla TST1 pro minerální a organické půdy.",
 "loamy sand TMS1",         0,      0.00033,     -0.19389,          NA,    NA,    NA,    NA, "Vlček (2010) Kalibrace vlhkostního čidla TST1 pro minerální a organické půdy.",
  "silt loam TMS1",         0,      0.00038,     -0.29427,          NA,    NA,    NA,    NA, "Vlček (2010) Kalibrace vlhkostního čidla TST1 pro minerální a organické půdy."
))

usethis::use_data(mc_data_vwc_parameters, overwrite = TRUE)
