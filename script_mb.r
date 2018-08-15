%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Project:	Hosp_Specific Data Analytics
%%	Author:		James Wafula
%%	Date:		08 Arpil, 2016
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Subsets according to supplied criteria
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in_data <- subset(data.copy,hosp_id=="hospX DH")
attach(in_data)
%%write.csv(in_data, file="hospX_data.csv")
group_1 <- subset(in_data,is.na(temp)==F)
group_2 <- subset(in_data,is.na(temp)==T & ((fever=="Yes" & is.na(fever)==F) | (fever=="No" & is.na(fever)==F)))

group_2Y <- subset(in_data,is.na(temp)==T & ((fever=="Yes" & is.na(fever)==F)))
group_2N <- subset(in_data,is.na(temp)==T & ((fever=="No" & is.na(fever)==F)))

group_1_diarr <- subset(group_1,temp>=38.0 & (diarrhoea=="Yes" & is.na(diarrhoea)==F))

group_11 <- subset(group_1,temp>=38.0 & ((cough=="Yes" & is.na(cough)==F)|(diff_breath=="Yes" & is.na(diff_breath)==F)) & ((wheeze=="Yes" & is.na(wheeze)==F)|(stridor=="Yes" & is.na(stridor)==F)|(grunting=="Yes" & is.na(grunting)==F)|(indrawing=="Yes" & is.na(indrawing)==F)|(c_cyanosis=="Yes" & is.na(c_cyanosis)==F)|(crackles=="Yes" & is.na(crackles)==F)|(acidotic__breathing=="Yes" & is.na(acidotic__breathing)==F)))

group_1_diarr_resp <- subset(group_1,temp>=38.0 & (diarrhoea=="Yes" & is.na(diarrhoea)==F) & ((cough=="Yes" & is.na(cough)==F)|(diff_breath=="Yes" & is.na(diff_breath)==F)) & ((wheeze=="Yes" & is.na(wheeze)==F)|(stridor=="Yes" & is.na(stridor)==F)|(grunting=="Yes" & is.na(grunting)==F)|(indrawing=="Yes" & is.na(indrawing)==F)|(c_cyanosis=="Yes" & is.na(c_cyanosis)==F)|(crackles=="Yes" & is.na(crackles)==F)|(acidotic__breathing=="Yes" & is.na(acidotic__breathing)==F)))

group_21 <- subset(group_2,fever=="Yes" & ((cough=="Yes" & is.na(cough)==F)|(diff_breath=="Yes" & is.na(diff_breath)==F)) & ((wheeze=="Yes" & is.na(wheeze)==F)|(stridor=="Yes" & is.na(stridor)==F)|(grunting=="Yes" & is.na(grunting)==F)|(indrawing=="Yes" & is.na(indrawing)==F)|(c_cyanosis=="Yes" & is.na(c_cyanosis)==F)|(crackles=="Yes" & is.na(crackles)==F)|(acidotic__breathing=="Yes" & is.na(acidotic__breathing)==F)))

group_21_diarr <- subset(group_2,fever=="Yes" & (diarrhoea=="Yes" & is.na(diarrhoea)==F))

group_211 <- subset(group_2,fever=="No" & ((cough=="Yes" & is.na(cough)==F)|(diff_breath=="Yes" & is.na(diff_breath)==F)) & ((wheeze=="Yes" & is.na(wheeze)==F)|(stridor=="Yes" & is.na(stridor)==F)|(grunting=="Yes" & is.na(grunting)==F)|(indrawing=="Yes" & is.na(indrawing)==F)|(c_cyanosis=="Yes" & is.na(c_cyanosis)==F)|(crackles=="Yes" & is.na(crackles)==F)|(acidotic__breathing=="Yes" & is.na(acidotic__breathing)==F)))

group_211_diarr <- subset(group_2,fever=="No" & (diarrhoea=="Yes" & is.na(diarrhoea)==F))

group_21_diarr_resp <- subset(group_2,fever=="Yes" & (diarrhoea=="Yes" & is.na(diarrhoea)==F) & ((cough=="Yes" & is.na(cough)==F)|(diff_breath=="Yes" & is.na(diff_breath)==F)) & ((wheeze=="Yes" & is.na(wheeze)==F)|(stridor=="Yes" & is.na(stridor)==F)|(grunting=="Yes" & is.na(grunting)==F)|(indrawing=="Yes" & is.na(indrawing)==F)|(c_cyanosis=="Yes" & is.na(c_cyanosis)==F)|(crackles=="Yes" & is.na(crackles)==F)|(acidotic__breathing=="Yes" & is.na(acidotic__breathing)==F)))

SAM_set <- subset(in_data, ((is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | ((muac<11.5&is.na(muac)==F)&(ages.in.months>=6&ages.in.months<60)) | ((muac<11&is.na(muac)==F)&(ages.in.months>=2&ages.in.months<6)) | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Severe[ICD10: E43]") | ((is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Severe[ICD10: E43]") & (is.na(oedema)==F & as.factor(oedema)!="None"))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Subsets with SAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

group_1_SAM <- subset(group_1,  ((is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | ((muac<11.5&is.na(muac)==F)&(ages.in.months>=6&ages.in.months<60)) | ((muac<11&is.na(muac)==F)&(ages.in.months>=2&ages.in.months<6)) | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Severe[ICD10: E43]") | ((is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Severe[ICD10: E43]") & (is.na(oedema)==F & as.factor(oedema)!="None"))))

group_2_SAM <- subset(group_2,  ((is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | ((muac<11.5&is.na(muac)==F)&(ages.in.months>=6&ages.in.months<60)) | ((muac<11&is.na(muac)==F)&(ages.in.months>=2&ages.in.months<6)) | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Severe[ICD10: E43]") | ((is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Severe[ICD10: E43]") & (is.na(oedema)==F & as.factor(oedema)!="None"))))

group_2Y_SAM <- subset(group_2Y,  ((is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | ((muac<11.5&is.na(muac)==F)&(ages.in.months>=6&ages.in.months<60)) | ((muac<11&is.na(muac)==F)&(ages.in.months>=2&ages.in.months<6)) | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Severe[ICD10: E43]") | ((is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Severe[ICD10: E43]") & (is.na(oedema)==F & as.factor(oedema)!="None"))))

group_2N_SAM <- subset(group_2N,  ((is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | ((muac<11.5&is.na(muac)==F)&(ages.in.months>=6&ages.in.months<60)) | ((muac<11&is.na(muac)==F)&(ages.in.months>=2&ages.in.months<6)) | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Severe[ICD10: E43]") | ((is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Severe[ICD10: E43]") & (is.na(oedema)==F & as.factor(oedema)!="None"))))

group_11_SAM <- subset(group_11,  ((is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | ((muac<11.5&is.na(muac)==F)&(ages.in.months>=6&ages.in.months<60)) | ((muac<11&is.na(muac)==F)&(ages.in.months>=2&ages.in.months<6)) | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Severe[ICD10: E43]") | ((is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Severe[ICD10: E43]") & (is.na(oedema)==F & as.factor(oedema)!="None"))))

group_21_SAM <- subset(group_21,  ((is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | ((muac<11.5&is.na(muac)==F)&(ages.in.months>=6&ages.in.months<60)) | ((muac<11&is.na(muac)==F)&(ages.in.months>=2&ages.in.months<6)) | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Severe[ICD10: E43]") | ((is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Severe[ICD10: E43]") & (is.na(oedema)==F & as.factor(oedema)!="None"))))

group_211_SAM <- subset(group_211,  ((is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | ((muac<11.5&is.na(muac)==F)&(ages.in.months>=6&ages.in.months<60)) | ((muac<11&is.na(muac)==F)&(ages.in.months>=2&ages.in.months<6)) | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Severe[ICD10: E43]") | ((is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Severe[ICD10: E43]") & (is.na(oedema)==F & as.factor(oedema)!="None"))))

group_1_diarr_SAM <- subset(group_1_diarr, ((is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | ((muac<11.5&is.na(muac)==F)&(ages.in.months>=6&ages.in.months<60)) | ((muac<11&is.na(muac)==F)&(ages.in.months>=2&ages.in.months<6)) | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Severe[ICD10: E43]") | ((is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Severe[ICD10: E43]") & (is.na(oedema)==F & as.factor(oedema)!="None"))))

group_21_diarr_SAM <- subset(group_21_diarr, ((is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | ((muac<11.5&is.na(muac)==F)&(ages.in.months>=6&ages.in.months<60)) | ((muac<11&is.na(muac)==F)&(ages.in.months>=2&ages.in.months<6)) | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Severe[ICD10: E43]") | ((is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Severe[ICD10: E43]") & (is.na(oedema)==F & as.factor(oedema)!="None"))))

group_211_diarr_SAM <- subset(group_211_diarr, ((is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | ((muac<11.5&is.na(muac)==F)&(ages.in.months>=6&ages.in.months<60)) | ((muac<11&is.na(muac)==F)&(ages.in.months>=2&ages.in.months<6)) | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Severe[ICD10: E43]") | ((is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Severe[ICD10: E43]") & (is.na(oedema)==F & as.factor(oedema)!="None"))))

group_21_diarr_resp_SAM <- subset(group_21_diarr_resp, ((is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | ((muac<11.5&is.na(muac)==F)&(ages.in.months>=6&ages.in.months<60)) | ((muac<11&is.na(muac)==F)&(ages.in.months>=2&ages.in.months<6)) | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Severe[ICD10: E43]") | ((is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Severe[ICD10: E43]") & (is.na(oedema)==F & as.factor(oedema)!="None"))))

group_1_diarr_resp_SAM <- subset(group_1_diarr_resp, ((is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Kwashiorkor[ICD10: E40]") | (is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Marasmus-Kwashiorkor[ICD10: E42]") | ((muac<11.5&is.na(muac)==F)&(ages.in.months>=6&ages.in.months<60)) | ((muac<11&is.na(muac)==F)&(ages.in.months>=2&ages.in.months<6)) | (is.na(dsc_dx1_malnutr)==F & as.factor(dsc_dx1_malnutr)=="Severe[ICD10: E43]") | ((is.na(dsc_dx2_malnutr)==F & as.factor(dsc_dx2_malnutr)=="Severe[ICD10: E43]") & (is.na(oedema)==F & as.factor(oedema)!="None"))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SAM_temp38 <- subset(SAM_set, temp>=38.0 & is.na(temp)==F)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nrow(in_data)
nrow(group_1)
nrow(group_1_diarr)
nrow(group_1_diarr_resp)
nrow(group_2)
nrow(group_2Y)
nrow(group_2N)
nrow(group_11)
nrow(group_21)
nrow(group_21_diarr)
nrow(group_211)
nrow(group_211_diarr)
nrow(SAM_set)
nrow(group_1_SAM)
nrow(group_2_SAM)
nrow(group_2Y_SAM)
nrow(group_2N_SAM)
nrow(group_11_SAM)
nrow(group_21_SAM)
nrow(group_21_diarr_resp)
nrow(group_21_diarr_resp_SAM)
nrow(group_211_SAM)
nrow(group_21_diarr_SAM)
nrow(group_211_diarr_SAM)
nrow(group_1_diarr_resp_SAM)
nrow(SAM_temp38)


