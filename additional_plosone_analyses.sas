/* Telephone reminder analysis */

libname rep1 "O:\AFHS-DATA\Caselevel_Data";

data phone;
   set rep1.afhs_caselevel;
   /* Full */
   if reachNRCallFull = 1 and NRcallContR_F = 0 then fullphone = 0; * entering phase 4 who did not get contacted by phone;
   if reachNRCallFull = 1 and NRcallContR_F = 1 then fullphone = 1; * entering phase 4 who did get contacted by phone;
   if FinalAaporCategory_F in ('I' 'P') and fullphone in (1,0) then Response_F = 1;
   else if fullphone in (1,0) then Response_F = 0;
   /* Module 1 */
   if reachNRCallMod1 = 1 and NRcallContR_m1 = 0 then mod1phone = 0; * entering phase 4 who did not get contacted by phone;
   if reachNRCallMod1 = 1 and NRcallContR_m1 = 1 then mod1phone = 1; * entering phase 4 who did get contacted by phone;
   if FinalAaporCategory_M1 in ('I' 'P') and mod1phone in (1,0) then Response_m1 = 1;
   else if mod1phone in (1,0) then Response_m1 = 0;
   /* Module 2 */
   if reachNRCallMod2 = 1 and NRcallContR_m2 = 0 then mod2phone = 0; * entering phase 4 who did not get contacted by phone;
   if reachNRCallMod2 = 1 and NRcallContR_m2 = 1 then mod2phone = 1; * entering phase 4 who did get contacted by phone;
   if FinalAaporCategory_M2 in ('I' 'P') and mod2phone in (1,0) then Response_m2 = 1;
   else if mod2phone in (1,0) then Response_m2 = 0;
   /* Module 3 */
   if reachNRCallMod3 = 1 and NRcallContR_m3 = 0 then mod3phone = 0; * entering phase 4 who did not get contacted by phone;
   if reachNRCallMod3 = 1 and NRcallContR_m3 = 1 then mod3phone = 1; * entering phase 4 who did get contacted by phone;
   if FinalAaporCategory_M3 in ('I' 'P') and mod3phone in (1,0) then Response_m3 = 1;
   else if mod3phone in (1,0) then Response_m3 = 0;
run;

/* Save de-identified data file to accompany PLOS ONE revision */

data rep1.phone (keep = fullphone Response_F mod1phone Response_m1 mod2phone Response_m2 mod3phone Response_m3
   CensusRegion Domain Low_Response_Score SelectedMemberAge SelectedMemberSex SelectedMemberHisp
   ScreenerMode FullMode FinalAaporCategory_F FinalAaporCategory_M1 FinalAaporCategory_M2 FinalAaporCategory_M3 
   Mod1Mode Mod2Mode Mod3Mode);
   set phone;
run;

proc freq data = rep1.phone;
   tables fullphone*Response_F / chisq; /* Significant increase for full cases */
run;

proc freq data = rep1.phone;
   tables mod1phone*Response_m1 / chisq; /* Significant increase for mod1 cases */
run;

proc freq data = rep1.phone;
   tables mod2phone*Response_m2 / chisq; /* Significant increase for mod2 cases */
run;

proc freq data = rep1.phone;
   tables mod3phone*Response_m3 / chisq; /* Significant increase for mod3 cases */
run;

/* Multivariable Models, with interaction tests */
proc logistic data = rep1.phone;
   class CensusRegion Domain SelectedMemberSex SelectedMemberHisp / param = ref;
   model Response_F(event = "1") = fullphone CensusRegion Domain Low_Response_Score 
      SelectedMemberAge SelectedMemberSex SelectedMemberHisp /*fullphone*SelectedMemberHisp*/;
   where fullphone in (1,0); 
run;

proc logistic data = rep1.phone;
   class CensusRegion Domain SelectedMemberSex SelectedMemberHisp / param = ref;
   model Response_m1(event = "1") = mod1phone CensusRegion Domain Low_Response_Score 
      SelectedMemberAge SelectedMemberSex SelectedMemberHisp /*fullphone*SelectedMemberHisp*/;
   where mod1phone in (1,0); 
run;

proc logistic data = rep1.phone;
   class CensusRegion Domain SelectedMemberSex SelectedMemberHisp / param = ref;
   model Response_m2(event = "1") = mod2phone CensusRegion Domain Low_Response_Score 
      SelectedMemberAge SelectedMemberSex SelectedMemberHisp /*fullphone*SelectedMemberHisp*/;
   where mod2phone in (1,0); 
run;

proc logistic data = rep1.phone;
   class CensusRegion Domain SelectedMemberSex SelectedMemberHisp / param = ref;
   model Response_m3(event = "1") = mod3phone CensusRegion Domain Low_Response_Score 
      SelectedMemberAge SelectedMemberSex SelectedMemberHisp /*fullphone*SelectedMemberHisp*/;
   where mod3phone in (1,0); 
run;

/* Mode Analyses */

proc freq data = rep1.phone;
   tables ScreenerMode*FullMode / chisq;
   where FinalAaporCategory_F in ('I' 'P');
run;

proc freq data = rep1.phone;
   tables ScreenerMode*Mod1Mode / chisq;
   where FinalAaporCategory_M1 in ('I' 'P');
run;

proc freq data = rep1.phone;
   tables ScreenerMode*Mod2Mode / chisq;
   where FinalAaporCategory_M2 in ('I' 'P');
run;

proc freq data = rep1.phone;
   tables ScreenerMode*Mod3Mode / chisq;
   where FinalAaporCategory_M3 in ('I' 'P');
run;





