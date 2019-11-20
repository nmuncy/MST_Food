
# [1] "L_aLEC"  "L_CA1"   "L_ERC"   "L_Multi" "L_PHG"   "L_pMEC"  "L_Sub"   "R_aLEC" 
# "R_CA1"   "R_ERC"   "R_Multi" "R_PHG"   "R_pMEC"  "R_Sub" 
stats <- ezANOVA(Mdata_long[Mdata_long$Mask=="L_aLEC",],dv=Value,wid=Subj,between=Group,within=c(Congruency,Behavior),type='III')

i <- 8
ezANOVA(Mdata_long[Mdata_long$Mask==Mdata$mask.names[i],],dv=Value,wid=Subj,between=Group,within=c(Congruency,Behavior),type='III')$ANOVA


t.test( Mdata_long$Value[Mdata_long$Congruency=="Con" &  Mdata_long$Behavior=="Miss" & Mdata_long$Mask==Mdata$mask.names[i]],Mdata_long$Value[Mdata_long$Congruency=="Incon" & Mdata_long$Behavior=="Miss" & Mdata_long$Mask==Mdata$mask.names[i]],paired=T)




t.test( Mdata_long$Value[Mdata_long$Congruency=="Con" & Mdata_long$Mask==Mdata$mask.names[i]],Mdata_long$Value[Mdata_long$Congruency=="Incon" &  Mdata_long$Mask==Mdata$mask.names[i]],paired=T)




t.test( Mdata_long$Value[Mdata_long$Congruency=="Con" &  Mdata_long$Behavior=="Hit" & Mdata_long$Mask==Mdata$mask.names[i]],Mdata_long$Value[Mdata_long$Congruency=="Incon" & Mdata_long$Behavior=="Hit" & Mdata_long$Mask==Mdata$mask.names[i]],paired=T)

j <- HCmaster_list[6,1]




