(* ::Package:: *)

(* This script caculates the equilibrium interest in an economy with two different groups of people*)

(* This function computes the global ratio of net foreign assets to GDP in the model without stakes as a function of the interest rate and the time discount factor, with social insurance. *)
NoverYgeNS[tempR_,temp\[Beta]_]:=Block[{tempMPCU,temp\[CapitalThorn]\[CapitalGamma],tempKoverY,Alt\[ScriptB]Targ,\[ScriptCapitalB]overPe,\[ScriptCapitalB]overPu,\[ScriptCapitalB]overP,NoverY},
\[Beta]=temp\[Beta];
r=tempR-1;
calculateValues;

tempMPCU=1-PLives*(tempR*\[Beta])^(1/\[Rho])/tempR;(* Definition of MPCU as in eq:kappaDef. *)
temp\[CapitalThorn]\[CapitalGamma]=(\[Beta]*tempR)^(1/\[Rho])/PGro;(* Definition of \[CapitalThorn]\[CapitalGamma] as in eq:\[CapitalThorn]\[CapitalGamma]. *)
tempKoverY=kapshare/(tempR-DeprFac);(* This is a direct application of eq:KOyLev. *)

Alt\[ScriptB]Targ = \[ScriptB]Targ*(1-\[Stigma]H*(\[Mho]*XperGro/EmpGro+tempMPCU*(1+(temp\[CapitalThorn]\[CapitalGamma]^(-\[Rho])-1)/\[Mho])^(1/\[Rho])));(* the target level of wealth in the model without stakes, with social insurance; *)
\[ScriptCapitalB]overPe=(1-kapshare)*Alt\[ScriptB]Targ; (* the ratio of wealth to GDP for employed consumers... this is a direct application of eq:\[ScriptB]TargWithSocIns *)
\[ScriptCapitalB]overPu=(\[Mho]*EmpGro*\[GothicCapitalG])/(EmpGro*\[GothicCapitalG]-PLives*(\[Beta]*tempR)^(1/\[Rho]))*\[ScriptCapitalB]overPe; (* the ratio of wealth to GDP for unemployed consumers...  This is a direct application of eq:BuOyLev.*)
\[ScriptCapitalB]overP=\[ScriptCapitalB]overPe+\[ScriptCapitalB]overPu;(* ratio of wealth to GDP for all consumers of the more patient group*)

(* Compute the ratio of net foreign assets to GDP in the home country:
Inside the parentheses is a direct application of eq:NFALev.  We must scale by EmpGro*\[GothicCapitalG] because P grows by this factor in each period (\[ScriptCapitalB]overP and KoverY are for period t,but we want them in period t+1). *)
NoverY=EmpGro*\[GothicCapitalG]*(\[ScriptCapitalB]overP/tempR-tempKoverY);  (* !!! Here tempR is once misused as Rfree*)
Return[NoverY]
];


equiR = Table[x/.First[FindRoot[NoverYgeNS[x,1/1.02]+NoverYgeNS[x,1/1.06],{x,1.04}]]];
resetParams;
calculateValues;
