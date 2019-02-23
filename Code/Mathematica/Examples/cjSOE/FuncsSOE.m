(* ::Package:: *)

(* This notebook contains all of the functions used in the cjSOE model *)

(* This function computes the ratio of workers' wealth to GDP, in the model without stakes *)
find\[ScriptB]Targ[]:=Block[{maxt,i,\[ScriptB]path,\[ScriptC]path,\[ScriptCapitalB]},
maxt = 100; (* The highest period of time in the path of wealth and consumption *)
FindStableArm;
SimGeneratePath[0,maxt];

\[ScriptB]path = Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],{2,maxt+2}];
\[ScriptC]path = Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],{2,maxt+2}];

\[ScriptCapitalB] = 0; (* Initialize the ratio at zero *)

(* This loop is a finite approximation to the infinite summation in eq:BeOyLevNostake.  Each generation is geometrically weighted by LGro to represent that employed generations born earlier
and thus further along the path from zero wealth to the target wealth) are smaller due to labor force exit and generational growth. *)
For[i=1,i<=maxt,i++,
\[ScriptCapitalB] = \[ScriptCapitalB] + LGro^(i-1)*\[ScriptB]path[[i]];
];
(* Because the loop has only a finite number of generations, we must account for the part of the employed population that is very very close to the target level of assets.  After an individual has been employed for 101 periods, his wealth has reached 99.999% of the target level, so we treat generations 101, 102, 103,... as if they are exactly at the target wealth level.  These generations have a total weight of (LGro^100)/(1-LGro). *)
\[ScriptCapitalB] = \[ScriptCapitalB] + LGro^maxt/(1-LGro)*\[ScriptB]Targ;
(* Finally, multiply by the two factors in front of the summation in eq:BeOyLevNostake. *)
\[ScriptCapitalB] = (1 - kapshare)*(1 - LGro)*\[ScriptCapitalB];
Return[\[ScriptCapitalB]]
];


(* This function computes the ratio of employed consumers' wealth to GDP in the model with stakes *)
find\[ScriptB]TargTarg[]:=(1 - kapshare)*\[ScriptB]TargTarg;(* This is a direct application of eq:SRatEStakes. *)


(* This function finds the ratio of net foreign assets to GDP in the model with no stakes. *)
NoverYns[]:=Block[{\[ScriptCapitalB]overYe,\[ScriptCapitalB]overYu,\[ScriptCapitalB]overY,NoverY},
\[ScriptCapitalB]overYe = find\[ScriptB]Targ[]; (* the country's ratio of wealth to GDP (for employed consumers) in the model with no stakes. *)
\[ScriptCapitalB]overYu = (\[Mho]*EmpGro*\[GothicCapitalG])/(EmpGro*\[GothicCapitalG]-PLives*(\[Beta]*(R))^(1/\[Rho]))*\[ScriptCapitalB]overYe; (* This is a direct application of eq:BuOyLev. *)
\[ScriptCapitalB]overY = \[ScriptCapitalB]overYe + \[ScriptCapitalB]overYu;

(* Inside the parentheses is a direct application of eq:NFALev.  We must scale by EmpGro*WGro because Y grows by this factor each period (\[ScriptCapitalB]overY and KoverY are for period t, but we want them in period t+1). *)
NoverY=EmpGro*\[GothicCapitalG]*(\[ScriptCapitalB]overY/(R)-KoverY);
Return[NoverY]
];


(* This function finds the ratio of net foreign assets to GDP in the model with stakes. *)
NoverYws[]:=Block[{\[ScriptCapitalB]overYe,\[ScriptCapitalB]overYu,\[ScriptCapitalB]overY,NoverY}, (* the only difference with NoverYns is the definition of \[ScriptCapitalB]overYe*)
\[ScriptCapitalB]overYe = find\[ScriptB]TargTarg[]; (* the country's ratio of wealth to GDP (for employed consumers) in the model with stakes. *)
\[ScriptCapitalB]overYu = (\[Mho]*EmpGro*\[GothicCapitalG])/(EmpGro*\[GothicCapitalG]-PLives*(\[Beta]*(R))^(1/\[Rho]))*\[ScriptCapitalB]overYe; (* This is a direct application of eq:BuOyLev. *)
\[ScriptCapitalB]overY = \[ScriptCapitalB]overYe + \[ScriptCapitalB]overYu;

(* Inside the parentheses is a direct application of eq:NFALev.  We must scale by EmpGro*WGro because P grows by this factor each period (\[ScriptCapitalB]overY and KoverY are for period t, but we want them in period t+1). *)
NoverY=EmpGro*\[GothicCapitalG]*(\[ScriptCapitalB]overY/(R)-KoverY);
Return[NoverY]
];


(* This function computes the global ratio of net foreign assets to GDP in the model with stakes as a function of the interest rate, with social insurance. *)
NoverYge[tempR_]:=Block[{tempMPCU,temp\[CapitalThorn]\[CapitalGamma],tempKoverY,Alt\[ScriptB]TargH,\[ScriptCapitalB]overYeH,\[ScriptCapitalB]overYuH,\[ScriptCapitalB]overYH,Alt\[ScriptB]TargF,\[ScriptCapitalB]overYeF,\[ScriptCapitalB]overYuF,\[ScriptCapitalB]overYF,NoverYH,NoverYF,NoverYworld},
tempMPCU=1-PLives*(tempR*\[Beta])^(1/\[Rho])/tempR;(* Definition of MPCU as in eq:kappaDef. *)
temp\[CapitalThorn]\[CapitalGamma]=(\[Beta]*tempR)^(1/\[Rho])/PGro;(* Definition of \[CapitalThorn]\[CapitalGamma] as in eq:\[CapitalThorn]\[CapitalGamma]. *)
tempKoverY=kapshare/(tempR-DeprFac);(* This is a direct application of eq:KOyLev. *)

(* Compute the long-term ratio of wealth to GDP with social insurance for the home country: 
This is a direct application of eq:\[ScriptB]TargWithSocIns. *)
Alt\[ScriptB]TargH = \[ScriptB]TargTarg*(1-\[Stigma]H*(\[Mho]*XperGro/EmpGro+tempMPCU*(1+(temp\[CapitalThorn]\[CapitalGamma]^(-\[Rho])-1)/\[Mho])^(1/\[Rho])));(* the target level of wealth in the model with stakes, with social insurance; *)
\[ScriptCapitalB]overYeH=(1-kapshare)*Alt\[ScriptB]TargH; (* the ratio of wealth to GDP for employed consumers... this is a direct application of eq:\[ScriptB]TargWithSocIns *)
\[ScriptCapitalB]overYuH=(\[Mho]*EmpGro*\[GothicCapitalG])/(EmpGro*\[GothicCapitalG]-PLives*(\[Beta]*tempR)^(1/\[Rho]))*\[ScriptCapitalB]overYeH; (* the ratio of wealth to GDP for unemployed consumers...  This is a direct application of eq:BuOyLev.*)
\[ScriptCapitalB]overYH=\[ScriptCapitalB]overYeH+\[ScriptCapitalB]overYuH;(* ratio of wealth to GDP for all consumers *)

(* Compute the ratio of net foreign assets to GDP in the home country:
Inside the parentheses is a direct application of eq:NFALev.  We must scale by EmpGro*WGro because P grows by this factor in each period (\[ScriptCapitalB]overY and KoverY are for period t,but we want them in period t+1). *)
NoverYH=EmpGro*\[GothicCapitalG]*(\[ScriptCapitalB]overYH/tempR-tempKoverY);

(* Compute the long-term ratio of wealth to GDP with social insurance for the foreign country: 
This is a direct application of eq:\[ScriptB]TargWithSocIns. *)
Alt\[ScriptB]TargF = \[ScriptB]TargTarg*(1-\[Stigma]F*(\[Mho]*XperGro/EmpGro+tempMPCU*(1+(temp\[CapitalThorn]\[CapitalGamma]^(-\[Rho])-1)/\[Mho])^(1/\[Rho])));(* the target level of wealth in the model with stakes, with social insurance; *)
\[ScriptCapitalB]overYeF=(1-kapshare)*Alt\[ScriptB]TargF; (* the ratio of wealth to GDP for employed consumers... this is a direct application of eq:\[ScriptB]TargWithSocIns *)
\[ScriptCapitalB]overYuF=(\[Mho]*EmpGro*\[GothicCapitalG])/(EmpGro*\[GothicCapitalG]-PLives*(\[Beta]*tempR)^(1/\[Rho]))*\[ScriptCapitalB]overYeF; (* the ratio of wealth to GDP for unemployed consumers...  This is a direct application of eq:BuOyLev.*)
\[ScriptCapitalB]overYF=\[ScriptCapitalB]overYeF+\[ScriptCapitalB]overYuF;(* ratio of wealth to GDP for all consumers *)

(* Compute the ratio of net foreign assets to GDP in the foreign country:
Inside the parentheses is a direct application of eq:NFALev.  We must scale by EmpGro*WGro because P grows by this factor in each period (\[ScriptCapitalB]overY and KoverY are for period t,but we want them in period t+1). *)
NoverYF=EmpGro*\[GothicCapitalG]*(\[ScriptCapitalB]overYF/tempR-tempKoverY);

(* Note that the calculation of NoverYH and NoverYF can be achieved more directly through eq:FOyLev once we have \[ScriptCapitalB]overYeH and \[ScriptCapitalB]overYeF.  The longer form is used in order to demonstrate the derivation of eq:FOyLev *)

(* Compute the ratio of net foreign assets to GDP in the world: 
See Long-term Impact of Reducing Global Imbalances, in which we state that the home country accounts for 20% of global GDP, while the foreign country accounts for the remaining 80%. *)
NoverYworld = 0.2*NoverYH + 0.8*NoverYF;
Return[NoverYworld]
];
