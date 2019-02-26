(* ::Package:: *)

If[NameQ["ParamsAreSet"]==True,If[ParamsAreSet==True,Print["This file should be executed before parameter values are defined."];Abort[]]]


(* Variables *)

(* \[Lambda] is the MPS for perfect foresight problem (like unemployed consumers); *)
(* \[Kappa] is the MPC for perfect foresight problem (like unemployed consumers)  *)
\[Lambda]=((R)^-1) ((R) \[Beta])^(1/\[Rho]) (1-PDies); 
(* Note that in a Blanchard model, the interest rate perceived by retired agents is R/(1-PDies) and the discount factor is \[Beta] (1-PDies). That is why the MPS
 takes the form of the above formula. *)
\[Kappa]=1-\[Lambda];

(* \[CapitalGamma] is the growth factor conditional on remaining employed *)
\[CapitalGamma]=\[GothicCapitalG] XperGro; (*\[GothicCapitalG]/(1-\[Mho])*)
\[Tau]ForUI = \[Stigma] \[Mho]/EmpGro; (* Unemployment insurance tax rate needed to raise revenues to pay severance benefit \[Stigma] *)


(* \[ScriptCapitalR] is the return factor for the normalized problem *)
(* (R) is the return factor in levels (in parens to allow search-and-replace) *)
\[ScriptCapitalR]=(R)/\[CapitalGamma];
\[CapitalThorn]\[CapitalGamma] = ((R) \[Beta])^(1/\[Rho])/\[CapitalGamma];

(* \[ScriptCapitalP] is the growth patience factor *)
\[ScriptCapitalP]Growth=((R) \[Beta])^(1/\[Rho])/\[CapitalGamma];
\[ScriptCapitalP]Return=((R) \[Beta])^(1/\[Rho])/(R);
\[WeierstrassP]\[Gamma]=Log[\[ScriptCapitalP]Growth];
\[WeierstrassP]rtntn=Log[\[ScriptCapitalP]Return];
\[CapitalPi]Alt=(1+(\[ScriptCapitalP]Growth^-\[Rho]-1)/\[Mho])^(1/\[Rho]);
\[GothicH]=(1/(1-\[CapitalGamma]/(R))) (1-\[Tau]ForUI);  (* Human wealth, as a ratio to permanent labor income (\[Tau]ForUI is proportional to wages and thus to permanent income) *)
\[Zeta]=\[ScriptCapitalR] \[Kappa] \[CapitalPi]Alt; (* A useful constant *)
\[Beta] = 1/(1+\[CurlyTheta]); (* Time preference factor *)
(R)= 1+(r);
\[GothicCapitalG]= 1+\[GothicG];

(* Target values of \[ScriptB]E,\[ScriptC]E,\[ScriptA]E,\[ScriptB]E,\[ScriptB]U,\[ScriptC]U *)
\[ScriptB]E=(1-\[Stigma] (\[Mho]/EmpGro+\[Kappa] (1+(\[CapitalThorn]\[CapitalGamma]^(-\[Rho])-1)/\[Mho])^(1/\[Rho]))) (\[CapitalGamma]/(R)-(1-\[Tau]ForStake)+\[Kappa] (1+(\[CapitalThorn]\[CapitalGamma]^(-\[Rho])-1)/\[Mho])^(1/\[Rho]))^(-1); 
\[ScriptC]E= 1-\[Tau]ForUI+((1-\[Tau]ForStake)-(1/\[ScriptCapitalR])) \[ScriptB]E; (* Tax for stakes is proportional to wealth; in steady state, can only consume out of after-tax wealth *)
\[ScriptA]E=(1-\[Tau]ForStake)\[ScriptB]E-\[ScriptC]E+(1-\[Tau]ForUI);
\[ScriptY]E=\[ScriptA]E (\[ScriptCapitalR]-1) + (1-\[Tau]ForUI);
\[ScriptX]E=Chop[\[ScriptY]E-\[ScriptC]E];
\[ScriptB]U=((1-\[Tau]ForStake)\[ScriptB]E-\[ScriptC]E+(1-\[Tau]ForUI))\[ScriptCapitalR]+\[Stigma];
\[ScriptC]U=\[ScriptB]U \[Kappa];
\[GothicV] =1/(1-\[Beta] (1-PDies) (((R) \[Beta] (1-PDies))^((1/\[Rho])-1)));
\[ScriptV]U=u[\[ScriptC]U] \[GothicV];
\[ScriptV]E=(u[\[ScriptC]E] + \[Beta] (\[CapitalGamma]^(1-\[Rho])) \[Mho] vUPF[\[ScriptA]E \[ScriptCapitalR]])/(1-\[Beta] (\[CapitalGamma]^(1-\[Rho]) (1-\[Mho])));
                                                                                                                                                   
(* Combined discount factor for normalized problem *)                                                                                              
\[Bet] = \[ScriptCapitalR] \[Beta] \[CapitalGamma]^(1-\[Rho]);

