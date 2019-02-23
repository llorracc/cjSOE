(* ::Package:: *)

(* This notebook contains various scripts (which do not produce figures) used in the cjSOE model *)

(* This script resets the parameters to their base values by referring to the file in the Calibrations directory. *)
resetParams:={
Get[CoreCodeDir<>"/ParametersBase.m"];
};


(* This script calculates various useful variables based on the parameters.  It should be re-run each time a parameter value is changed. *)
calculateValues:={
erate = 1 - \[Mho];
PGro = \[GothicCapitalG]*XperGro; (* individual wage growth if worker stays employed... see The Microeconomic Consumer's Problem, second paragraph *)
PLives = 1 - PDies; (* Probability that an individual survives another year... see Macroeconomic Assumptions, third paragraph *)
\[ScriptB]Targ = (PGro/(R)-1+\[Kappa]*(1+(\[CapitalThorn]\[CapitalGamma]^(-\[Rho])-1)/\[Mho])^(1/\[Rho]))^(-1);(* target wealth to income ratio for the employed consumer with no stake... This is a direct application of eq:bbar *)
\[ScriptC]Targ=1+(1-PGro/(R))*\[ScriptB]Targ;(* level of consumption at the target wealth level (no stake)... see Appendix: Saddle-Point Stability *) (* this is actually an application of eq:sss, equivalent to using eq:ssc and simpler*)
LGro = erate*XperGro/EmpGro; (* generational change in labor force participation... taken from eq:BetOyLevt, the underbrace. *)
\[ScriptB]TargTarg=(PGro/(R)-1/(2-LGro)+\[Kappa]*(1+(\[CapitalThorn]\[CapitalGamma]^(-\[Rho])-1)/\[Mho])^(1/\[Rho]))^(-1);(* target wealth to income ratio for the employed consumer with stakes... This is a direct application of eq:bTargEStakes *)
Tax=(1-LGro)/(2-LGro);(* tax rate for model with stakes... This is a direct application of eq:tau *)

(* See Appendix: Saddle-Point Stability.  This is the analog to the definition of \[ScriptC]Targ above, but now we use \[ScriptB]TargTarg rather than \[ScriptB]Targ and the consumer only keeps a (1 - Tax) portion of his wealth each period. *)
\[ScriptC]TargTarg=1+(1-Tax-PGro/(R))*\[ScriptB]TargTarg; (* level of consumption at the target wealth level (stakes) *)
KoverY = kapshare/((R)-DeprFac);(* ratio of capital to GDP... % This is a direct application of eq:KOyLev. *)
};
