(* ::Package:: *)

\[CurlyEpsilon]=0.0001; 
\[Mho]=\[Mho]Base= 0.025; 
\[CurlyTheta]=\[CurlyTheta]Base= 0.04;
r=rBase= 0.04; 
\[GothicG]=\[GothicG]Base=0.04; 
\[Rho]=\[Rho]Base= 2;
PDies = 0.05;
(*WGro = 1.02;*)
XperGro = 1.01;
EmpGro = 1.01;
\[Stigma] = 0;
\[Stigma]H = 1.5;
\[Stigma]F = 0.75;
kapshare = 0.3;
DeprFac = 0.94;
\[Tau]ForStake = 0; (*\[Tau]ForStake is the tax associated with stakes *)


CheckFor\[CapitalGamma]Impatience;
CheckForReturnImpatience;
{\[ScriptC]EBase,\[ScriptM]EBase} = {\[ScriptC]E,\[ScriptM]E};
VerboseOutput=False;
ParamsAreSet=True;

