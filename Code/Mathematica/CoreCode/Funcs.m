(* ::Package:: *)

If[NameQ["ParamsAreSet"]==True,If[ParamsAreSet==True,Print["This file should be executed before parameter values are defined."];Abort[]]]


(* Functions *)
(* Now define the functions used in solving the model *)

(* Precautionary saving is the difference between consumption of the perfect foresight consumer and the consumer facing uncertainty *)
psavEfrombInterp[\[ScriptB]_] := cEfrombPF[\[ScriptB]]-cEfrombInterp[\[ScriptB]];

(* Mathematica's built-in differentiation operator ' does not work well with conditionals, so define them explicitly *)
psavEfromb'''[\[ScriptB]_] := psavEfrombInterp'''[\[ScriptB]] /; \[ScriptB] < \[ScriptB]Top;  (* Conditional definition, see Mathematica definition of /;*)
psavEfromb'''[\[ScriptB]_] := psavEfrombExtrap'''[\[ScriptB]] /; \[ScriptB] >= \[ScriptB]Top;
psavEfromb''[\[ScriptB]_] := psavEfrombInterp''[\[ScriptB]] /; \[ScriptB] < \[ScriptB]Top;
psavEfromb''[\[ScriptB]_] := psavEfrombExtrap''[\[ScriptB]] /; \[ScriptB] >= \[ScriptB]Top;
psavEfromb'[\[ScriptB]_] := psavEfrombInterp'[\[ScriptB]] /; \[ScriptB] < \[ScriptB]Top;
psavEfromb'[\[ScriptB]_] := psavEfrombExtrap'[\[ScriptB]] /; \[ScriptB] >= \[ScriptB]Top;
psavEfromb[\[ScriptB]_] := psavEfrombInterp[\[ScriptB]] /; \[ScriptB] < \[ScriptB]Top;
psavEfromb[\[ScriptB]_] := psavEfrombExtrap[\[ScriptB]] /; \[ScriptB] >= \[ScriptB]Top;

(* Find limiting MPC as \[ScriptB] approaches zero *)
\[Natural]EFuncLim0[\[Kappa]Et_] := \[Bet] \[ScriptCapitalR] \[Mho] (\[Kappa] \[ScriptCapitalR] ((1-\[Kappa]Et)/\[Kappa]Et))^(-\[Rho]-1) \[Kappa];
(* The first version of the function below is easier to understand, but under some parameter values FindRoot tries \[Kappa]E = 1 which blows up *)
(* The second version of the function deals with this by a value which is exponentiated to generate \[Kappa]E -- this can never reach 1 *)
\[Kappa]ELim0Find := \[Kappa]ESeek /.  FindRoot[ \[Kappa]ESeek == \[Natural]EFuncLim0[\[Kappa]ESeek]/(1 + \[Natural]EFuncLim0[\[Kappa]ESeek]), {\[Kappa]ESeek, 0.9, 0.99999999999999}];
\[Kappa]ELim0Find := Block[{\[Kappa]ELim0Soln},
  Off[FindRoot::cvmit]; (* Turn off warning about insufficient accuracy *)
  \[Kappa]ELim0Soln=Exp[-1/\[Kappa]EInvSeek/.FindRoot[Exp[-1/\[Kappa]EInvSeek]==\[Natural]EFuncLim0[Exp[-1/\[Kappa]EInvSeek]]/(1+\[Natural]EFuncLim0[Exp[-1/\[Kappa]EInvSeek]]),{\[Kappa]EInvSeek,0.5,100}]];
  On[FindRoot::cvmit]; (* Turn warning back on *)
  Return[\[Kappa]ELim0Soln]
];

CheckFor\[CapitalGamma]Impatience  :=If[\[ScriptCapitalP]Growth>= 1.,Print["Aborting: Employed Consumer Not Growth Impatient."];Abort[]];
CheckForReturnImpatience:=If[\[ScriptCapitalP]Return>= 1.,Print["Aborting: Employed Consumer Not Return Impatient."];Abort[]];

(* Define utility function from highest to lowest derivative to avoid Mathematica difficulties *)
Clear[u];
u'''[\[ScriptC]_] := -\[Rho](-\[Rho]-1) \[ScriptC]^(-\[Rho]-2);
u''[\[ScriptC]_] := -\[Rho] \[ScriptC]^(-\[Rho]-1);
u'[\[ScriptC]_] := \[ScriptC]^-\[Rho];
u[\[ScriptC]_] := If[ Chop[\[Rho]-1] != 0, (\[ScriptC]^(1-\[Rho]))/(1-\[Rho]), Log[\[ScriptC]] ];

ClearAll[cEfromb];
cEfromb'''[\[ScriptB]_] := cEfrombInterp'''[\[ScriptB]] /; \[ScriptB] < \[ScriptB]Top;
cEfromb'''[\[ScriptB]_] := -psavEfromb'''[\[ScriptB]] /; \[ScriptB] >= \[ScriptB]Top;
cEfromb''[\[ScriptB]_] := cEfrombInterp''[\[ScriptB]] /; \[ScriptB] < \[ScriptB]Top;
cEfromb''[\[ScriptB]_] := -psavEfromb''[\[ScriptB]] /; \[ScriptB] >= \[ScriptB]Top;
cEfromb'[\[ScriptB]_] := cEfrombInterp'[\[ScriptB]] /; \[ScriptB] < \[ScriptB]Top;
cEfromb'[\[ScriptB]_] := \[Kappa]-psavEfromb'[\[ScriptB]] /; \[ScriptB] >= \[ScriptB]Top;
cEfromb[\[ScriptB]_] := cEfrombInterp[\[ScriptB]] /; \[ScriptB] < \[ScriptB]Top;
cEfromb[\[ScriptB]_] := cEfrombPF[\[ScriptB]]-psavEfromb[\[ScriptB]] /; \[ScriptB] >= \[ScriptB]Top;
SetAttributes[cEfromb,Listable]; (* Allows function to be applied to a list and return a list *)

cEfrombExtrap[\[ScriptB]_] := cEfrombPF[\[ScriptB]]-psavEfrombExtrap[\[ScriptB]];
cEfrombPF[\[ScriptB]_] := (\[ScriptB]+\[GothicH]) \[Kappa]; 
vEfrombPF[\[ScriptB]_] := u[ cEfrombPF[\[ScriptB]]] \[GothicV]+If[ Chop[\[Rho]-1] == 0, Log[ (R) \[Beta] ] (\[Beta]/((\[Beta]-1)^2)),0];
cUPF[\[ScriptB]_]  := \[Kappa] \[ScriptB];
vUPF[\[ScriptB]_]  := u[ \[Kappa] \[ScriptB] ] \[GothicV]+If[ Chop[\[Rho]-1] == 0, Log[ (R) \[Beta] ] (\[Beta]/((\[Beta]-1)^2)),0];
\[ScriptC]EDelEqZero[\[ScriptB]_] := 1/(1+\[CapitalGamma]/(\[Kappa]*(R))*(1+(\[CapitalGamma]^\[Rho]/((R) \[Beta])-1)/\[Mho])^(-1/\[Rho]))*(1+(1-\[Tau]ForStake)\[ScriptB]-\[Tau]ForUI+\[Stigma]/\[ScriptCapitalR]); 
\[ScriptB]EDelEqZero[\[ScriptB]_] := 1-\[Tau]ForUI+((1-\[Tau]ForStake)-\[CapitalGamma]/(R))\[ScriptB]; (*(\[CapitalGamma]/(R))+(1-\[CapitalGamma]/(R))\[ScriptB]*)
\[ScriptB]Etp1Fromt[\[ScriptB]Et_,\[ScriptC]Et_] := ((1-\[Tau]ForStake)\[ScriptB]Et-\[ScriptC]Et+1-\[Tau]ForUI) \[ScriptCapitalR]; (*(\[ScriptB]Et-\[ScriptC]Et)\[ScriptCapitalR]+1.* Dynamic budget constraint *)
\[ScriptC]EtFromtp1[\[ScriptB]Etp1_,\[ScriptC]Etp1_] :=\[CapitalGamma]/(\[Beta] (R))^(1/\[Rho])*((1-\[Mho])*\[ScriptC]Etp1^(-\[Rho])+\[Mho]*(\[Kappa] (\[ScriptB]Etp1+\[Stigma]))^(-\[Rho]))^(-1/\[Rho]) ;   
\[ScriptB]EtFromtp1[\[ScriptB]Etp1_,\[ScriptC]Etp1_] := ((\[CapitalGamma]/(R))\[ScriptB]Etp1-1+\[Tau]ForUI+ \[ScriptC]EtFromtp1[\[ScriptB]Etp1,\[ScriptC]Etp1])/(1-\[Tau]ForStake);   
\[Kappa]EtFromtp1[\[ScriptB]Etp1_,\[ScriptC]Etp1_,\[Kappa]Etp1_,\[ScriptB]Et_,\[ScriptC]Et_] := Block[{\[ScriptC]Utp1 = \[Kappa] (((1-\[Tau]ForStake)\[ScriptB]Et-\[ScriptC]Et+1-\[Tau]ForUI) \[ScriptCapitalR]+\[Stigma]),\[Natural]},
  \[Natural] =  \[Bet] \[ScriptCapitalR] (1/u''[\[ScriptC]Et]) ((1-\[Mho]) u''[\[ScriptC]Etp1] \[Kappa]Etp1 + \[Mho] u''[\[ScriptC]Utp1] \[Kappa]);
  Return[\[Natural]/(1+\[Natural])]
]; 
\[Kappa]EPtFromtp1[\[ScriptB]Etp1_,\[ScriptC]Etp1_,\[Kappa]Etp1_,\[ScriptB]Et_,\[ScriptC]Et_,\[Kappa]Et_,\[Kappa]EPtp1_] := Block[{\[ScriptC]Utp1=(((1-\[Tau]ForStake)\[ScriptB]Et-\[ScriptC]Et+1-\[Tau]ForUI) \[ScriptCapitalR]+\[Stigma]) \[Kappa]},
   (\[Bet] \[ScriptCapitalR]^2 (1-\[Kappa]Et)^2 (\[Mho] \[Kappa]^2 u'''[\[ScriptC]Utp1]+(1-\[Mho]) \[Kappa]Etp1^2 u'''[\[ScriptC]Etp1]+(1-\[Mho]) u''[\[ScriptC]Etp1] \[Kappa]EPtp1 )-(\[Kappa]Et)^2 u'''[\[ScriptC]Et])/
   (u''[\[ScriptC]Et]+\[Bet] \[ScriptCapitalR] (\[Mho] \[Kappa] u''[\[ScriptC]Utp1]+(1-\[Mho]) \[Kappa]Etp1 u''[\[ScriptC]Etp1]))
];
cEfrombTaylorNearTarget[\[FilledUpTriangle]_] := \[ScriptC]E+\[FilledUpTriangle] \[Kappa]E +(1/2) (\[FilledUpTriangle]^2)\[Kappa]EP+(1/6) (\[FilledUpTriangle]^3) \[Kappa]EPP +(1/24)(\[FilledUpTriangle]^4)\[Kappa]EPPP;

vEfromb'[\[ScriptB]_] := u'[cEfromb[\[ScriptB]]]; (* Envelope result *)
vEfromb[\[ScriptB]_] := \[ScriptV]Bot + NIntegrate[u'[cEfrombInterp[\[ScriptB]\[Bullet]]],{\[ScriptB]\[Bullet],\[ScriptB]Vec[[1]], \[ScriptB]}] /; \[ScriptB] < \[ScriptB]Vec[[1]]; (* If \[ScriptB] is below the bottom, use consumption function to construct \[ScriptV] *)
vEfromb[\[ScriptB]_] := vEfrombInterp[\[ScriptB]] /; \[ScriptB]Vec[[-1]] >=  \[ScriptB] >=  \[ScriptB]Vec[[1]]; (* If in interior of interpolating points, use interpolating approximation *)
vEfromb[\[ScriptB]_] := \[ScriptV]Top + NIntegrate[u'[cEfrombInterp[\[ScriptB]\[Bullet]]],{\[ScriptB]\[Bullet],\[ScriptB]Vec[[-1]], \[ScriptB]}] /; \[ScriptB] > \[ScriptB]Vec[[-1]]; (* If \[ScriptB] is above the top, use consumption function to construct \[ScriptV] *)

(* BackShoot iterates the reverse Euler equation until it reaches a point outside some predefined boundaries *)

BackShoot[InitialPoints_] := Block[{\[ScriptB]Prev,\[ScriptC]Prev,\[Kappa]Prev,\[ScriptV]Prev,\[Kappa]PPrev,\[ScriptB]MaxBound=10,\[ScriptB]MinBound=0.1,Counter = 0,PointsList =InitialPoints},
 {\[ScriptB]Prev,\[ScriptC]Prev,\[Kappa]Prev,\[ScriptV]Prev,\[Kappa]PPrev} = Take[InitialPoints[[-1]],5];
 If[VerboseOutput != False, Print["Solving ..."]];
 While[
  \[ScriptB]Prev > -\[Stigma]+ 0.5 && \[ScriptB]Prev <= \[ScriptB]MaxBound Abs[\[ScriptB]E],
    AppendTo[
        PointsList
          ,{\[ScriptB]Now,\[ScriptC]Now} = {\[ScriptB]EtFromtp1[\[ScriptB]Prev,\[ScriptC]Prev],\[ScriptC]EtFromtp1[\[ScriptB]Prev,\[ScriptC]Prev]};
          \[Kappa]Now = \[Kappa]EtFromtp1[\[ScriptB]Prev,\[ScriptC]Prev,\[Kappa]Prev,\[ScriptB]Now,\[ScriptC]Now];
          \[Kappa]PNow = \[Kappa]EPtFromtp1[\[ScriptB]Prev,\[ScriptC]Prev,\[Kappa]Prev,\[ScriptB]Now,\[ScriptC]Now,\[Kappa]Now,\[Kappa]PPrev];
          \[ScriptV]Now = u[\[ScriptC]Now] + \[Beta] (\[CapitalGamma]^(1-\[Rho])) ((1-\[Mho]) \[ScriptV]Prev + \[Mho] vUPF[((1-\[Tau]ForStake)\[ScriptB]Now-\[ScriptC]Now+1-\[Tau]ForUI)\[ScriptCapitalR]+\[Stigma] ]);
          {\[ScriptB]Prev,\[ScriptC]Prev,\[Kappa]Prev,\[ScriptV]Prev,\[Kappa]PPrev} = {\[ScriptB]Now,\[ScriptC]Now,\[Kappa]Now,\[ScriptV]Now,\[Kappa]PNow}
    ];
    If[\[ScriptB]Prev<=\[ScriptB]MinBound  \[ScriptB]E && VerboseOutput != False
      ,Print["  Below \[ScriptB]MinBound times \[ScriptB]E after ",Counter," backwards Euler iterations."];
       Print["Last Point:",PointsList[[-1]]]];
    If[\[ScriptB]Prev > \[ScriptB]MaxBound \[ScriptB]E && VerboseOutput != False
      ,Print["  Above \[ScriptB]MaxBound times \[ScriptB]E after ",Counter," backwards Euler iterations."];
       Print["Last Point:",PointsList[[-1]]]];
  Counter++];
  Return[PointsList]
];

EulerPointsStartingFromSSPlus[\[FilledUpTriangle]_] := Block[{},
  \[ScriptB]Start = \[ScriptB]E + \[FilledUpTriangle];
  \[Kappa]Start   = \[Kappa]E   + \[Kappa]EP \[FilledUpTriangle] + \[Kappa]EPP (\[FilledUpTriangle]^2)/2 + \[Kappa]EPPP (\[FilledUpTriangle]^3)/6;
  \[Kappa]PStart  =        \[Kappa]EP   + \[Kappa]EPP \[FilledUpTriangle]       + \[Kappa]EPPP (\[FilledUpTriangle]^2)/2;
  \[ScriptC]Start = cEfrombTaylorNearTarget[\[FilledUpTriangle]];
  \[ScriptV]Start = \[ScriptV]E + NIntegrate[u'[cEfrombTaylorNearTarget[\[Bullet]]],{\[Bullet],0,\[FilledUpTriangle]}];
  StartPoint = {\[ScriptB]Start,\[ScriptC]Start,\[Kappa]Start,\[ScriptV]Start,\[Kappa]PStart};
  BackShoot[{StartPoint}]
];

FindStableArm := Block[{},
  \[Kappa]EMax = \[Kappa]ELim0Find;
(* Digest the \[ScriptC]E derivatives into numbers; must do this before evaluating EulerPointsStartingFromSSPlus *)
  \[Kappa]E   = Select[\[ScriptC]EPAnalytical, 0 <= # <= 1 &][[1]]   ;
  If[Not[MachineNumberQ[\[Kappa]E]],Print["\[Kappa]E = ",\[Kappa]E," is not a number."];Abort[]];
  \[Kappa]EP  = \[ScriptC]EPPAnalytical /. \[ScriptC]EP -> \[Kappa]E ;
  \[Kappa]EPP = (\[ScriptC]EPPPAnalytical /. \[ScriptC]EPP -> \[Kappa]EP) /. \[ScriptC]EP -> \[Kappa]E;
  \[Kappa]EPPP= ((\[ScriptC]EPPPPAnalytical /. \[ScriptC]EPPP -> \[Kappa]EPP)/. \[ScriptC]EPP -> \[Kappa]EP) /. \[ScriptC]EP -> \[Kappa]E ;

  StableArmBelowSS = Sort[EulerPointsStartingFromSSPlus[-\[CurlyEpsilon] ]];
(* %% The following code is only in the private part of the codebase; it densifies the grid of points obtained by Euler iteration *)
(* %% *)  D\[ScriptB]First=StableArmBelowSS[[-1,1]]-StableArmBelowSS[[-2,1]];    (* Size of gap between 1st Euler point and 2nd *)
(* %% *)  IterLength=Length[StableArmBelowSS];
(* %% *)  PrecisionAugmentationFactor = Floor[1+0.1/(\[Mho]+0.1)]; (* Smaller \[Mho] yields kinkier consumption function; increase precision accordingly *)
(* %% *)  If[VerboseOutput != False  && PrecisionAugmentationFactor>1, Print["Solving again from different starting points to augment precision."]];
(* %% *)  Do[StableArmBelowSS = Join[
(* %% *)    StableArmBelowSS
(* %% *)    ,Take[EPList=EulerPointsStartingFromSSPlus[-\[CurlyEpsilon]  -D\[ScriptB]First*(i/PrecisionAugmentationFactor)],Min[Length[EPList],IterLength]]]
(* %% *)                   ,{i,PrecisionAugmentationFactor-1}]; 
(* %% *)  StableArmBelowSS = Sort[StableArmBelowSS];
  StableArmAboveSS = EulerPointsStartingFromSSPlus[\[CurlyEpsilon]];  
  {\[ScriptB]Bot,\[ScriptC]Bot,\[Kappa]Bot,\[ScriptV]Bot,\[Kappa]PBot} = Take[StableArmBelowSS[[ 1]],5];
  {\[ScriptB]Top,\[ScriptC]Top,\[Kappa]Top,\[ScriptV]Top,\[Kappa]PTop} = Take[StableArmAboveSS[[-1]],5];
  ETarget = {\[ScriptB]E,\[ScriptC]E,\[Kappa]E,\[ScriptV]E,\[Kappa]EP};
  StableArmPoints = Sort[Union[Join[StableArmBelowSS,{ETarget},StableArmAboveSS]]];
  {\[ScriptB]Vec,\[ScriptC]Vec,\[Kappa]Vec,\[ScriptV]Vec,\[Kappa]PVec} = Take[Transpose[StableArmPoints],5];
uPVec   = u'[\[ScriptC]Vec];
uPPVec  = u''[\[ScriptC]Vec];
uPPPVec = u'''[\[ScriptC]Vec];
\[ScriptV]PVec   = uPVec;
\[ScriptV]PPVec  = uPPVec \[Kappa]Vec;

(* This is the value such that, when integrated, the interpolating function matches the last descending Euler point *)
\[Kappa]EPAtZero = 2 (\[ScriptC]Vec[[1]]-((1-\[Tau]ForStake)\[ScriptB]Vec[[1]]+1-\[Tau]ForUI) \[Kappa]EMax)/(((1-\[Tau]ForStake)\[ScriptB]Vec[[1]]+1-\[Tau]ForUI)^2); 
cEfrombInterp = Interpolation[Join[{{{-1+\[Tau]ForUI},0.,\[Kappa]EMax,\[Kappa]EPAtZero}},Transpose[{Partition[\[ScriptB]Vec,1],\[ScriptC]Vec,\[Kappa]Vec,\[Kappa]PVec}]]];

vEfrombInterp = Interpolation[Transpose[{Partition[\[ScriptV]Vec,1],u'[\[ScriptC]Vec],u''[\[ScriptC]Vec] \[Kappa]Vec}]];

(* Caclulate the derivatives of the precautionary saving function necessary for constructing the extrapolation *)

s0=psavEfrombInterp[\[ScriptB]Top];
s1=psavEfrombInterp'[\[ScriptB]Top];
s2=psavEfrombInterp''[\[ScriptB]Top];
s3=psavEfrombInterp'''[\[ScriptB]Top];

(* Now use the automatically derived analytical formulas to produce numerical values for the parameters for the extrapolation *)

\[Phi]0 = \[Phi]Analytical0;
\[Phi]1 = \[Phi]Analytical1;
\[Gamma]0 = \[Gamma]Analytical0;
\[Gamma]1 = \[Gamma]Analytical1;

(* Define the precautionary saving function extrapolation -- defined here because only now do the coefficients have numerical values *)
Clear[psavEfrombExtrap];
psavEfrombExtrap[\[ScriptB]_] := Chop[Exp[\[Phi]0 - \[Phi]1 (\[ScriptB]-\[ScriptB]Top)] + Exp[\[Gamma]0 - \[Gamma]1 (\[ScriptB]-\[ScriptB]Top)] //N]; (* Chop removes any tiny imaginary part *)

]; (* End FindStableArm *)

SimAddAnotherPoint[\[ScriptB]\[ScriptC]Path_]:= Block[{},
  \[ScriptB]NextVal = \[ScriptB]Etp1Fromt[\[ScriptB]\[ScriptC]Path[[-1,1]],\[ScriptB]\[ScriptC]Path[[-1,2]]];
  \[ScriptC]NextVal = cEfromb[\[ScriptB]NextVal];
  {\[ScriptB]NextVal,\[ScriptC]NextVal}];

SimGeneratePathFrom\[ScriptB]Start[\[ScriptB]Initial_,PeriodsToGo_] := Block[{},
  \[ScriptC]Initial = cEfromb[\[ScriptB]Initial];
  \[ScriptB]\[ScriptC]Path = {{\[ScriptB]Initial,\[ScriptC]Initial}};
  Do[AppendTo[\[ScriptB]\[ScriptC]Path,SimAddAnotherPoint[\[ScriptB]\[ScriptC]Path]],{PeriodsToGo}];
];

SimGeneratePath[\[ScriptB]Initial_,PeriodsToGo_] := Block[{},
  \[ScriptC]Initial = cEfromb[\[ScriptB]Initial];
  \[ScriptB]\[ScriptC]Path = {{\[ScriptB]EBase,\[ScriptC]EBase},{\[ScriptB]Initial,\[ScriptC]Initial}};
  Do[AppendTo[\[ScriptB]\[ScriptC]Path,SimAddAnotherPoint[\[ScriptB]\[ScriptC]Path]],{PeriodsToGo}];
];

(* Miscellaneous other useful functions *)

\[ScriptCapitalC]tp1O\[ScriptCapitalC]t[\[ScriptB]_] := (((R) \[Beta])^(1/\[Rho]) (1+\[Mho] ((cEfrombInterp[((1-\[Tau]ForStake)\[ScriptB]-cEfrombInterp[\[ScriptB]]+1-\[Tau]ForUI)\[ScriptCapitalR]+\[Stigma]]/(\[Kappa] (\[ScriptB]-cEfrombInterp[\[ScriptB]])\[ScriptCapitalR]))^\[Rho]-1))^(1/\[Rho]));

ShowParams:=MatrixForm[
  Map[
    {#,ToExpression[#]}&
      ,{"\[Mho]","\[Rho]","\[Kappa]E","\[Kappa]","\[CapitalGamma]","\[ScriptCapitalR]","\[ScriptCapitalP]Growth","\[ScriptCapitalP]Return","\[WeierstrassP]\[Gamma]","\[WeierstrassP]rtntn","\[CapitalPi]Alt","\[GothicH]","\[Zeta]","\[Beta] ","R","\[GothicCapitalG]","r","\[CurlyTheta]","\[GothicG]","\[ScriptB]E","\[ScriptC]E","\[Kappa]E","\[CurlyEpsilon]"}]];

SteadyStateVals= { \[ScriptB]E, \[ScriptC]E, \[ScriptA]E, \[Kappa]E, \[Kappa]EP, \[ScriptV]E};

