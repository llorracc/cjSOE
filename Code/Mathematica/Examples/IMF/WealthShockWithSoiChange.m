(* ::Package:: *)

(* This file considers wealth shocks as well as social insurance shocks and computes the consumption and wealth paths of an economy in which there's two groups of people with different degrees of impatience, one with discount factor 1/1.047
 and the other 1/1.087. \[Stigma] payement is originally set to 2. The parameters are chosen to set the orginal equilibrium ratio of wealth to permanent income approximately 3. The interest rate 
is set to be 1.05, but it can be endogenized using equiR.m*)


\[Stigma] = 2;
\[CapitalDelta]\[Stigma] = 0.15;

(*Path for the more patient group*)
\[CurlyTheta] = 0.047;
Shock = 0.15;
{\[ScriptC]EBase,\[ScriptB]EBase} = {\[ScriptC]E,\[ScriptB]E}; test1=\[ScriptB]E;
\[Stigma] = \[Stigma]+\[CapitalDelta]\[Stigma];
FindStableArm;
(*First shock:  a sudden, unexpected increment to wealth of a size equal to 15 percent 
of disposable income and an increase in the availability of social insurance by an amount 
that corresponds to another 15 percent of disposable income*)
SimGeneratePath[\[ScriptB]EBase+Shock,15];
HowMany=10;
\[ScriptB]PathMP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],HowMany];
\[ScriptC]PathMP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],HowMany];
For[i=1,i<=4,i++,
PrependTo[\[ScriptB]PathMP,\[ScriptB]EBase];
PrependTo[\[ScriptC]PathMP,\[ScriptC]EBase]
];
(*Second shock: a sudden, unexpected decrease in wealth of a size equal to 15 percent 
of disposable income and a reduction in the availability of social insurance by an amount 
that corresponds to another 15 percent of disposable income *)
\[Stigma] = \[Stigma]-\[CapitalDelta]\[Stigma];
FindStableArm;
SimGeneratePath[\[ScriptB]PathMP[[-1]]-Shock,15];
\[ScriptB]PathMP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],{2,HowMany}];
\[ScriptC]PathMP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],{2,HowMany}];
\[ScriptB]PathMP=Join[\[ScriptB]PathMP,\[ScriptB]PathMP2];
\[ScriptC]PathMP=Join[\[ScriptC]PathMP,\[ScriptC]PathMP2];

timePath=Table[i,{i,Length[\[ScriptB]PathMP]}];


(*Path for the less patient group*)
\[CurlyTheta] = 0.087;
Shock = 0.15;
{\[ScriptC]EBase,\[ScriptB]EBase} = {\[ScriptC]E,\[ScriptB]E};test2=\[ScriptB]E;
\[Stigma] = \[Stigma]+\[CapitalDelta]\[Stigma];
FindStableArm;
(*First shock:  a sudden, unexpected increment to wealth of a size equal to 15 percent of disposable income. 
Meanwhile there is an increase in the social insurance level that corresponds to another 15 percent of disposable income*)
SimGeneratePath[\[ScriptB]EBase+Shock,15];
HowMany=10;
\[ScriptB]PathLP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],HowMany];
\[ScriptC]PathLP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],HowMany];
For[i=1,i<=4,i++,
PrependTo[\[ScriptB]PathLP,\[ScriptB]EBase];
PrependTo[\[ScriptC]PathLP,\[ScriptC]EBase]
];

(*Second shock: a sudden, unexpected decrease in wealth of a size equal to 15 percent of disposable income
Meanwhile there is a decrease in the social insurance level that corresponds to another 15 percent of disposable income*)
\[Stigma] = \[Stigma]-\[CapitalDelta]\[Stigma];
FindStableArm;
SimGeneratePath[\[ScriptB]PathLP[[-1]]-Shock,15];
\[ScriptB]PathLP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],{2,HowMany}];
\[ScriptC]PathLP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],{2,HowMany}];
\[ScriptB]PathLP=Join[\[ScriptB]PathLP,\[ScriptB]PathLP2];
\[ScriptC]PathLP=Join[\[ScriptC]PathLP,\[ScriptC]PathLP2];
(* Aggregate the consumption and wealth paths of the two groups of consumers*)
\[ScriptB]Path = 0.5*\[ScriptB]PathLP+0.5*\[ScriptB]PathMP;
\[ScriptC]Path = 0.5*\[ScriptC]PathLP+0.5*\[ScriptC]PathMP;

\[ScriptC]PathPlot=ListPlot[{Transpose[{timePath,\[ScriptC]Path}],Transpose[{timePath,\[ScriptC]PathMP}],Transpose[{timePath,\[ScriptC]PathLP}]},PlotStyle->{Black,Green,Blue},PlotRange->All,AxesLabel->{"Time","\[ScriptC]"},Ticks->{{{5,"0"}},None}];
\[ScriptB]PathPlot=ListPlot[{Transpose[{timePath,\[ScriptB]Path}],Transpose[{timePath,\[ScriptB]PathMP}],Transpose[{timePath,\[ScriptB]PathLP}]},PlotStyle->{Black,Green,Blue},PlotRange->All,AxesLabel->{"Time","\[ScriptB]"},
Ticks->{{{5,"0"}},None}];

PathAfterWealthShockWithSoiChange=ShowLegend[Show[
GraphicsGrid[{{\[ScriptC]PathPlot},{\[ScriptB]PathPlot}}]
],
{{{Graphics[{Black,Point[{{0,0},{1,0}}]}],"The whole economy"},{Graphics[{Blue,Point[{{0,0},{1,0}}]}],"Less patient group"},{Graphics[{Green,Point[{{0,0},{1,0}}]}],"More patient group"}},
LegendPosition->{0.05,0.65},LegendSize->{0.6,0.25}}
];
Export[figuredirectory<>"PathAfterWealthShockWithSoiChange.eps",PathAfterWealthShockWithSoiChange];



resetParams;
calculateValues;
