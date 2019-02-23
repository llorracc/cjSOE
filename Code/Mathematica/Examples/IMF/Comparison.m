(* ::Package:: *)

(*Growth rate change*)
(*Path for the more patient group*)
\[Stigma] = 2;
\[CurlyTheta] = 0.047;
{\[ScriptC]EBase,\[ScriptB]EBase} = {\[ScriptC]E,\[ScriptB]E}; 
\[GothicG] = 0.05;
FindStableArm;
(*First shock:  a sudden, unexpected increment to growth rate of 1%*)
SimGeneratePath[\[ScriptB]EBase,15];
HowMany=10;
\[ScriptB]PathMP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],HowMany];
\[ScriptC]PathMP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],HowMany];
For[i=1,i<=4,i++,
PrependTo[\[ScriptB]PathMP,\[ScriptB]EBase];
PrependTo[\[ScriptC]PathMP,\[ScriptC]EBase]
];

(*Second shock: a sudden, unexpected decrease in growth rate of 1% *)
\[GothicG] = 0.04;
FindStableArm;
SimGeneratePath[\[ScriptB]PathMP[[-1]],15];
\[ScriptB]PathMP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],{2,HowMany}];
\[ScriptC]PathMP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],{2,HowMany}];
\[ScriptB]PathMP=Join[\[ScriptB]PathMP,\[ScriptB]PathMP2];
\[ScriptC]PathMP=Join[\[ScriptC]PathMP,\[ScriptC]PathMP2];

timePath=Table[i,{i,Length[\[ScriptB]PathMP]}];

(*Path for the less patient group*)
\[CurlyTheta] = 0.087;
{\[ScriptC]EBase,\[ScriptB]EBase} = {\[ScriptC]E,\[ScriptB]E};
\[GothicG] = 0.05;
FindStableArm;
(*First shock:  a sudden, unexpected increment to in growth rate of 1%*)
SimGeneratePath[\[ScriptB]EBase,15];
HowMany=10;
\[ScriptB]PathLP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],HowMany];
\[ScriptC]PathLP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],HowMany];
For[i=1,i<=4,i++,
PrependTo[\[ScriptB]PathLP,\[ScriptB]EBase];
PrependTo[\[ScriptC]PathLP,\[ScriptC]EBase]
];

(*Second shock: a sudden, unexpected decrease in growth rate of 1%*)
\[GothicG] = 0.04;
FindStableArm;
SimGeneratePath[\[ScriptB]PathLP[[-1]],15];
\[ScriptB]PathLP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],{2,HowMany}];
\[ScriptC]PathLP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],{2,HowMany}];
\[ScriptB]PathLP=Join[\[ScriptB]PathLP,\[ScriptB]PathLP2];
\[ScriptC]PathLP=Join[\[ScriptC]PathLP,\[ScriptC]PathLP2];
(* Aggregate the consumption and wealth paths of the two groups of consumers*)
\[ScriptB]PathGrowthRate = 0.5*\[ScriptB]PathLP+0.5*\[ScriptB]PathMP;
\[ScriptC]PathGrowthRate = 0.5*\[ScriptC]PathLP+0.5*\[ScriptC]PathMP; 
\[ScriptA]PathGrowthRate = \[ScriptB]PathGrowthRate-\[ScriptC]PathGrowthRate+1-\[Tau]ForUI;
NormalizationFactor = \[ScriptC]PathGrowthRate[[6]]-\[ScriptC]PathGrowthRate[[5]];
For[i=6,i<Length[\[ScriptC]PathGrowthRate]+1,i++,\[ScriptC]PathGrowthRate[[i]]=\[ScriptC]PathGrowthRate[[5]]+(\[ScriptC]PathGrowthRate[[i]]-\[ScriptC]PathGrowthRate[[5]])/NormalizationFactor];



(*Path for the more patient group*)
\[CurlyTheta] = 0.047;
{\[ScriptC]EBase,\[ScriptB]EBase} = {\[ScriptC]E,\[ScriptB]E};
\[Mho] = 0.015; 
FindStableArm;
(*First shock:  a sudden, unexpected increase in the probability of unemployment*)
SimGeneratePath[\[ScriptB]EBase,15];
HowMany=10;
\[ScriptB]PathMP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],HowMany];
\[ScriptC]PathMP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],HowMany];
For[i=1,i<=4,i++,
PrependTo[\[ScriptB]PathMP,\[ScriptB]EBase];
PrependTo[\[ScriptC]PathMP,\[ScriptC]EBase]
];

(*Second shock: a sudden, unexpected decrease in the probability of unemployment*)
\[Mho] = 0.025;
FindStableArm;
SimGeneratePath[\[ScriptB]PathMP[[-1]],15];
\[ScriptB]PathMP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],{2,HowMany}];
\[ScriptC]PathMP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],{2,HowMany}];
\[ScriptB]PathMP=Join[\[ScriptB]PathMP,\[ScriptB]PathMP2];
\[ScriptC]PathMP=Join[\[ScriptC]PathMP,\[ScriptC]PathMP2];

(*Path for the less patient group*)
\[CurlyTheta] = 0.087;
{\[ScriptC]EBase,\[ScriptB]EBase} = {\[ScriptC]E,\[ScriptB]E};
\[Mho] = 0.015; 
FindStableArm;
(*First shock:  a sudden, unexpected increase in the probability of unemployment*)
SimGeneratePath[\[ScriptB]EBase,15];
HowMany=10;
\[ScriptB]PathLP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],HowMany];
\[ScriptC]PathLP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],HowMany];
For[i=1,i<=4,i++,
PrependTo[\[ScriptB]PathLP,\[ScriptB]EBase];
PrependTo[\[ScriptC]PathLP,\[ScriptC]EBase]
];

(*Second shock: a sudden, unexpected decrease in the probability of unemployment*)
\[Mho] = 0.025;
FindStableArm;
SimGeneratePath[\[ScriptB]PathLP[[-1]],15];
\[ScriptB]PathLP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],{2,HowMany}];
\[ScriptC]PathLP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],{2,HowMany}];
\[ScriptB]PathLP=Join[\[ScriptB]PathLP,\[ScriptB]PathLP2];
\[ScriptC]PathLP=Join[\[ScriptC]PathLP,\[ScriptC]PathLP2];
(* Aggregate the consumption and wealth paths of the two groups of consumers*)
\[ScriptB]PathUrate = 0.5*\[ScriptB]PathLP+0.5*\[ScriptB]PathMP; 
\[ScriptC]PathUrate = 0.5*\[ScriptC]PathLP+0.5*\[ScriptC]PathMP; 
\[ScriptA]PathUrate = \[ScriptB]PathUrate-\[ScriptC]PathUrate+1-\[Tau]ForUI;
NormalizationFactor = \[ScriptC]PathUrate[[6]]-\[ScriptC]PathUrate[[5]];
For[i=6,i<Length[\[ScriptC]PathUrate]+1,i++,\[ScriptC]PathUrate[[i]]=\[ScriptC]PathUrate[[5]]+(\[ScriptC]PathUrate[[i]]-\[ScriptC]PathUrate[[5]])/NormalizationFactor];


(*Path for the more patient group*)
\[CurlyTheta] = 0.047;
Shock = 0.15;
\[CapitalDelta]\[Stigma] = 0.15;
{\[ScriptC]EBase,\[ScriptB]EBase} = {\[ScriptC]E,\[ScriptB]E}; \[Tau]ForUIOld = \[Tau]ForUI;
\[Stigma] = \[Stigma]+\[CapitalDelta]\[Stigma]; \[Tau]ForUINew = \[Tau]ForUI;
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
\[ScriptB]PathWealthAndSoiChange = 0.5*\[ScriptB]PathLP+0.5*\[ScriptB]PathMP;
\[ScriptC]PathWealthAndSoiChange = 0.5*\[ScriptC]PathLP+0.5*\[ScriptC]PathMP; 
\[ScriptA]PathWealthAndSoiChange = \[ScriptB]PathWealthAndSoiChange-\[ScriptC]PathWealthAndSoiChange+1-\[Tau]ForUIOld;
For[i=6,i<15,i++,\[ScriptA]PathWealthAndSoiChange[[i]]=\[ScriptB]PathWealthAndSoiChange[[i]]-\[ScriptC]PathWealthAndSoiChange[[i]]+1-\[Tau]ForUINew];

NormalizationFactor = \[ScriptC]PathWealthAndSoiChange[[6]]-\[ScriptC]PathWealthAndSoiChange[[5]]; 
For[i=6,i<Length[\[ScriptC]PathWealthAndSoiChange]+1,i++,\[ScriptC]PathWealthAndSoiChange[[i]]=\[ScriptC]PathWealthAndSoiChange[[5]]+(\[ScriptC]PathWealthAndSoiChange[[i]]-\[ScriptC]PathWealthAndSoiChange[[5]])/NormalizationFactor];


\[Stigma] = 2;
(*Path for the more patient group*)
\[CurlyTheta] = 0.047;
Shock = 0.15;
{\[ScriptC]EBase,\[ScriptB]EBase} = {\[ScriptC]E,\[ScriptB]E}; 
FindStableArm;
(*First shock:  a sudden, unexpected increment to wealth of a size equal to 15 percent of disposable income*)
SimGeneratePath[\[ScriptB]EBase+Shock,15];
HowMany=10;
\[ScriptB]PathMP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],HowMany];
\[ScriptC]PathMP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],HowMany];
For[i=1,i<=4,i++,
PrependTo[\[ScriptB]PathMP,\[ScriptB]EBase];
PrependTo[\[ScriptC]PathMP,\[ScriptC]EBase]
];

(*Second shock: a sudden, unexpected decrease in wealth of a size equal to 15 percent of disposable income*)
FindStableArm;
SimGeneratePath[\[ScriptB]PathMP[[-1]]-Shock,15];
\[ScriptB]PathMP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],{2,HowMany}];
\[ScriptC]PathMP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],{2,HowMany}];
\[ScriptB]PathMP=Join[\[ScriptB]PathMP,\[ScriptB]PathMP2];
\[ScriptC]PathMP=Join[\[ScriptC]PathMP,\[ScriptC]PathMP2];

(*Path for the less patient group*)
\[CurlyTheta] = 0.087;
Shock = 0.15;
{\[ScriptC]EBase,\[ScriptB]EBase} = {\[ScriptC]E,\[ScriptB]E};
FindStableArm;
(*First shock:  a sudden, unexpected increment to wealth of a size equal to 15 percent of disposable income*)
SimGeneratePath[\[ScriptB]EBase+Shock,15];
HowMany=10;
\[ScriptB]PathLP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],HowMany];
\[ScriptC]PathLP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],HowMany];
For[i=1,i<=4,i++,
PrependTo[\[ScriptB]PathLP,\[ScriptB]EBase];
PrependTo[\[ScriptC]PathLP,\[ScriptC]EBase]
];

(*Second shock: a sudden, unexpected decrease in wealth of a size equal to 15 percent of disposable income*)
FindStableArm;
SimGeneratePath[\[ScriptB]PathLP[[-1]]-Shock,15];
\[ScriptB]PathLP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],{2,HowMany}];
\[ScriptC]PathLP2=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],{2,HowMany}];
\[ScriptB]PathLP=Join[\[ScriptB]PathLP,\[ScriptB]PathLP2];
\[ScriptC]PathLP=Join[\[ScriptC]PathLP,\[ScriptC]PathLP2];
(* Aggregate the consumption and wealth paths of the two groups of consumers*)
\[ScriptB]PathWealthShock = 0.5*\[ScriptB]PathLP+0.5*\[ScriptB]PathMP;
\[ScriptC]PathWealthShock = 0.5*\[ScriptC]PathLP+0.5*\[ScriptC]PathMP; 
\[ScriptA]PathWealthShock = \[ScriptB]PathWealthShock-\[ScriptC]PathWealthShock+1-\[Tau]ForUI;

NormalizationFactor = \[ScriptC]PathWealthShock[[6]]-\[ScriptC]PathWealthShock[[5]];
For[i=6,i<Length[\[ScriptC]PathWealthShock]+1,i++,\[ScriptC]PathWealthShock[[i]]=\[ScriptC]PathWealthShock[[5]]+(\[ScriptC]PathWealthShock[[i]]-\[ScriptC]PathWealthShock[[5]])/NormalizationFactor];


\[ScriptC]PathPlot=ListPlot[{Transpose[{timePath,\[ScriptC]PathWealthShock}],Transpose[{timePath,\[ScriptC]PathWealthAndSoiChange}],Transpose[{timePath,\[ScriptC]PathGrowthRate}],Transpose[{timePath,\[ScriptC]PathUrate}]},PlotStyle->{Black,Green,Blue,Purple},PlotRange->All,AxesLabel->{"Time","\[ScriptC]"},Ticks->{{{5,"0"}},None}];
\[ScriptB]PathPlot=ListPlot[{Transpose[{timePath,\[ScriptB]PathWealthShock}],Transpose[{timePath,\[ScriptB]PathWealthAndSoiChange}],Transpose[{timePath,\[ScriptB]PathGrowthRate}],Transpose[{timePath,\[ScriptB]PathUrate}]},PlotStyle->{Black,Green,Blue,Purple},PlotRange->All,AxesLabel->{"Time","\[ScriptB]"},
Ticks->{{{5,"0"}},None}];
\[ScriptA]PathPlot=ListPlot[{Transpose[{timePath,\[ScriptA]PathWealthShock}],Transpose[{timePath,\[ScriptA]PathWealthAndSoiChange}],Transpose[{timePath,\[ScriptA]PathGrowthRate}],Transpose[{timePath,\[ScriptA]PathUrate}]},PlotStyle->{Black,Green,Blue,Purple},PlotRange->All,AxesLabel->{"Time","\[ScriptA]"},
Ticks->{{{5,"0"}},None}];
\[ScriptA]PathLinePlot=ListLinePlot[{Transpose[{timePath,\[ScriptA]PathWealthShock}],Transpose[{timePath,\[ScriptA]PathWealthAndSoiChange}],Transpose[{timePath,\[ScriptA]PathGrowthRate}],Transpose[{timePath,\[ScriptA]PathUrate}]},PlotStyle->{Black,Green,Blue,Purple},PlotRange->All,AxesLabel->{"Time","\[ScriptA]"},
Ticks->{{{5,"0"}},None}];


Needs["PlotLegends`"];
Comparison=ShowLegend[Show[
GraphicsGrid[{{\[ScriptC]PathPlot},{\[ScriptB]PathPlot},{\[ScriptA]PathPlot}}]
],
{{{Graphics[{Black,Point[{{0,0},{1,0}}]}],"Wealth"},{Graphics[{Blue,Point[{{0,0},{1,0}}]}],"Growth Rate"},{Graphics[{Green,Point[{{0,0},{1,0}}]}],"Wealth and Credit"},{Graphics[{Purple,Point[{{0,0},{1,0}}]}],"Urate"}},
LegendPosition->{-0.4,0.75},LegendSize->{0.5,0.25}}
];
Export[figuredirectory<>"Comparison.eps",Comparison];

Comparison2=ShowLegend[Show[
\[ScriptB]PathPlot,\[ScriptA]PathLinePlot
],
{{{Graphics[{Black,Point[{{0,0},{1,0}}]}],"Wealth"},{Graphics[{Blue,Point[{{0,0},{1,0}}]}],"Growth Rate"},{Graphics[{Green,Point[{{0,0},{1,0}}]}],"Wealth and Credit"},{Graphics[{Purple,Point[{{0,0},{1,0}}]}],"Urate"}},
LegendPosition->{0.05,0.45},LegendSize->{0.6,0.25}}
];
Export[figuredirectory<>"Comparison2.eps",Comparison2];


resetParams;
calculateValues;
