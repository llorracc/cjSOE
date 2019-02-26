(* ::Package:: *)

(*This file first provides an example of the dynamics of consumption and wealth in the phase diagram upon changes 
 of severance payment from 0.75 to 1.5. The discount factor is set to be 1/1.04. Then it does a similar exercise for 
an economy in which there are two groups of people with different degrees of impatience, one with discount factor 1/1.02
 and the other 1/1.06 *)

\[Stigma] = 0.75;
FindStableArm;
{\[ScriptC]EBase,\[ScriptB]EBase} = {\[ScriptC]E,\[ScriptB]E};
\[ScriptB]Max=0.5 \[ScriptB]E;
\[ScriptB]MaxMax=1.5 \[ScriptB]E;

cFuncPlotBasePoints=Map[{{#,cEfromb[#]}}&,Table[\[ScriptB],{\[ScriptB],\[ScriptB]Max,\[ScriptB]MaxMax,0.01}]];
cFuncPlotBase=cFuncPlot=Plot[cEfromb[\[ScriptB]],{\[ScriptB],\[ScriptB]Max,\[ScriptB]MaxMax}];
Stable\[ScriptB]LocusPlot=Plot[{\[ScriptB]EDelEqZero[\[ScriptB]]},{\[ScriptB],\[ScriptB]Max,\[ScriptB]MaxMax},PlotStyle->{Thickness[0.003],Blue,Opacity[0.5],Dashed}];
Stable\[ScriptC]LocusPlot=Plot[{\[ScriptC]EDelEqZero[\[ScriptB]]},{\[ScriptB],\[ScriptB]Max,\[ScriptB]MaxMax},PlotStyle->{Thickness[0.003],Blue,Opacity[0.5],Dashed}];

\[Stigma] = 1.5;
FindStableArm;
cFuncPlotNew=Plot[cEfromb[\[ScriptB]],{\[ScriptB],\[ScriptB]Max,\[ScriptB]MaxMax}];
SimGeneratePath[\[ScriptB]EBase,5];
HowMany=5;
\[ScriptB]\[ScriptC]PathPlot = ListPlot[Take[\[ScriptB]\[ScriptC]Path,-(HowMany+1)],PlotStyle->{PointSize[Medium],Red}];
Stable\[ScriptB]LocusPlotNew=Plot[{\[ScriptB]EDelEqZero[\[ScriptB]]},{\[ScriptB],\[ScriptB]Max,\[ScriptB]MaxMax},PlotStyle->{Thickness[0.003],Green,Opacity[0.5],Dashed}];
Stable\[ScriptC]LocusPlotNew=Plot[{\[ScriptC]EDelEqZero[\[ScriptB]]},{\[ScriptB],\[ScriptB]Max,\[ScriptB]MaxMax},PlotStyle->{Thickness[0.003],Green,Opacity[0.5],Dashed}];
SoiChange = Show[cFuncPlot,cFuncPlotNew,\[ScriptB]\[ScriptC]PathPlot ,Stable\[ScriptB]LocusPlot,Stable\[ScriptC]LocusPlot,Stable\[ScriptB]LocusPlotNew,Stable\[ScriptC]LocusPlotNew,
Graphics[{Black,PointSize[Medium],Point[{\[ScriptB]EBase,\[ScriptC]EBase}]}],
Graphics[{Black,PointSize[Medium],Point[{\[ScriptB]E,\[ScriptC]E}]}],
Graphics[{Arrowheads[0.02],Arrow[{{\[ScriptB]EBase,\[ScriptC]EBase},\[ScriptB]\[ScriptC]Path[[2]]}]}],
Graphics[{Arrowheads[0.02],Arrow[{\[ScriptB]\[ScriptC]Path[[2]],\[ScriptB]\[ScriptC]Path[[7]]}]}],
Graphics[Text[" \[UpperLeftArrow] Original Target",{\[ScriptB]EBase,\[ScriptC]EBase},{-1,1}]],
Graphics[Text[" \[UpperLeftArrow] New Target",{\[ScriptB]E,\[ScriptC]E},{-1,1}]],
Graphics[Text["\[LongLeftArrow] \[ScriptC](\[ScriptB])",{ 0.8`\[ScriptB]E,0.85` \[ScriptC]E},{-1,0}]],
Graphics[Text["\[ScriptC](\[ScriptB]) after social insurance rise \[LongRightArrow]",{ 0.7`\[ScriptB]E,1.1` \[ScriptC]E},{-1,0}]]
];
Export[figuredirectory<>"SoiChange.PDF",SoiChange];


(* Calculate the consumption and wealth path for the more patient consumers upon a change of social insurance tax*)
\[CurlyTheta] = 0.02; \[Stigma] = 0.75;{\[ScriptC]EBase,\[ScriptB]EBase} = {\[ScriptC]E,\[ScriptB]E}; \[Stigma] = 1.5;
SimGeneratePath[\[ScriptB]EBase,100];
HowMany=75;
\[ScriptB]PathMP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],HowMany];
\[ScriptC]PathMP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],HowMany];
For[i=1,i<=5,i++,
PrependTo[\[ScriptB]PathMP,\[ScriptB]EBase];
PrependTo[\[ScriptC]PathMP,\[ScriptC]EBase]];
timePath=Table[i,{i,Length[\[ScriptB]PathMP]}
];
(* Calculate the consumption and wealth path for the less patient consumers upon a change of social insurance tax*)
\[CurlyTheta] = 0.06;  \[Stigma] = 0.75;{\[ScriptC]EBase,\[ScriptB]EBase} = {\[ScriptC]E,\[ScriptB]E};  \[Stigma] = 1.5;
SimGeneratePath[\[ScriptB]EBase,100];
HowMany=75;
\[ScriptB]PathLP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],HowMany];
\[ScriptC]PathLP=Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],HowMany];
For[i=1,i<=5,i++,
PrependTo[\[ScriptB]PathLP,\[ScriptB]EBase];
PrependTo[\[ScriptC]PathLP,\[ScriptC]EBase]
];
(* Aggregate the consumption and wealth paths of the two groups of consumers*)
\[ScriptB]Path = 0.5*\[ScriptB]PathLP+0.5*\[ScriptB]PathMP;
\[ScriptC]Path = 0.5*\[ScriptC]PathLP+0.5*\[ScriptC]PathMP;

\[ScriptC]PathPlot=ListPlot[Transpose[{timePath,\[ScriptC]Path}],PlotRange->All,AxesLabel->{"Time","\[ScriptC]"},Ticks->{{{6,"0"}},None}];
\[ScriptB]PathPlot=ListPlot[Transpose[{timePath,\[ScriptB]Path}],PlotRange->All,AxesLabel->{"Time","\[ScriptB]"},
Ticks->{{{6,"0"}},None}];

PathAfterSoiShock=Show[
GraphicsGrid[{{\[ScriptC]PathPlot},{\[ScriptB]PathPlot}}]
];
Export[figuredirectory<>"PathAfterSoiShock.PDF",PathAfterSoiShock];


resetParams;
calculateValues;
