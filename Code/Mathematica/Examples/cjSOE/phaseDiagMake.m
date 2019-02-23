(* ::Package:: *)

(* This script constructs the phase diagram for the model with no stakes *)
phaseDiagMake := Block[{\[ScriptB]Min, \[ScriptB]Max,cFuncPlot,Stable\[ScriptB]LocusPlot,Stable\[ScriptC]LocusPlot,arrowlength,arrowheight},
\[ScriptB]Min = 0 \[ScriptB]E;
\[ScriptB]Max = 2 \[ScriptB]E;
FindStableArm;

cFuncPlot = Plot[cEfromb[\[ScriptB]],{\[ScriptB],\[ScriptB]Min,\[ScriptB]Max}];
Stable\[ScriptB]LocusPlot = Plot[{\[ScriptB]EDelEqZero[\[ScriptB]]},{\[ScriptB],\[ScriptB]Min,\[ScriptB]Max},PlotStyle->{Thickness[0.003],Black,Opacity[0.5]}];
Stable\[ScriptC]LocusPlot = Plot[{\[ScriptC]EDelEqZero[\[ScriptB]]},{\[ScriptB],\[ScriptB]Min,\[ScriptB]Max},PlotStyle->{Thickness[0.003],Black,Opacity[0.5]}];

phaseDiag = Show[
cFuncPlot,
Stable\[ScriptB]LocusPlot,
Stable\[ScriptC]LocusPlot,

(* Mark the target level of wealth and consumption *)
ListLinePlot[{{\[ScriptB]E,0},{\[ScriptB]E,\[ScriptC]E}},PlotStyle->{Thickness[Medium],Dotted,Black},PlotRange->All],
ListLinePlot[{{0,\[ScriptC]E},{\[ScriptB]E,\[ScriptC]E}},PlotStyle->{Thickness[Medium],Dotted,Black},PlotRange->All],

(* Add text annotations *)
Graphics[Text["\[CapitalDelta]\[ScriptC]=0",{0.5*\[ScriptB]E,\[ScriptC]EDelEqZero[0.5*\[ScriptB]E]-0.1}]],
Graphics[Text["\[CapitalDelta]\[ScriptB]=0",{1.5*\[ScriptB]E,\[ScriptB]EDelEqZero[1.5*\[ScriptB]E]-0.1}]],

(* Add phase arrows *)
arrowlength = 1;
arrowheight = 0.25*arrowlength;
Graphics[Arrow[{{6,0.5},{6,0.5-arrowheight}}]],
Graphics[Arrow[{{6,0.5},{6+arrowlength,0.5}}]],
Graphics[Arrow[{{3.5,1.3},{3.5,1.3+arrowheight}}]],
Graphics[Arrow[{{3.5,1.3},{3.5-arrowlength,1.3}}]],
Graphics[Arrow[{{0.5,0.65},{0.5,0.65+arrowheight}}]],
Graphics[Arrow[{{0.5,0.65},{0.5+arrowlength,0.65}}]],
Graphics[Arrow[{{9.25,1.2},{9.25,1.2-arrowheight}}]],
Graphics[Arrow[{{9.25,1.2},{9.25-arrowlength,1.2}}]],
(* Label the figure and set up the frame: *)
Frame->True,
FrameLabel->{"Wealth ratio \[ScriptB]","Consumption Ratio \[ScriptC]"},
AxesOrigin->{0,0},
FrameTicks->{{{\[ScriptB]E,"\!\(\*OverscriptBox[\"\[ScriptB]\", \"\[Vee]\"]\)"}},{{\[ScriptC]E,"\!\(\*OverscriptBox[\"\[ScriptC]\", \"\[Vee]\"]\)"}},None,None},
PlotRange->{{0,2*\[ScriptB]E},{0,\[ScriptC]EDelEqZero[2*\[ScriptB]E]}},
ImageSize->400
,AxesLabel->{"\[ScriptB]","\[ScriptC]"}
];
Export[FigsDir<>"/"<>"phaseDiag.pdf",phaseDiag];
Export[FigsDir<>"/"<>"phaseDiag.png",phaseDiag,ImageSize->FullPageSize];
];
