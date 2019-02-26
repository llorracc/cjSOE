(* ::Package:: *)

(* This script creates the figure showing the sensitivity analysis of the foreign assets to GDP ratio (figure 3). *)
sensitivityMake:=Block[{i,CRRAvec,uratevec,WGrovec,Rfreevec, NtoYbyCRRAns,NtoYbyCRRAws,NtoYbyuratens,NtoYbyuratews,NtoYbyWGrons,NtoYbyWGrows,NtoYbyRfreens,NtoYbyRfreews,ConnectTheDots,NSstyle,WSstyle,CRRAsubplot,uratesubplot,WGrosubplot,Rfreesubplot,IS,AR},
(* Compute the variation of net foreign assets to GDP with respect to risk aversion (CRRA): *)
CRRAvec = Table[i,{i,0.1,10,0.2}]; (* Grid of values that CRRA will take on *)
NtoYbyCRRAns = {}; (* N/Y in model with no stakes *)
NtoYbyCRRAws = {}; (* N/Y in model with stakes *)
(* For each value in CRRAvec, recalculate the model's variables,then find the ratio N/Y in the model with and without stakes. *)
For[i=1,i<=Length[CRRAvec],i++,
\[Rho] = CRRAvec[[i]];
calculateValues;
AppendTo[NtoYbyCRRAns,NoverYns[]];
AppendTo[NtoYbyCRRAws,NoverYws[]];
];
resetParams;

(* Compute the variation of net foreign assets to GDP with respect to unemployment probability (urate): *)
uratevec = Table[i,{i,0.02,0.05,0.001}]; (* Grid of values that urate will take on *)
NtoYbyuratens = {}; (* N/Y in model with no stakes *)
NtoYbyuratews = {}; (* N/Y in model with stakes *)
(* For each value in uratevec, recalculate the model's variables,then find the ratio N/Y in the model with and without stakes. *)
For[i=1,i<=Length[uratevec],i++,
\[Mho] = uratevec[[i]];
PDies=1/(60-1/\[Mho]); (* Life expectancy is 60 years *)
calculateValues;
AppendTo[NtoYbyuratens,NoverYns[]];
AppendTo[NtoYbyuratews,NoverYws[]];
];
resetParams;

(* Compute the variation of net foreign assets to GDP with respect to productivity growth (WGro): *)
WGrovec = Table[i,{i,1.03,1.10,0.001}]; (* Grid of values that WGro will take on *)
NtoYbyWGrons = {}; (* N/Y in model with no stakes *)
NtoYbyWGrows = {}; (* N/Y in model with stakes *)
(* For each value in WGrovec, recalculate the model's variables,then find the ratio N/Y in the model with and without stakes. *)
For[i=1,i<=Length[WGrovec],i++,
\[GothicG] = WGrovec[[i]]-1;
calculateValues;
AppendTo[NtoYbyWGrons,NoverYns[]];
AppendTo[NtoYbyWGrows,NoverYws[]];
];
resetParams;

(* Compute the variation of net foreign assets to GDP with respect to interest factor (Rfree): *)
Rfreevec = Table[i,{i,1.001,1.05,0.001}]; (* Grid of values that Rfree will take on *)
NtoYbyRfreens = {}; (* N/Y in model with no stakes *)
NtoYbyRfreews = {}; (* N/Y in model with stakes *)
(* For each value in Rfreevec, recalculate the model's variables,then find the ratio N/Y in the model with and without stakes. *)
For[i=1,i<=Length[Rfreevec],i++,
(r) = Rfreevec[[i]]-1;
calculateValues;
AppendTo[NtoYbyRfreens,NoverYns[]];
AppendTo[NtoYbyRfreews,NoverYws[]];
];
resetParams;
calculateValues;

(* Specify some style parameters: *)
ConnectTheDots = True;
WSstyle = {Thickness[Medium],Black,Dashing[{0.02}]};
NSstyle = {Thickness[Medium],Black};
IS = 200;
AR = 2/3;

(* Create the subplot for risk aversion: *)
CRRAsubplot = Show[
ListPlot[Transpose[{CRRAvec,NtoYbyCRRAns}],Joined->ConnectTheDots,PlotStyle->NSstyle],
ListPlot[Transpose[{CRRAvec,NtoYbyCRRAws}],Joined->ConnectTheDots,PlotStyle->WSstyle],
Frame->True,
FrameLabel->{"Risk aversion, \[Rho]", "N/Y"},
FrameTicks->{None,Table[i,{i,-3,3}],None,None},
AxesOrigin->{0,-4},
PlotRange->{{0,10},{-3,3}},
AspectRatio->AR,
ImageSize->IS
];

(* Create the subplot for unemployment probability: *)
uratesubplot = Show[
ListPlot[Transpose[{uratevec,NtoYbyuratens}],Joined->ConnectTheDots,PlotStyle->NSstyle],
ListPlot[Transpose[{uratevec,NtoYbyuratews}],Joined->ConnectTheDots,PlotStyle->WSstyle],
Frame->True,
FrameLabel->{"Unemployment probability, \[Mho]", "N/Y"},
FrameTicks->{{0.02,0.03,0.04,0.05},{-2,0,2,4},None,None},
AxesOrigin->{0,-4},
PlotRange->{{0.02,0.05},{-2,5}},
AspectRatio->AR,
ImageSize->IS
];

(* Create the subplot for productivity growth: *)
WGrosubplot = Show[
ListPlot[Transpose[{WGrovec,NtoYbyWGrons}],Joined->ConnectTheDots,PlotStyle->NSstyle],
ListPlot[Transpose[{WGrovec,NtoYbyWGrows}],Joined->ConnectTheDots,PlotStyle->WSstyle],
Graphics[Text["With stakes",{WGrovec[[30]]+0.02,NtoYbyWGrows[[30]]+0.1}]],
Graphics[Text["No stakes",{WGrovec[[30]]-0.01,NtoYbyWGrons[[30]]-0.2}]],
Frame->True,
FrameLabel->{"Productivity growth, G", "N/Y"},
FrameTicks->{{1.04,1.06,1.08,{1.1,"1.10"}},{-1.5,{-1,"1.0"},-0.5,{0,"0.0"},0.5,{1,"1.0"},1.5},None,None},
AxesOrigin->{-2,-4},
PlotRange->{{1.03,1.1},{-1.5,1.5}},
AspectRatio->AR,
ImageSize->IS
];

(* Create the subplot for interest factor: *)
Rfreesubplot = Show[
ListPlot[Transpose[{Rfreevec,NtoYbyRfreens}],Joined->ConnectTheDots,PlotStyle->NSstyle],
ListPlot[Transpose[{Rfreevec,NtoYbyRfreews}],Joined->ConnectTheDots,PlotStyle->WSstyle],
Frame->True,
FrameLabel->{"Interest factor, R", "N/Y"},
FrameTicks->{{{1,"1.00"},1.01,1.02,1.03,1.04,1.05},{-3,-2,-1,0,1,2},None,None},
AxesOrigin->{-5,-5},
PlotRange->{{1,1.05},{-3,2}},
AspectRatio->AR,
ImageSize->IS
];

sensitivity = Show[
GraphicsGrid[{{CRRAsubplot,uratesubplot},{WGrosubplot,Rfreesubplot}}]
(*PlotLabel->"Figure 3. Sensitivity analysis"*)
];
(* Save the figure: *)
Export[FigsDir<>"/"<>"sensitivity.pdf",sensitivity];
Export[FigsDir<>"/"<>"sensitivity.png",sensitivity,ImageSize->FullPageSize];
];
