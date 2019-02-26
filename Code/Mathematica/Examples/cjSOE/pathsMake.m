(* ::Package:: *)

(* This script creates the figure showing the paths of wealth and consumption as a share of labor income. *)
pathsMake:=Block[{\[ScriptB]Path, \[ScriptC]Path, maxt, time, \[ScriptB]Plot, \[ScriptC]Plot},
maxt = 100;
FindStableArm;
SimGeneratePath[0,maxt];
time = Table[i,{i,0,maxt}];

\[ScriptB]path = Take[Transpose[\[ScriptB]\[ScriptC]Path][[1]],{2,maxt+2}];
\[ScriptC]path = Take[Transpose[\[ScriptB]\[ScriptC]Path][[2]],{2,maxt+2}];

ConnectTheDots = True;
\[ScriptB]Plot = ListPlot[Transpose[{time,\[ScriptB]path}],Joined->ConnectTheDots,PlotStyle->{Thick,Black},PlotRange->{Automatic,{0.,Automatic}}];
\[ScriptC]Plot = ListPlot[Transpose[{time,\[ScriptC]path}],Joined->ConnectTheDots,PlotStyle->{Thick,Black},PlotRange->{Automatic,{0.,Automatic}}];

paths = Show[
\[ScriptB]Plot,
\[ScriptC]Plot,
(* Add text annotations: *)
Graphics[Text["Consumption",{maxt*0.4,1.22*\[ScriptC]E}]],
Graphics[Text["Wealth",{maxt*0.6,0.96*\[ScriptB]E}]],
(* Label the graph and set up the frame: *)
Frame->True,
FrameLabel->{"Time","Consumption and wealth ratios, \[ScriptC] and \[ScriptB]"},
FrameTicks->{{0,20,40,60,80,100},{1,2,3,4,5},None,None},
AxesOrigin->{0,0},
PlotRange->{{1,100},{0,5}},
ImageSize->400
];
(* Save the figure *)
Export[FigsDir<>"/"<>"paths.pdf",paths];
Export[FigsDir<>"/"<>"paths.png",paths,ImageSize->FullPageSize];
];
