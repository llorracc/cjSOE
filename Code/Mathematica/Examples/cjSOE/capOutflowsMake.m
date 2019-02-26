(* ::Package:: *)

(* This script creates the figure showing how the ratio of capital outflows to GDP varies with the growth rate in the model with stakes (figure 6). *)
capOutflowsMake:=Block[{WGromin,WGromax,WGrovec,uratemin,uratemax,uratevec,PDiesvec,i,dNoverY1,dNoverY2},
(* Define the grids for growth factor and unemployment probability (the latter if we want to vary \[Mho] at the same time as G: *)
(* See Appendix: Steady States, third paragraph *)
WGromin=1.02;(* minimum value of WGro *)
WGromax=1.10;(* maximum value of WGro *)
WGrovec=Table[i,{i,WGromin,WGromax,((WGromax-WGromin)/100)}]; (* grid for values of WGro *)
uratemin=0.02;(* minimum value of urate *)
uratemax=0.04;(* maximum value of urate *)
uratevec=Table[i,{i,uratemin,uratemax,((uratemax-uratemin)/100)}];(* grid for values of urate *)

(* Define the grid for probability of death: *)
PDiesvec = {};
For[i=1,i<=101,i++,
AppendTo[PDiesvec,1/(60-1/uratevec[[i]])]; (* Recalibrate the probability of death so that the consumer has an expected "lifespan" of 60 years *)
];

(* Compute the ratio of capital outflows to GDP with increase in risk: *)
dNoverY1 = {};
(* For each pair of values of WGro and urate, recalibrate the parameters and other variables, then calculate capital outflows: *)
For[i=1,i<=101,i++,
\[GothicCapitalG] = WGrovec[[i]];
\[Mho] = uratevec[[i]];
PDies = PDiesvec[[i]];
calculateValues;
AppendTo[dNoverY1,NoverYws[]*(1-1/(EmpGro*\[GothicCapitalG]))]; (* This is a direct application of eq:DeltaFOyLev *)
];

(* Compute the ratio of capital outflows to GDP without increase in risk: *)
dNoverY2 = {};
(* For each value of WGro, leave urate and PDies unchanged (no increase in risk), recalibrate the parameters and other variables, then calculate capital outflows: *)
For[i=1,i<=101,i++,
\[GothicCapitalG] = WGrovec[[i]];
\[Mho] = uratevec[[1]];
PDies = PDiesvec[[1]];
calculateValues;
AppendTo[dNoverY2,NoverYws[]*(1-1/(EmpGro*\[GothicCapitalG]))]; (* This is a direct application of eq:DeltaFOyLev *)
];

(* Reset the parameters and other values: *)
resetParams;
calculateValues;

(* Create the capital outflows figure: *)
capOutflows = Show[
(* Plot the paths with and without increase in risk: *)
ListPlot[Transpose[{WGrovec,dNoverY1}],Joined->True,PlotStyle->{Thick,Black}],
ListPlot[Transpose[{WGrovec,dNoverY2}],Joined->True,PlotStyle->{Thick,Black,Dashing[0.01]}],
(* Add text annotations: *)
Graphics[Text["Increasing risk",{1.06,0.032}]],
Graphics[Text["Constant risk",{1.073,-0.15}]],
Graphics[Text["A",{1.04,0.045}]],
Graphics[Text["B",{1.0215,0.006}]],
Graphics[Text["C",{1.06,0.055}]],
(* Set up the frame and title: *)
AxesOrigin->{-10,-10},
Frame->True,
FrameLabel->{"Productivity growth factor, G","Capital outflows to GDP, (\!\(\*SubscriptBox[\"N\", \"t\"]\)-\!\(\*SubscriptBox[\"N\", 
RowBox[{\"t\", \"-\", \"1\"}]]\))/\!\(\*SubscriptBox[\"Y\", \"t\"]\)"},
FrameTicks->{{1.02,1.03,1.04,1.05,1.06,1.07,1.08,1.09},{{-0.2,"-0.20"},-0.15,{-0.1,"-0.10"},-0.05,{0,"0.00"},0.05},None,None},
PlotRange->{{1.02,1.09},{-0.2,0.07}},
ImageSize->400
];

(* Save the figure: *)
Export[FigsDir<>"/"<>"capOutflows.pdf",capOutflows];
Export[FigsDir<>"/"<>"capOutflows.png",capOutflows,ImageSize->FullPageSize];
];

