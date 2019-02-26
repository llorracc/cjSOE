(* ::Package:: *)

(* This script creates the figure showing how the ratio of foreign assets to GDP depends on the extent of social insurance, zeta (figure 4). *)
socInsMake:=Block[{\[ScriptCapitalB]overYnsi,\[Stigma]min,\[Stigma]max,\[Stigma]vec,i,\[ScriptCapitalB]overYvec,NoverYvec,insPlot },
(* Compute the ratio of wealth to GDP in the model with stakes (for employed and unemployed),but no social insurance: See eq:BuOyLev... this is (SLevU+SLevEfromb)/GDPLev *)
\[ScriptCapitalB]overYnsi=find\[ScriptB]TargTarg[]*(1+\[Mho]*EmpGro*\[GothicCapitalG]/(EmpGro*\[GothicCapitalG]-PLives*(\[Beta]*(R))^(1/\[Rho])));

(* Define the grid for social insurance (\[Stigma]): *)
\[Stigma]min = 0; (* minimum level of social insurance *)
\[Stigma]max = 5; (* maximum level of social insurance *)
\[Stigma]vec = Table[i,{i,\[Stigma]min,\[Stigma]max,(\[Stigma]max-\[Stigma]min)/100}]; (* grid for different levels of social insurance *)

(* Compute the ratio of foreign assets to GDP for each level of social insurance: *)
\[ScriptCapitalB]overYvec = {}; (* grid for the ratio of wealth to GDP for each level of social insurance *)
NoverYvec = {}; (* grid for the ratio of net foreign assets to GDP for each level of social insurance *)
For[i=1,i<=101,i++,
(* This is a direct application of eq:\[ScriptB]TargWithsocIns. *)
AppendTo[\[ScriptCapitalB]overYvec,\[ScriptCapitalB]overYnsi*(1-\[Stigma]vec[[i]]*(\[Mho]*XperGro/\[GothicCapitalG]+\[Kappa]*(1+(\[CapitalThorn]\[CapitalGamma]^(-\[Rho])-1)/\[Mho])^(1/\[Rho])))]; 
(* Inside the parentheses is a direct application of eq:NFALev. We must scale by EmpGro*WGro because P grows by this factor each period (\[ScriptCapitalB]overY and KoverY are for period t,but we want them in period t+1). *)
AppendTo[NoverYvec,EmpGro*\[GothicCapitalG]*(\[ScriptCapitalB]overYvec[[i]]/(R)-KoverY)];
];
resetParams;
calculateValues;

(* Create the social insurance figure: *)
insPlot = ListPlot[Transpose[{\[Stigma]vec,NoverYvec}],Joined->True,PlotStyle->{Thick,Black}];
socIns = Show[
insPlot,
Frame->True,
FrameLabel->{"Social insurance benefit in years of wage, \[Stigma]","Foreign assets/GDP, N/Y"},
FrameTicks->{{0,1,2,3,4,5},{-4,-3,-2,-1,0,1},None,None},
AxesOrigin->{-5,-5},
PlotRange->{{0,5},{-4,1}},
ImageSize->400
];
(* Save the figure: *)
Export[FigsDir<>"/"<>"socIns.pdf",socIns];
Export[FigsDir<>"/"<>"socIns.png",socIns,ImageSize->FullPageSize];
];
