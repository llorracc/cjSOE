(* ::Package:: *)

(* This script creates the figure showing the desired levels of wealth and capital as a function of the interest rate (figure 7). *)
genEqbmMake:=Block[{\[Stigma]1,\[Stigma]2,\[Stigma]3,Rfreemin,Rfreemax,Rfreevec,\[ScriptCapitalB]vec1,\[ScriptCapitalB]vec2,\[ScriptCapitalB]vec3,KoverYvec,\[ScriptCapitalB]overYnsi,i},
(* Define a grid for the interest rate: *)
Rfreemin = 1.0; (* minimum value for Rfree *)
Rfreemax = 1.08; (* maximum value for Rfree *)
Rfreevec = Table[i,{i,Rfreemin,Rfreemax,((Rfreemax-Rfreemin)/100)}];

(* Define the three levels of social insurance.  See Closed Economy, second paragraph: *)
\[Stigma]1 = 0;
\[Stigma]2 = 1;
\[Stigma]3 = 2;

(* Define grids for the desired level of wealth and capital, initialized to be empty: *)
\[ScriptCapitalB]vec1 = {}; (* vector for \[ScriptCapitalB]/Y when \[Stigma] = 0 *)
\[ScriptCapitalB]vec2 = {}; (* vector for \[ScriptCapitalB]/Y when \[Stigma] = 1 *)
\[ScriptCapitalB]vec3 = {}; (* vector for \[ScriptCapitalB]/Y when \[Stigma] = 2 *)
KoverYvec = {}; (* vector for K/Y *)

(* Calculate values for \[ScriptCapitalB]/Y and K/Y for each point in the interest rate grid: *)
For[i=1,i<=101,i++,
(r) = Rfreevec[[i]]-1; (* set the interest rate to the ith value *)
calculateValues; (* then recalibrate the other variables *)
(* See eq:BuOyLev... this is (SLevU+SLevEfromb)/GDPLev when there is no social insurance *)
\[ScriptCapitalB]overYnsi = find\[ScriptB]TargTarg[]*(1+\[Mho]*EmpGro*\[GothicCapitalG]/(EmpGro*\[GothicCapitalG]-PLives*(\[Beta]*(R))^(1/\[Rho]))); 
(* The next three lines are direct applications of eq:\[ScriptB]TargWithsocIns *)
AppendTo[\[ScriptCapitalB]vec1,\[ScriptCapitalB]overYnsi*(1-\[Stigma]1*(\[Mho]*XperGro/EmpGro+\[Kappa]*(1+(\[CapitalThorn]\[CapitalGamma]^(-\[Rho])-1)/\[Mho])^(1/\[Rho])))]; 
AppendTo[\[ScriptCapitalB]vec2,\[ScriptCapitalB]overYnsi*(1-\[Stigma]2*(\[Mho]*XperGro/EmpGro+\[Kappa]*(1+(\[CapitalThorn]\[CapitalGamma]^(-\[Rho])-1)/\[Mho])^(1/\[Rho])))]; 
AppendTo[\[ScriptCapitalB]vec3,\[ScriptCapitalB]overYnsi*(1-\[Stigma]3*(\[Mho]*XperGro/EmpGro+\[Kappa]*(1+(\[CapitalThorn]\[CapitalGamma]^(-\[Rho])-1)/\[Mho])^(1/\[Rho])))]; 
AppendTo[KoverYvec,KoverY];
];

(* Reset the parameters and other values: *)
resetParams;
calculateValues;

(* Create the desired wealth and capital ratios figure: *)
genEqbm = Show[
(* Add the plots for the capital and wealth ratios: *)
ListPlot[Transpose[{Rfreevec,\[ScriptCapitalB]vec1}],Joined->True,PlotStyle->{Thick,Black}],
ListPlot[Transpose[{Rfreevec,\[ScriptCapitalB]vec2}],Joined->True,PlotStyle->{Thick,Black}],
ListPlot[Transpose[{Rfreevec,\[ScriptCapitalB]vec3}],Joined->True,PlotStyle->{Thick,Black}],
ListPlot[Transpose[{Rfreevec,KoverYvec}],Joined->True,PlotStyle->{Thick,Black}],
(* Add text annotations: *)
Graphics[Text["Wealth ratio, \[Stigma]=0",{Rfreevec[[13]]+0.003,\[ScriptCapitalB]vec1[[13]]-0.2}]],
Graphics[Text["Wealth ratio, \[Stigma]=1",{Rfreevec[[13]]+0.003,\[ScriptCapitalB]vec2[[13]]-0.2}]],
Graphics[Text["Wealth ratio, \[Stigma]=2",{Rfreevec[[13]]+0.003,\[ScriptCapitalB]vec3[[13]]-0.2}]],
Graphics[Text[" \[LongLeftArrow]Capital ratio",{Rfreevec[[15]]+0.011,KoverYvec[[15]]}]],
(* Set up the frame and figure title: *)
Frame->True,
FrameLabel->{"Interest factor, R","Wealth and capital ratios"},
FrameTicks->{{{1.00,"1.00"},1.01,1.02,1.03,1.04,1.05,1.06,1.07,1.08},{1.5,{2.0,"2.0"},2.5,{3,"3.0"},3.5,{4,"4.0"},4.5,{5,"5.0"}},None,None},
(*PlotLabel->"Figure 7. Desired wealth and capital ratios",*)
PlotRange->{{1,1.08},{1.5,5}},
AxesOrigin->{-10,-10},
ImageSize->400
];

(* Save the figure: *)
Export[FigsDir<>"/"<>"genEqbm.pdf",genEqbm];
Export[FigsDir<>"/"<>"genEqbm.png",genEqbm,ImageSize->FullPageSize];
];

