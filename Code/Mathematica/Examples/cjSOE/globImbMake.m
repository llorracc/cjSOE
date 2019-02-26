(* ::Package:: *)

(* This script constructs the figure showing the equilibrium interest rate, capital to output ratio, and net foreign assets to GDP ratios in a two country world when the level of social insurance in the foreign country catches up with the home country level (figure 8). *)
globImbMake:=Block[{temp\[Stigma]F,\[Stigma]Fvec,KoverYvec,Rfreevec,NoverYHvec,NoverYFvec,Wagevec,i,\[ScriptCapitalB]overYnsi,\[ScriptCapitalB]overYwsiH,\[ScriptCapitalB]overYwsiF,NoverYsubplot,factorsubplot,IS,AR,Rlist},
(* Create a workaround for a slight glitch *)
temp\[Stigma]F = \[Stigma]F-0.03*(\[Stigma]H-\[Stigma]F);

(* Define grid vectors: *)
\[Stigma]Fvec = Table[i,{i,temp\[Stigma]F,\[Stigma]H, ((\[Stigma]H-temp\[Stigma]F)/103)}]; (*grid vector for values of social insurance in the foreign country*)
KoverYvec = {}; (* grid vector for the ratio of capital to GDP *)
Rfreevec = {}; (* grid vector for the equilibrium interest rate *)
NoverYHvec = {}; (* grid vector for the ratio of net foreign assets to GDP in the home country *)
NoverYFvec = {}; (* grid vector for the ratio of net foreign assets to GDP in the foreign country *)
Wagevec = {}; (* grid vector for the wage rate *)

(* Compute the values in each grid vector for each level of social insurance in the foreign country: *)
For[i=1,i<=104,i++,
\[Stigma]F = \[Stigma]Fvec[[i]]; (* Set the level of foreign social insurance to the ith value in its list *)
 (* Find the general equilibrium interest rate,i.e. the rate at which the desired net foreign assets in one country exactly offset the desired net foreign liabilities in the other *)
Rlist = x/.NSolve[NoverYge[x]==0,x];
Rlist = Select[Rlist,Element[#,Reals]&];
(*Rlist = Table[Re[Rlist[[j]]],{j,1,Length[Rlist]}];*)
AppendTo[Rfreevec,Max[Rlist]];
(r) = Last[Rfreevec]-1; (* Recalibrate the interest rate to the newly found general equilibrium interest rate *)
calculateValues; (* Recalibrate other variables for the model *)
AppendTo[KoverYvec,KoverY];
(* See eq:BuOyLev... this is (SLevU+SLevEfromb)/GDPLev when there is no social insurance *)
\[ScriptCapitalB]overYnsi = find\[ScriptB]TargTarg[]*(1+\[Mho]*EmpGro*\[GothicCapitalG]/(EmpGro*\[GothicCapitalG]-PLives*(\[Beta]*(R))^(1/\[Rho])));
\[ScriptCapitalB]overYwsiH=\[ScriptCapitalB]overYnsi*(1-\[Stigma]H*(\[Mho]*XperGro/EmpGro+\[Kappa]*(1+(\[CapitalThorn]\[CapitalGamma]^(-\[Rho])-1)/\[Mho])^(1/\[Rho])));(* S/P with home country social insurance, a direct application of eq:\[ScriptB]TargWithsocIns. *)
\[ScriptCapitalB]overYwsiF=\[ScriptCapitalB]overYnsi*(1-\[Stigma]F*(\[Mho]*XperGro/EmpGro+\[Kappa]*(1+(\[CapitalThorn]\[CapitalGamma]^(-\[Rho])-1)/\[Mho])^(1/\[Rho])));(* S/P with foreign country social insurance, a direct application of eq:\[ScriptB]TargWithsocIns. *)
AppendTo[NoverYHvec,EmpGro*\[GothicCapitalG]*(\[ScriptCapitalB]overYwsiH/(R)-KoverY)];(* This is a direct application of eq:NFALev. *)
AppendTo[NoverYFvec,EmpGro*\[GothicCapitalG]*(\[ScriptCapitalB]overYwsiF/(R)-KoverY)]; (* This is a direct application of eq:NFALev. *)
AppendTo[Wagevec,(1-kapshare)*(kapshare/((R)-DeprFac))^(kapshare/(1-kapshare))];  (* this is from eq:Cobb-Douglas but omits \ptyLev_t*)
];

(* Reset the parameters and other variables to their original values: *)
resetParams;
calculateValues;

(* Chop off the leftmost tails, where the glitch occurs *)
\[Stigma]Fvec = Rest[Rest[Rest[\[Stigma]Fvec]]];
NoverYFvec =  Rest[Rest[Rest[NoverYFvec]]];
NoverYHvec =  Rest[Rest[Rest[NoverYHvec]]];
Rfreevec = Rest[Rest[Rest[Rfreevec]]];
Wagevec = Rest[Rest[Rest[Wagevec]]];

IS=200;
AR = 2;
(* Create the net foreign assets subplot: *)
NoverYsubplot = Show[
ListPlot[Transpose[{\[Stigma]Fvec,NoverYHvec}],Joined->True,PlotStyle->{Thickness[Medium],Black}],
ListPlot[Transpose[{\[Stigma]Fvec,NoverYFvec}],Joined->True,PlotStyle->{Thickness[Medium],Black}],
Graphics[Text["\[LongLeftArrow]Home",{1.0,-.4},{-1,0}]],
Graphics[Text["\[LongLeftArrow]Foreign",{1.0,0.10},{-1,0}]],
Frame->True,
FrameLabel->{"Foreign social insurance","NFA to GDP Ratio N/Y"},
FrameTicks->{{0.8,{1.0,"1.0"},1.2,1.4},{-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,{0,"0.0"},0.1,0.2},None,None},
AxesOrigin->{0.7,-0.6},
PlotRange->{{\[Stigma]F,\[Stigma]H},{-0.6,0.2}},
PlotRangeClipping->True,
ImageSize->IS,
AspectRatio->AR
,AlignmentPoint->{0.8,-0.6}
,BaselinePosition->Bottom
];

(* Create the factor payments subplot: *)
factorsubplot = Show[
ListPlot[Transpose[{\[Stigma]Fvec,Rfreevec}],Joined->True,PlotStyle->{Thickness[Medium],Black}],
ListPlot[Transpose[{\[Stigma]Fvec,Wagevec}],Joined->True,PlotStyle->{Thickness[Medium],Black}],
Graphics[Text["\[LongLeftArrow]Wage W",{1.0,1.10},{-1,0}]],
Graphics[Text["Return R\[LongRightArrow]",{1.2,1.05},{1,0}]],
Frame->True,
FrameLabel->{"Foreign social insurance","World interest factor and wage"},
FrameTicks->{{0.8,{1,"1.0"},1.2,1.4},{1.04,1.05,1.06,1.07,1.08,1.09,{1.10,"1.10"},1.11,1.12},None,None},
AxesOrigin->{0.7,1.04},
PlotRange->{{\[Stigma]F,\[Stigma]H},{1.04,1.12}},
PlotRangeClipping->True,
ImageSize->IS,
AspectRatio->AR
,AlignmentPoint->{0.8,1.04}
,BaselinePosition->Bottom
];

(* Create the global imbalances figure by putting the subplots together: *)
globImb = Show[
GraphicsGrid[{{NoverYsubplot,factorsubplot}}
,BaselinePosition->Bottom]
(* ,PlotLabel->"Figure 8. Global impact of decrease in desired level of foreign wealth" *)
];

(* Save the figure: *)
Export[FigsDir<>"/"<>"globImb.pdf",globImb];
Export[FigsDir<>"/"<>"globImb.png",globImb,ImageSize->FullPageSize];
];
