(* ::Package:: *)

(* This script constructs the figure showing the paths for growth, net foreign assets, and capital outflows following an economics take-off (with and without an increase in idiosyncratic risk) in the model with stakes (figure 5). *)
transDynMake:=Block[{WGroAnte,WGroPost,urateAnte,uratePost,PDiesAnte,\[ScriptB]EAnte,\[ScriptB]UAnte,\[ScriptB]Ante,NoverYAnte,labor0,T,\[ScriptB]E1,\[ScriptB]U1,\[ScriptCapitalB]1,labor1,Glabor1,GP1,dNoverY1,NoverY1,\[ScriptB]E2,\[ScriptB]U2,\[ScriptCapitalB]2,labor2,Glabor2,GP2,dNoverY2,NoverY2,time,i,j,PDiesA,riskyStyle,noriskStyle,AR,IS,GPsubplot,NoverYsubplot,dNoverYsubplot},
(* Growth and unemployment risk before (B) and after (A). See Transitions, fifth paragraph: *)
WGroAnte=1.02;
WGroPost=1.06;
urateAnte=0.02;
uratePost=0.03;

(* Recalibrate the parameters of the model for the "before" state: *)
\[GothicCapitalG]=WGroAnte;
\[Mho]=urateAnte;
PDiesAnte=1/(60-1/\[Mho]);(* Recalibrate the probability of death so that the consumer has an expected "lifespan" of 60 years *)
PDies=PDiesAnte; 
calculateValues;

(* Calculate \[ScriptB]TargTarg (employed and unemployed) before: *)
\[ScriptB]EAnte=\[ScriptB]TargTarg;(* employed target wealth *)
\[ScriptB]UAnte=\[ScriptB]EAnte*EmpGro*\[GothicCapitalG]*\[Mho]/(EmpGro*\[GothicCapitalG]-PLives*(\[Beta]*(R))^(1/\[Rho]));(* unemployed target wealth, a direct application of eq:BuOyLev. *)\[ScriptB]Ante=\[ScriptB]EAnte+\[ScriptB]UAnte;(* The sum of employed and unemployed wealth *)
NoverYAnte=NoverYws[];(* ratio of net foreign assets to GDP in the "before" state *)

(* Calculate initial labor supply: (see Appendix: Transition Dynamics) *)
labor0=EmpGro/(EmpGro-erate*XperGro);

(* Define the paths with time horizon of T.  Index 1 is with increase in risk, index 2 is without increase in risk. *)
T = 40;

(* Initialize time path variables for case with increase in risk: *)
\[ScriptB]E1 = Table[0,{T+1}]; (* wealth of the employed *)
\[ScriptB]U1 = Table[0,{T+1}]; (* wealth of the unemployed *)
\[ScriptCapitalB]1 = Table[0,{T+1}]; (* total wealth *)
labor1 = Table[0,{T+1}]; (* labor supply *)
Glabor1 = Table[0,{T+1}]; (* growth in labor supply *)
GP1 = Table[0,{T+1}]; (* growth in output *)
dNoverY1 = Table[0,{T}]; (* capital outflows in terms of output *)
NoverY1 = Table[0,{T}]; (* net foreign assets in terms of output *)

(* Initialize time path variables for case without increase in risk: *)
\[ScriptB]E2 = Table[0,{T+1}]; (* wealth of the employed *)
\[ScriptB]U2 = Table[0,{T+1}]; (* wealth of the unemployed *)
\[ScriptCapitalB]2 = Table[0,{T+1}]; (* total wealth *)
labor2 = Table[0,{T+1}]; (* labor supply *)
Glabor2 = Table[0,{T+1}]; (* growth in labor supply *)
GP2 = Table[0,{T+1}]; (* growth in output *)
dNoverY2 = Table[0,{T}]; (* capital outflows in terms of output *)
NoverY2 = Table[0,{T}]; (* net foreign assets in terms of output *)

(* Define the time index (transition occurs at time 0): *)
time = Table[i,{i,-9,30,1}];

(* Set the variables to the initial steady state for the first 10 periods: *)
For[i=1,i<=10,i++,
GP1[[i]]=EmpGro*\[GothicCapitalG];(* See Appendix: Transition Dynamics, 5th equation *)
GP2[[i]]=EmpGro*\[GothicCapitalG];(* See Appendix: Transition Dynamics, 5th equation *)
\[ScriptB]E1[[i]]=\[ScriptB]EAnte;
\[ScriptB]E2[[i]]=\[ScriptB]EAnte;
\[ScriptB]U1[[i]]=\[ScriptB]UAnte;
\[ScriptB]U2[[i]]=\[ScriptB]UAnte;
\[ScriptCapitalB]1[[i]]=\[ScriptB]E1[[i]]+\[ScriptB]U1[[i]];
\[ScriptCapitalB]2[[i]]=\[ScriptB]E2[[i]]+\[ScriptB]U2[[i]];
];
For[i=1,i<=9,i++,
(* See Appendix: Transition Dynamics, seventh equation *) 
dNoverY1[[i]]=(1-kapshare)/(R)*(GP1[[i]]-1)*\[ScriptB]Ante-(GP1[[i]]-1)*KoverY;
dNoverY2[[i]]=(1-kapshare)/(R)*(GP2[[i]]-1)*\[ScriptB]Ante-(GP2[[i]]-1)*KoverY;
NoverY1[[i]]=NoverYAnte;
NoverY2[[i]]=NoverYAnte;
];

(* Compute the paths with the change in risk: *)
(* Recalibrate the parameters and other variables for the "after" state: *)
\[GothicCapitalG]=WGroPost;
\[Mho]=uratePost;
PDiesPost=1/(60-1/\[Mho]);(* Recalibrate the probability of death so that the consumer has an expected "lifespan" of 60 years *)
PDies=PDiesPost;
calculateValues;
\[Tau]ForStake=Tax;
FindStableArm;

labor1[[10]] = labor0; (* Set initial labor supply at the time of the change *)
(* In this loop, all references are to equations in the Appendix: Transition Dynamics. *)
For[j=11,j<=(T+1),j++,
labor1[[j]]=erate*XperGro*labor1[[j-1]]+EmpGro^(i-10);(* see fourth equation *)
Glabor1[[j]]=labor1[[j]]/labor1[[j-1]];(* see definition of LGro_t *)
GP1[[j]]=Glabor1[[j]]*\[GothicCapitalG];(* see fifth equation *)
\[ScriptB]E1[[j]]=((R)/PGro)*((1-Tax)*\[ScriptB]E1[[j-1]]-cEfromb[\[ScriptB]E1[[j-1]]]+1);(* new wealth level is our previous wealth after taxes minus what we consumed last period *)
\[ScriptB]U1[[j]]=PLives*(\[Beta]*(R))^(1/\[Rho])*\[ScriptB]U1[[j-1]]/GP1[[j]]+\[Mho]*\[ScriptB]E1[[j]];(* see ninth equation *)
\[ScriptCapitalB]1[[j]]=\[ScriptB]E1[[j]]+\[ScriptB]U1[[j]];
dNoverY1[[j-1]]=(1-kapshare)/(R)*(GP1[[j]]*\[ScriptCapitalB]1[[j]]-\[ScriptCapitalB]1[[j-1]])-(GP1[[j]]-1)*KoverY;(* see seventh equation *)
NoverY1[[j-1]]=GP1[[j]]*((1-kapshare)*\[ScriptCapitalB]1[[j]]/(R)-KoverY);(* see sixth equation *)
];
GP1 = Most[GP1];

(* Compute the paths without the change in risk: *)
(* Recalibrate the parameters and other variables for the after state, but with the unemployment probability still in the before state: *)
\[GothicCapitalG]=WGroPost;
\[Mho]=urateAnte;
PDiesPost=1/(60-1/\[Mho]);(* Recalibrate the probability of death so that the consumer has an expected "lifespan" of 60 years *)
PDies=PDiesPost;
calculateValues;
\[Tau]ForStake=0;
FindStableArm;

labor2[[10]] = labor0; (* Set initial labor supply at the time of the change *)
(* In this loop, all references are to equations in the Appendix: Transition Dynamics. *)
For[i=11,i<=(T+1),i++,
labor2[[i]]=erate*XperGro*labor2[[i-1]]+EmpGro^(i-10);(* see fourth equation *)
Glabor2[[i]]=labor2[[i]]/labor2[[i-1]];(* see definition of LGro_t *)
GP2[[i]]=Glabor2[[i]]*\[GothicCapitalG];(* see fifth equation *)
\[ScriptB]E2[[i]]=((R)/PGro)*((1-Tax)*\[ScriptB]E2[[i-1]]-cEfromb[\[ScriptB]E2[[i-1]]]+1);(* new wealth level is our previous wealth after taxes minus what we consumed last period *)
\[ScriptB]U2[[i]]=PLives*(\[Beta]*(R))^(1/\[Rho])*\[ScriptB]U2[[i-1]]/GP2[[i]]+\[Mho]*\[ScriptB]E2[[i]];(* see ninth equation *)
\[ScriptCapitalB]2[[i]]=\[ScriptB]E2[[i]]+\[ScriptB]U2[[i]];
dNoverY2[[i-1]]=(1-kapshare)/(R)*(GP2[[i]]*\[ScriptCapitalB]2[[i]]-\[ScriptCapitalB]2[[i-1]])-(GP2[[i]]-1)*KoverY;(* see seventh equation *)
NoverY2[[i-1]]=GP2[[i]]*((1-kapshare)*\[ScriptCapitalB]2[[i]]/(R)-KoverY);(* see sixth equation *)
];
GP2 = Most[GP2];

(* Reset the parameters and other values: *)
resetParams;
calculateValues;

riskyStyle = {Thickness[Medium],Black};
noriskStyle = {Thickness[Medium],Black,Dashing[0.02]};
AR = 1/3;
IS = {400};
(* Create the GDP growth subplot: *)
GPsubplot = Show[
ListPlot[Transpose[{time,GP1}],Joined->True,PlotStyle->riskyStyle],
ListPlot[Transpose[{time,GP2}],Joined->True,PlotStyle->noriskStyle],
Frame->True,
FrameTicks->{{-5,0,5,10,15,20,25,30},{1.02,1.04,1.06,1.08,{1.10,"1.10"}},None,None},
FrameLabel->{"","\!\(\*SubscriptBox[\"Y\", 
RowBox[{\"t\", \"+\", \"1\"}]]\)/\!\(\*SubscriptBox[\"Y\", \"t\"]\)"},
AxesOrigin->{-20,-20},
PlotRange->{{-9,30},{1.02,1.10}},
AspectRatio->AR,
ImageSize->IS
];

(* Creare the net foreign assets to GDP subplot: *)
NoverYsubplot = Show[
ListPlot[Transpose[{time,NoverY1}],Joined->True,PlotStyle->riskyStyle],
ListPlot[Transpose[{time,NoverY2}],Joined->True,PlotStyle->noriskStyle],
Graphics[Text["No risk increase",{14,-0.8}]],
Graphics[Text["With risk increase",{21,0.3}]],
Frame->True,
FrameTicks->{{-5,0,5,10,15,20,25,30},{-2,-1,0,1},None,None},
FrameLabel->{"","\!\(\*SubscriptBox[\"N\", \"t\"]\)/\!\(\*SubscriptBox[\"Y\", \"t\"]\)"},
AxesOrigin->{-20,-20},
PlotRange->{{-9,30},{-2,1}},
AspectRatio->AR,
ImageSize->IS
];

(* Create the GDP outflows to GDP subplot: *)
dNoverYsubplot = Show[
ListPlot[Transpose[{time,dNoverY1}],Joined->True,PlotStyle->riskyStyle],
ListPlot[Transpose[{time,dNoverY2}],Joined->True,PlotStyle->noriskStyle],
Frame->True,
FrameTicks->{{-5,0,5,10,15,20,25,30},{-0.2,-0.1,{0,"0.0"},0.1,0.2},None,None},
FrameLabel->{"","(\!\(\*SubscriptBox[\"N\", \"t\"]\)-\!\(\*SubscriptBox[\"N\", 
RowBox[{\"t\", \"-\", \"1\"}]]\))/\!\(\*SubscriptBox[\"Y\", \"t\"]\)"},
AxesOrigin->{-20,-20},
PlotRange->{{-9,30},{-0.2,0.2}},
AspectRatio->AR,
ImageSize->IS
];

(* Create the transitional dynamics figure by putting the three subplots together: *)
transDyn = Show[
GraphicsGrid[{{GPsubplot},{NoverYsubplot},{dNoverYsubplot}}]
];

(* Save the figure: *)
Export[FigsDir<>"/"<>"transDyn.pdf",transDyn];
Export[FigsDir<>"/"<>"transDyn.png",transDyn,ImageSize->FullPageSize];
];
