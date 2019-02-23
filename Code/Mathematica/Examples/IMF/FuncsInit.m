(* ::Package:: *)

(*
This file contains various background routines that are useful for solving dynamic stochastic optimization models
*)

SetSystemOptions["EvaluateNumericalFunctionArgument" -> False];
(* Prevents problematic efforts to evaluate the arguments of numerical functions *)

(* There are 72 'points' to the inch *)

FullPageSize  = {72. 8.5  ,72. 8.5/GoldenRatio};
HalfPageSize  = {72. 6.5  ,72. 6.5/GoldenRatio};
ThirdPageSize = {72. 4.5  ,72. 4.5/GoldenRatio};
LandscapeSize = {72. 11.0,72. 7.5} //N;

SetOptions[Plot          ,BaseStyle->{FontSize->14}];
SetOptions[Plot          ,ImageSize->HalfPageSize];
SetOptions[Plot          ,PlotStyle->{{Black,Thickness[Medium]},{Black,Thickness[Medium]}}];
SetOptions[ListPlot      ,BaseStyle->{FontSize->14}];
SetOptions[ListPlot      ,ImageSize->HalfPageSize];
SetOptions[ParametricPlot,ImageSize->HalfPageSize];
SetOptions[Plot3D        ,ImageSize->HalfPageSize];

