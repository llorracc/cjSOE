(* ::Package:: *)

(* Compute the variation of net foreign assets to GDP with respect to unemployment probability (urate): *)
uratevec = Table[i,{i,0.02,0.05,0.01}]; (* Grid of values that urate will take on *)
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
