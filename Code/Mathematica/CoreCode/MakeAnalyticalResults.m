(* ::Package:: *)

(* This file constructs analytical formulae for the derivatives of the Euler equation *)
(* These formulae could be derived (tediously) by hand, but such derivations are error-prone *)
(* Once derived, the formulae automatically update when parameter values are changed *)


(* This file should be executed before parameter values are defined, enforced by the following line *)
If[NameQ["ParamsAreSet"]==True,If[ParamsAreSet==True,Print["MakeAnalyticalResults should be executed before parameter values are defined."];Abort[]]];

(* Derivations necessary for calculating derivatives of consumption function at the target *)

(* Start with Euler equation which reflects the first order condition *)
Euler1 = {u'[cEfromb[\[ScriptB]Et]] == \[CapitalThorn]\[CapitalGamma]^\[Rho] (\[Mho] u'[\[Kappa](((1-\[Tau]ForStake)\[ScriptB]Et-cEfromb[\[ScriptB]Et]+(1-\[Tau]ForUI))\[ScriptCapitalR]+\[Stigma])]+(1-\[Mho]) u'[cEfromb[((1-\[Tau]ForStake)\[ScriptB]Et-cEfromb[\[ScriptB]Et]+(1-\[Tau]ForUI)) \[ScriptCapitalR]]])}; 
(* JY: Your version was
Euler1 = {u'[cEfromb[\[ScriptB]Et]] == \[CapitalThorn]\[CapitalGamma]^\[Rho] (\[Mho] u'[\[Kappa] (\[ScriptB]Et-cEfromb[\[ScriptB]Et]+(1-\[Tau]ForUI)+\[Stigma]) \[ScriptCapitalR]]+(1-\[Mho]) u'[cEfromb[(\[ScriptB]Et-cEfromb[\[ScriptB]Et]+(1-\[Tau]ForUI)) \[ScriptCapitalR]]])}; 
Please verify that my change is correct, and if so delete this comment
*)
(* Differentiate the Euler equation with respect to \[ScriptB]Et *)
Euler2 = D[Euler1,\[ScriptB]Et];
Euler3 = D[Euler2,\[ScriptB]Et];
Euler4 = D[Euler3,\[ScriptB]Et];
Euler5 = D[Euler4,\[ScriptB]Et];

(* Substitute values at the target to obtain an implicit equation for the MPC at the target *)
Euler2AtTarget =
 (
   (
     (
       (
         (
           (
             Euler2 
                /. \[ScriptB]Et -> \[ScriptB]E
             ) /. cEfromb[\[ScriptB]E] -> \[ScriptC]E
           ) /.(1-\[Tau]ForUI-\[ScriptC]E + (1-\[Tau]ForStake)\[ScriptB]E) \[ScriptCapitalR] -> \[ScriptB]E
         ) /. \[ScriptB]E -> \[ScriptB]E 
       ) /. \[Kappa] (\[ScriptB]E+\[Stigma])  -> \[ScriptC]U
     ) /. cEfromb[\[ScriptB]E] -> \[ScriptC]E
   ) /. cEfromb'[\[ScriptB]E] -> \[ScriptC]EP;


(* Implicit equation for cEfromb'' *)
Euler3AtTarget =
    (
      (
        (
          (
              (
                (
                Euler3 /. \[ScriptB]Et -> \[ScriptB]E
                ) /. cEfromb[\[ScriptB]E] -> \[ScriptC]E
              ) /. (1-\[Tau]ForUI-\[ScriptC]E + (1-\[Tau]ForStake)\[ScriptB]E) \[ScriptCapitalR] -> \[ScriptB]E
          ) /. \[Kappa] (\[ScriptB]E+\[Stigma])  -> \[ScriptC]U
        ) /. cEfromb[\[ScriptB]E] -> \[ScriptC]E
      ) /. cEfromb'[\[ScriptB]E] -> \[ScriptC]EP
    ) /. cEfromb''[\[ScriptB]E] -> \[ScriptC]EPP;


(* Implicit equation for cEfromb''' *)
Euler4AtTarget =
  (
    (
      (
        (
          (
              (
                (
                Euler4 /. \[ScriptB]Et -> \[ScriptB]E
                ) /. cEfromb[\[ScriptB]E] -> \[ScriptC]E
              ) /. (1-\[Tau]ForUI-\[ScriptC]E + (1-\[Tau]ForStake)\[ScriptB]E) \[ScriptCapitalR] -> \[ScriptB]E
          ) /. \[Kappa] (\[ScriptB]E+\[Stigma])  -> \[ScriptC]U
        ) /. cEfromb[\[ScriptB]E] -> \[ScriptC]E
      ) /. cEfromb'[\[ScriptB]E] -> \[ScriptC]EP
    ) /. cEfromb''[\[ScriptB]E] -> \[ScriptC]EPP
  ) /. cEfromb'''[\[ScriptB]E] -> \[ScriptC]EPPP;


 (* Implicit equation for cEfromb'''' *)
 Euler5AtTarget =
 (
   (
     (
       (
         (
           (
               (
                 (
                 Euler5 /. \[ScriptB]Et -> \[ScriptB]E
                 ) /. cEfromb[\[ScriptB]E] -> \[ScriptC]E
               ) /. (1-\[Tau]ForUI-\[ScriptC]E + (1-\[Tau]ForStake)\[ScriptB]E) \[ScriptCapitalR] -> \[ScriptB]E
           ) /. \[Kappa] (\[ScriptB]E+\[Stigma])  -> \[ScriptC]U
         ) /. cEfromb[\[ScriptB]E] -> \[ScriptC]E
       ) /. cEfromb'[\[ScriptB]E] -> \[ScriptC]EP
     ) /. cEfromb''[\[ScriptB]E] -> \[ScriptC]EPP
   ) /. cEfromb'''[\[ScriptB]E] -> \[ScriptC]EPPP
 ) /. cEfromb''''[\[ScriptB]E] -> \[ScriptC]EPPPP;


(* Solve for analytical formulae for derivatives of the consumption functions at the target *)
\[ScriptC]EPAnalytical    = \[ScriptC]EP    /. Solve[Euler2AtTarget, \[ScriptC]EP];  (* This is a quadratic; later must select the solution in the range [0,1] *)
\[ScriptC]EPPAnalytical   = \[ScriptC]EPP   /. Solve[Euler3AtTarget, \[ScriptC]EPP][[1]]; (* These are linear solutions, given the lower-order derivatives *)
\[ScriptC]EPPPAnalytical  = \[ScriptC]EPPP  /. Solve[Euler4AtTarget, \[ScriptC]EPPP][[1]];
\[ScriptC]EPPPPAnalytical = \[ScriptC]EPPPP /. Solve[Euler5AtTarget, \[ScriptC]EPPPP][[1]]; (* Needed only for the CDC version that uses second derivatives in interpolation *)


(* Now construct analytical solution for parameters of extrapolating function using the first four derivatives *)
(* There will be two solutions which are numerically equivalent; we use the first, arbitrarily *)

Off[Solve::"ifun"]; (* Turn off an error message that warns about the solution -- it's OK *)
ClearAll[\[Phi]Solve0, \[Phi]Solve1, \[Gamma]Solve0, \[Gamma]Solve1];
ClearAll[s0, s1, s2, s3];
{ExpParams1,ExpParams2} = Assuming[{{\[Phi]Solve0,\[Phi]Solve1,\[Gamma]Solve0,\[Gamma]Solve1} \[Element] Reals}, Solve[{
    s0 ==            Exp[\[Phi]Solve0] +           Exp[\[Gamma]Solve0]
  , s1 == -\[Phi]Solve1   Exp[\[Phi]Solve0] - \[Gamma]Solve1   Exp[\[Gamma]Solve0]
  , s2 ==  \[Phi]Solve1^2 Exp[\[Phi]Solve0] + \[Gamma]Solve1^2 Exp[\[Gamma]Solve0]
  , s3 == -\[Phi]Solve1^3 Exp[\[Phi]Solve0] - \[Gamma]Solve1^3 Exp[\[Gamma]Solve0]}
 , {\[Phi]Solve0, \[Phi]Solve1, \[Gamma]Solve0, \[Gamma]Solve1}]];

\[Phi]Analytical0 = ((\[Phi]Solve0 /. ExpParams1) /. C[2] -> 0);
\[Phi]Analytical1 = \[Phi]Solve1 /. ExpParams1;
\[Gamma]Analytical0 = \[Gamma]Solve0 /. ExpParams1;
\[Gamma]Analytical1 = \[Gamma]Solve1 /. ExpParams1;
On[Solve::"ifun"]; 
