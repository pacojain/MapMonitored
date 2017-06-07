(* ::Package:: *)

BeginPackage["MapMonitored`"]


MapMonitored::usage = "MapMonitored[f, expr] works like Map, but displays a dynamically updated index of the element currently being operated upon."
CheckMapMonitored::usage = "CheckMapMonitored[f, expr] works like MapMonitored, but additionally exits the Map operation (or performs another action, as specified by the option \"MessageFunction\", on Sequence[index, item value]) whenever a message is generated."
DoMonitored::usage = "DoMonitored[f, expr] works like Do, but displays a dynamically updated index of the element currently being operated upon."
Begin["`Private`"]


ClearAll[MapMonitored]
MapMonitored[f_, list_, opts: OptionsPattern[{"DisplayFunction" -> None}]]:= Module[
	{g, i, currItem},
	g= (i= First@#2; currItem= OptionValue["DisplayFunction"][#1]; f[#1])&;
	Switch[OptionValue["DisplayFunction"],
		None, Block[{i=2}, Monitor[MapIndexed[g, list], If[Head[list] === Association, Row[{First[i], " (", Position[Keys[list], First[i]][[1, 1]], "/", Length[list], ")"}], i]]],
		_, Block[{i=2}, Monitor[MapIndexed[g, list], Column[{i, currItem}]]]
	]
]

ClearAll[CheckMapMonitored]
CheckMapMonitored[f_, list_, opts: OptionsPattern[{"DisplayFunction" -> Automatic, "MessageFunction" -> Automatic}]]:= Module[
	{g, i, messFunc, dispFunc},
	dispFunc = OptionValue["DisplayFunction"] /. Automatic -> (Nothing &);
	messFunc = OptionValue["MessageFunction"] /. Automatic -> Function[{index, item}, Print[Column[{index, dispFunc[item]}]]; Return[item, Module]];
	g= (i= First@#2; Check[f[#1], messFunc[First[#2], #1]])&;
	Block[{i=2}, Monitor[MapIndexed[g, list], If[Head[list] === Association, Row[{First[i], " (", Position[Keys[list], First[i]][[1, 1]], "/", Length[list], ")"}], i]]]
]

ClearAll[DoMonitored]
SetAttributes[DoMonitored, HoldAll]
DoMonitored[body_, iter_, opts: OptionsPattern[{"DisplayFunction" -> None}]]:= Module[
	{count = 0, dispFunc},
	dispFunc= Switch[OptionValue["DisplayFunction"], None, Nothing&, _, OptionValue["DisplayFunction"]];
	Monitor[
		Do[
			count++;
			body,
			iter
		], Switch[ OptionValue["DisplayFunction"], 
			None, count, 
			_, Column[{count, dispFunc[iter[[1]]]}]
		]
	]
]


End[]


EndPackage[]
