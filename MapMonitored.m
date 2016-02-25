(* ::Package:: *)

BeginPackage["MapMonitored`"]


MapMonitored::usage = "MapMonitored[f, expr] works like Map, but displays a dynamically updated index of the element currently being operated upon."
CheckMapMonitored::usage = "CheckMapMonitored[f, expr] works like MapMonitored, but additionally exits the Map operation (or performs the action specified by the option \"MessageFunction\") whenever a message is generated."
Begin["`Private`"]


ClearAll[MapMonitored]
MapMonitored[f_, list_, opts: OptionsPattern[{"DisplayFunction" -> None}]]:= Module[
	{g, i, currItem},
	g= (i= First@#2; currItem= OptionValue["DisplayFunction"][#1]; f[#1])&;
	Switch[OptionValue["DisplayFunction"],
		None, Block[{i=2}, Monitor[MapIndexed[g, list], i]],
		_, Block[{i=2}, Monitor[MapIndexed[g, list], Column[{i, currItem}]]]
	]
]


ClearAll[CheckMapMonitored]
CheckMapMonitored[f_, list_, opts: OptionsPattern[{"DisplayFunction" -> None, "MessageFunction" -> None}]]:= Module[
	{g, i, currItem},
	Switch[OptionValue["DisplayFunction"],
		None, g= (i= First@#2; Check[f[#1], Print[i]; Return[currItem, Module]])&;
			  Block[{i=2}, Monitor[MapIndexed[g, list], i]],
		_,    g= (i= First@#2; currItem= OptionValue["DisplayFunction"][#1]; Check[f[#1], Print[Column[{i, currItem}]]; Return[currItem, Module]])&;
			  Block[{i=2}, Monitor[MapIndexed[g, list], Column[{i, currItem}]]]
	]
]


End[]


EndPackage[]
