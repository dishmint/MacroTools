BeginPackage["FaizonZaman`MacroTools`"]

SequencedMap::usage="SequencedMap[f, args] Maps f over the sequence args and returns the result as a sequence"
SplicedMap::usage="SplicedMap[f, expr] Maps f over expr and wraps the result in Splice"

Begin["`Private`"]

(* Map functions *)
Options[SequencedMap] = {
   "Head" -> Automatic
   };
SequencedMap[f_][args__] := SequencedMap[f, args]
SequencedMap[f_, args__, opts : OptionsPattern[SequencedMap]] := 
    With[
        {
            res = Map[f, {args}],
            head = Replace[OptionValue["Head"], Automatic -> Sequence]
            },
            head @@ res
            ]

Options[SplicedMap] = {
   "Head" -> Automatic
   };
SplicedMap[f_, expr_, opts : OptionsPattern[SplicedMap]] := With[
    {
        res = Map[f, expr],
        head = Replace[OptionValue["Head"], Automatic -> List]
        },
        Splice[res, head]
        ]

End[] (* End `Private` *)

EndPackage[]
