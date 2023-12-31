BeginPackage["FaizonZaman`MacroTools`"]

(* Map *)
SequencedMap::usage="SequencedMap[f, args] Maps f over the sequence args and returns the result as a sequence"
SplicedMap::usage="SplicedMap[f, expr] Maps f over expr and wraps the result in Splice"

(* Graph *)
DirectedFan::usage = "DirectedFan[p, {c1, c2, ...}] returns a list of directed edges from p to each ci"
UndirectedFan::usage = "UndirectedFan[p, c1] returns a list of undirected edges from p to each ci"
EdgeFan::usage = "EdgeFan[e[p, c1]] returns a list of edges from p to each ci with the edge type e"

TaggedDirectedFan::usage = "TaggedDirectedFan[p, {c1, c2, ...}] returns a list of directed edges from p to each ci with integer tags"
TaggedUndirectedFan::usage = "TaggedUndirectedFan[p, c1] returns a list of undirected edges from p to each ci with integer tags"
TaggedEdgeFan::usage = "TaggedEdgeFan[e[p, c1]] returns a list of edges with edge type e from p to each ci with integer tags"

Optioned::usage = "Optioned[f, opts] applies function f with options opts to an expression"
ProcessBy::usage = "ProcessBy[p[f1, f2, ...]] is an operator form of Through"
AgendaTitle::usage = "AgendaTitle[date] creates agenda cells for the given date"
FlattenOn::usage = "FlattenOn[level] represents an operator form of Flatten[expr, level]\nFlattenOn[level, head] represents an operator form of Flatten[expr, level, head]"

OddIndex::usage = "OddIndex[list] returns the odd-indexed elements of list"
EvenIndex::usage = "EvenIndex[list] returns the even-indexed elements of list"

NormalFixedPoint::usage = "NormalFixedPoint[expr] repeatedly applies Normal until the expression no longer changes"

(* TODO: ItemCases         *)
(* TODO: HighlightQuery *)
(* ^^ Highlight Ascending and Descending operators in Query *)

(* TODO: $SystemDocumentationDirectory  *)
(* TODO: $ExampleDataDirectory          *)
(* TODO: FindExampleDataFiles           *)

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


(* Graphs *)

(* Generate edges from a parent (or list of parents) to a list of children *)
DirectedFan[parents_List,children_List] := Flatten[ (parent |-> DirectedFan[parent, children]) /@ parents ]
UndirectedFan[parents_List,children_List] := Flatten[ (parent |-> UndirectedFan[parent, children]) /@ parents ]
EdgeFan[((edge: DirectedEdge|UndirectedEdge)[parents_List,children_List])] := Flatten[ (parent |-> EdgeFan[edge[parent, children]]) /@ parents ]

DirectedFan[parent_,children_List] := Thread[DirectedEdge[parent, children]]
UndirectedFan[parent_,children_List] := Thread[UndirectedEdge[parent, children]]
EdgeFan[edge: ((DirectedEdge|UndirectedEdge)[parent_,children_List])] := Thread[edge]

DirectedFan[nodes_List] := DirectedFan[First[nodes], Rest[nodes]]
UndirectedFan[nodes_List] := UndirectedFan[First[nodes], Rest[nodes]]
EdgeFan[ ((edge:(DirectedEdge|UndirectedEdge))[nodes_List])] := EdgeFan[edge[First[nodes], Rest[nodes]]]

(* Generate tagged edges from a parent (or list of parents) to a list of children *)
TaggedDirectedFan[parents_List,children_List] := Flatten[ (parent |-> TaggedDirectedFan[parent, children]) /@ parents ]
TaggedUndirectedFan[parents_List,children_List] := Flatten[ (parent |-> TaggedUndirectedFan[parent, children]) /@ parents ]
TaggedEdgeFan[((edge: DirectedEdge|UndirectedEdge)[parents_List,children_List])] := Flatten[ (parent |-> TaggedEdgeFan[edge[parent, children]]) /@ parents ]

TaggedDirectedFan[parent_,children_List] := Thread[DirectedEdge[parent, children,  ( Length /* Range )[ children ] ]]
TaggedUndirectedFan[parent_,children_List] := Thread[UndirectedEdge[parent, children,  ( Length /* Range )[ children ]]]
TaggedEdgeFan[((edge: DirectedEdge|UndirectedEdge)[parent_,children_List])] := Thread[edge[parent, children,  ( Length /* Range )[ children ]]]

TaggedDirectedFan[nodes_List] := TaggedDirectedFan[First[nodes], Rest[nodes]]
TaggedUndirectedFan[nodes_List] := TaggedUndirectedFan[First[nodes], Rest[nodes]]
TaggedEdgeFan[ ((edge:(DirectedEdge|UndirectedEdge))[nodes_List])] := TaggedEdgeFan[edge[First[nodes], Rest[nodes]]]

(* With Explicit Tags *)

TaggedDirectedFan[parents_List,children_List, tags_List] /; (Length[children] === Length[tags]) := Flatten[ (parent |-> TaggedDirectedFan[parent, children, tags]) /@ parents ]
TaggedUndirectedFan[parents_List,children_List, tags_List] /; (Length[children] === Length[tags]) := Flatten[ (parent |-> TaggedUndirectedFan[parent, children, tags]) /@ parents ]
TaggedEdgeFan[((edge: DirectedEdge|UndirectedEdge)[parents_List,children_List, tags_List])] /; (Length[children] === Length[tags]) := Flatten[ (parent |-> TaggedEdgeFan[edge[parent, children, tags]]) /@ parents ]

TaggedDirectedFan[parent_,children_List, tags_List] /; (Length[children] === Length[tags]) := Thread[DirectedEdge[parent, children, tags]]
TaggedUndirectedFan[parent_,children_List, tags_List] /; (Length[children] === Length[tags]) := Thread[UndirectedEdge[parent, children, tags]]
TaggedEdgeFan[((edge: DirectedEdge|UndirectedEdge)[parent_,children_List, tags_List])] /; (Length[children] === Length[tags]) := Thread[edge[parent, children, tags]]

TaggedDirectedFan[nodes_List, tags_List] /; ((Length[nodes] - Length[tags]) === 1) := TaggedDirectedFan[First[nodes], Rest[nodes], tags]
TaggedUndirectedFan[nodes_List, tags_List] /; ((Length[nodes] - Length[tags]) === 1) := TaggedUndirectedFan[First[nodes], Rest[nodes], tags]
TaggedEdgeFan[ ((edge:(DirectedEdge|UndirectedEdge))[nodes_List, tags_List])] /; ((Length[nodes] - Length[tags]) === 1) := TaggedEdgeFan[edge[First[nodes], Rest[nodes], tags]]

(* Operator forms *)

(* Optioned *)
Optioned[h_Symbol, opts : OptionsPattern[]][content_] := h[content, opts]


(* ProcessBy *)
ProcessBy[p_[funcs__]][target__] := Through[p[funcs][target]]
ProcessBy[p_[funcs__], h_][target__] := Through[p[funcs][target], h]

(* FlattenOn *)
FlattenOn[level_][expr_]:=Flatten[expr, level]
FlattenOn[level_, head_][expr_]:=Flatten[expr, level, head]

(* Notebook functions *)
Options[AgendaTitle]={
	"SectionStyle"->"Chapter",
	"Content"->MakeBoxes[AgendaTitle[Tomorrow]],
	"Range"-> Today,
	"DateStringFormat"->{"DayNameShort"," ","DayShort"," ","MonthNameShort"," ","Year"}
};
AgendaTitle[date_DateObject,OptionsPattern[AgendaTitle]]:=NotebookWrite[
    InputNotebook[], 
    {
        Cell[DateString[date,OptionValue["DateStringFormat"]],OptionValue["SectionStyle"]],
        Cell[BoxData[OptionValue["Content"]],"Input"]
        }
    ]
AgendaTitle[date__DateObject,opts:OptionsPattern[AgendaTitle]]:=Map[AgendaTitle[#,opts]&,{date}]
AgendaTitle[dates:{__DateObject},opts:OptionsPattern[AgendaTitle]]:=Map[AgendaTitle[#,opts]&,dates]
AgendaTitle[datespec:{__String},opts:OptionsPattern[AgendaTitle]]:=AgendaTitle[OptionValue["Range"],Sequence[opts,"DateStringFormat"->datespec]]

(* TODO: Add Odd/EvenIndex documentation *)
(* TODO: Add Extract support for Odd/EvenIndex *)
OddIndex[expr_]:= expr[[1;;;;2]]
OddIndex /: expr_[[OddIndex]] := expr[[1;;;;2]] 
(* In[5]:= Range[5][[OddIndex]] *)
(* Out[5]= {1, 3, 5} *)

EvenIndex[expr_]:= expr[[2;;;;2]]
EvenIndex /: expr_[[EvenIndex]] := expr[[2;;;;2]] 
(* In[5]:= Range[5][[EvenIndex]] *)
(* Out[5]= {2, 4} *)


NormalFixedPoint[expr_]:=FixedPoint[Normal,expr]

End[] (* End `Private` *)

EndPackage[]
