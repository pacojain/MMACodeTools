(* Wolfram Language package *)
BeginPackage["MMACodeTools`"]

CanonicalizeSectionName::usage = "CanonicalizeSectionName[sec_] attempts to convert a header expression (potentially containing Boxes, inline Cells...) into a pure string with sentence capitalization."
SectionNameSameQ::usage = "SectionNameSameQ[secName1, secName2] checks that the two section names are SameQ after canonicalization."
CheckToExpression::usage = "CheckToExpression[expr_] attempts to convert expr to a Hold-wrapped StandardForm expression, supressing messages, and returning Missing[$Failed] if the conversion cannot be completetd."
NestedCells::usage = "NestedCells[nb_, opts] converts a notebook (filename or object) into a nested association with headerCellTypes (Private` variable) converted to keys."
PruneChildren::usage = "PruneChildren[nestedCells_, cellType_] prunes all children of cells matching pattern cellType from nestedCells."
PruneNeighbors::usage = "PruneNeighbors[nestedCells_, cellType_, opts] prunes \"neighbors\" of cells matching pattern cellType from nestedCells."
PruneEmptySections::usage = "PruneEmptySections[nestedCells_, headerCellTypes_] prunes any sections (as defined by Private` variable headerCellTypes) that are empty."
ScrapeNotebook::usage = "ScrapeNotebook[nb_, cellType_, opts] does a typical scrape of a notebook (filename or object) into a nested Association, keeping leaf cells that match pattern cellType."

Begin["`Private`"]
(* canonicalize and compare section names ********************************************************************)
ClearAll[CanonicalizeSectionName]
CanonicalizeSectionName[sectName_] := Module[
	{result = sectName, ruleList},
	result = result //. FormBox[expr_, _] :> expr;
	result = result //. SubscriptBox[a_String, b_String] :> a <> "_" <> b;
	result = result //. StyleBox[str_String, ___] :> str;
	result = result //. RowBox[{args : _String ..}] :> StringJoin[args];
	result = result //. Cell[BoxData[boxes_], ___] :> boxes;
	result = result //. Cell[BoxData[FormBox[str_String, _]]] :> str;
	result = result //. TextData :> StringJoin;
	ruleList = {
		FormBox[expr_, _] :> expr,
		SubscriptBox[a_String, b_String] :> a <> "_" <> b,
		SuperscriptBox[a_, b_] :> a <> "^" <> b,
		StyleBox[str_String, ___] :> str,
		RowBox[{args : _String ..}] :> StringJoin[args],
		BoxData[boxes_] :> boxes,
		Cell[expr_, ___] :> expr
	};
	result = result //. ruleList;
	result = result //. TextData :> StringJoin;
	result = If[Head[result] === String,
		StringSplit[result, Alternatives @@ CharacterRange["0", "9"] | "("] // First,
		Return[result]];
	result = result // StringTrim;
	result = ToUpperCase[StringTake[result, 1]] <> ToLowerCase[StringTake[result, {2 ;;}]]
]
	
ClearAll[SectionNameSameQ]
SectionNameSameQ[name1_, name2_] := CanonicalizeSectionName[name1] === CanonicalizeSectionName[name2]

(* CheckToExpression ***************************************************************************************)
ClearAll[CheckToExpression]
CheckToExpression[expr_] := Quiet[Check[ToExpression[expr, StandardForm, Hold], Missing["$Failed"]]]

(* NestedCells *********************************************************************************************)
headerCellTypes= {"Section", "Subsection", "Subsubsection", "Subsection3", "CommentText"}
cellPatternToScrape = "Input" | "StandardFormEquation"
ClearAll[NestedCells]
Options[NestedCells] = {
	"PruneEmptySections" -> False,
	"HeaderCellTypes" -> headerCellTypes,
	"CellTypeRules" -> {
		"Text" | "Section" | "Subsection" | "Subsubsection" | "Subsection3" | "CommentText" -> Identity,
		 _ -> CheckToExpression
	}
};
NestedCells[filename_String, opts : OptionsPattern[]] := NestedCells[Get[filename], opts];
NestedCells[nbIn_, opts : OptionsPattern[]] := Module[
	{nb, op},
	nb = nbIn //. {Notebook[first_, rest___] :> first, (CellChangeTimes -> _List) :> Unevaluated[Sequence[]]};
	nb = nb //. HoldPattern[Cell[CellGroupData[{first_Cell, rest___Cell}, ___]]] :> first -> {rest};
	nb = nb //. {{Cell[CellGroupData[{cells___}, ___], ___]} :> {cells}, Cell[CellGroupData[{cells___}, ___], ___] :> {cells}};
	If[OptionValue["PruneEmptySections"],
		nb = nb //. {
			{first___, Cell[_, _?(MemberQ[OptionValue["HeaderCellTypes"], #] &), ___], rest___} :> {first, rest},
			HoldPattern[Cell[_, _?(MemberQ[OptionValue["HeaderCellTypes"], #] &), ___] -> {}] :> Nothing
		}
	];
	op = MapAt[Replace[Inactivate[#, Except[Function]], s_Symbol -> Inactive[s]] &, OptionValue["CellTypeRules"], {All, 2}];
	ReplaceRepeated[nb, Function[ctrPair, Quiet[Cell[contents_, ct_?(MatchQ[#, ctrPair[[1]]] &), ___] -> {ct, Part[ctrPair, 2][contents]}]] /@ op] // Activate
]

(* PruneChildren/Neighbors/EmptySections ********************************************************************)
ClearAll[PruneChildren, PruneNeighbors, PruneEmptySections]
PruneChildren[nestedCells_, celltype_: cellPatternToScrape] := nestedCells //. HoldPattern[Rule[lhs : {celltype, _}, _]] :> lhs

Options[PruneNeighbors] = {"HeaderCellTypes" -> headerCellTypes};
PruneNeighbors[nestedCells_, celltype_: cellPatternToScrape, opts : OptionsPattern[]] := Module[
	{
		res = nestedCells, headerPattern = Alternatives @@ OptionValue["HeaderCellTypes"]
	},
	res = res //. {
		{first___, m : {celltype, _}, rest___} :> Join[Select[{first}, MatchQ[#, {celltype, _}] &], {m}, Select[{rest}, MatchQ[#, {celltype, _}] &]],
		Verbatim[Rule][{headerPattern, _}, {_?(FreeQ[#, {celltype, _}] &) ..}] :> Nothing,
		Verbatim[Rule][{headerPattern, _}, {}] :> Nothing
	}
]

PruneEmptySections[nestedCells_, headerCellTypes_List: headerCellTypes] :=
	nestedCells //. {
		wrapper_?(# =!= Rule &)[first___, {Alternatives @@ headerCellTypes, _}, rest___] :> wrapper[first, rest],
		Rule[{Alternatives @@ headerCellTypes, _}, {}] :> Nothing
	}

(* ScrapeNotebook ********************************************************************************************)
ClearAll[ScrapeNotebook]
Options[ScrapeNotebook] = {"HeaderCellTypes" -> headerCellTypes};
ScrapeNotebook[nb_, celltype_: cellPatternToScrape, opts : OptionsPattern[]] := Module[
	{
		res = NestedCells[nb], tempAssoc, headerPattern = Alternatives @@ OptionValue["HeaderCellTypes"]
	 },
	res = PruneChildren[res, celltype];
	res = PruneNeighbors[res, celltype];
	res = PruneEmptySections[res];
	res = res //. {secRules: Verbatim[Rule][{Alternatives @@ headerCellTypes, _}, _] ..} :> tempAssoc[secRules];
	res = res //. Verbatim[Rule][{headerType : headerPattern, headerText_}, data_] :> headerText -> data;
	res = res //. {cellPatternToScrape, body_} :> body;
	res //. tempAssoc :> Association
]


End[]


EndPackage[]
