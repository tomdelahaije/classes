(*
	Copyright 2016 Tom Dela Haije

	Licensed under the Apache License, Version 2.0 (the "License");
	you may not use this file except in compliance with the License.
	You may obtain a copy of the License at

		http://www.apache.org/licenses/LICENSE-2.0

	Unless required by applicable law or agreed to in writing, software
	distributed under the License is distributed on an "AS IS" BASIS,
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	See the License for the specific language governing permissions and
	limitations under the License.
*)

(*Fix for random malformed usage messages*)
System`Dump`fixmessagestring[System`Dump`s_] := ToString@InputForm@System`Dump`s;

(*Begin package*)
Block[{Notation`AutoLoadNotationPalette = False}, BeginPackage["Classes`", {"GeneralUtilities`", "Notation`"}]];

(*Unprotect variables*)
Unprotect[
	Affix,
	AffixTo,
	ClassSet,
	ClassSetDelayed,
	ClassQ,
	DeclareClass,
	DeclareDefaults,
	DeclareInvariant,
	Retrieve,
	UnlockedQ,
	ValidQ
];

(*Usage*)
SetUsage[Affix, 
	"Affix[instance$, element$] returns a copy of instance$ with element$ affixed.", 
	"Affix[element$] represents an operator form of Affix that can be applied to an expression."(**)
];
SetUsage[AffixTo, 
	"AffixTo[symbol$, element$] affixes element$ to the class object represented by symbol$, and sets this as the new value of symbol$."
];
SetUsage[ClassSet, 
	"symbol$\[LeftDoubleBracket]part$\[RightDoubleBracket] \!\(\*OverscriptBox[\(=\), \(\:02c2\)]\) value$ or ClassSet[symbol$\[LeftDoubleBracket]part$\[RightDoubleBracket], value$] sets part$ in the symbol$ representing a class instance to the evaluated value$.",
	"{l$1, l$2, $$} \!\(\*OverscriptBox[\(=\), \(\:02c2\)]\) {r$1, r$2, $$} evaluates the r$i and assigns the results to be the values of the corresponding l$i."(**)
];
SetUsage[ClassSetDelayed,
	"symbol$\[LeftDoubleBracket]part$\[RightDoubleBracket] \!\(\*OverscriptBox[\(:=\), \(\:02c2\)]\) value$ ClassSetDelayed[symbol$\[LeftDoubleBracket]part$\[RightDoubleBracket], value$] sets part$ in the symbol$ representing a class instance to the delayed evaluation of value$."
];
SetUsage[ClassQ, 
	"ClassQ[symbol$] returns True if symbol$ is a declared class, and False otherwise."
];
SetUsage[DeclareClass, 
	"DeclareClass[class$] declares the class$ symbol as a base class with no parents.",
	"DeclareClass[parent$, class$] declares a class$ symbol with properties inherited from parent$."
];
SetUsage[DeclareDefaults, 
	"DeclareDefaults[class$, association$] uses the keys and values in association$ to be the declared keys and their default values for the symbol class$."
];
SetUsage[DeclareInvariant, 
	"DeclareInvariant[class$, f$] declares the invariant for the symbol class$ to be the pure functions f$.",
	"DeclareInvariant[class$, {f$1, f$2, $$}] declares the invariant for the symbol class$ based on the pure functions f$i."
];
SetUsage[Retrieve, 
	"Retrieve[instance$, key$] retrieves the value associated with key$ in the class instance instance$.",
	"Retrieve[instance$, {key$1, key$2, $$}] retrieves the values associated with key$i in the class instance instance$.",
	"Retrieve[{instance$1, instance$2, $$}, key$] retrieves the value associated with key$ in the class instances instance$i.",(**)
	"Retrieve[key$] represents an operator form of Retrieve that can be applied to an expression."(**)
];
SetUsage[UnlockedQ, 
	"UnlockedQ[symbol$] gives True if symbol$ does not have the attribute Locked, and False otherwise."
];
SetUsage[ValidQ, 
	"ValidQ[expr$] gives True if expr$ is a valid class instance, and False otherwise.",
	"ValidQ[expr$, class$] gives True if expr$ is a class instance satisfying the invariant of class$, and False otherwise."
];

(*Messages*)
ClassSet::cfail = "Conversion to `1` failed; the result did not satisfy the corresponding class invariant.";
ClassSet::shape = "Lists `1` and `2` are not the same shape.";
DeclareClass::aldec = "Symbol `1` is alreadey declared to be a class.";
DeclareClass::locked = "Symbol `1` is locked.";
DeclareInvariant::ilinv = "The default elements of `1` do not satisfy the supplied class invariant.";
DeclareDefaults::ilinv = "The supplied options of `1` do not satisfy the class invariant of `2` or one of its superclasses.";
General::affail = "The affix operation failed to produce a valid instance of class `1`.";
General::deccls = "The expression `1` does not correspond to an instance of a declared class.";
General::funarg = "The expression `1` must be a pure function or a list of pure functions.";
General::lock = "The class `1` is locked; either is has the attribute Locked, or the class property \"Editable\" is set to False.";
General::ncls = "The expression `1` does not correspond to a well-defined class.";
General::nedt = "The class `1` is locked; the class property \"Editable\" is set to False.";
General::ninst = "The expression `1` does not correspond to an instance of a well-defined class.";
General::ninstl = "The expression `1` does not correspond to an instance or list of instance of well-defined classes.";
General::pkdcls = "The expression `1` does not correspond to a proper part of a declared class.";
General::propx = "No definition/value was found for the property `1`.";
General::subpx = "No definition/value was found for the property `1` in combination with the supplied subproperties `2`.";
ValidQ::noinv = "The invariant for the class `1` should be a list of pure functions.";
ValidQ::exfail = "The expression fails to satisfy the invariant of the class `1`.";
ValidQ::elfail = "The element `1` fails to satisfy the invariant of the class `2`; the element should `3`.";

(*Options*)
Options[ValidQ] = {"Local" -> False, "Verbose" -> False};

(*Syntax*)
SyntaxInformation[Affix] = {"ArgumentsPattern" -> {_, _.}};
SyntaxInformation[AffixTo] = {"ArgumentsPattern" -> {_, _}};
SyntaxInformation[ClassSet] = {"ArgumentsPattern" -> {_, _}};
SyntaxInformation[ClassSetDelayed] = {"ArgumentsPattern" -> {_, _}};
SyntaxInformation[ClassQ] = {"ArgumentsPattern" -> {_}};
SyntaxInformation[DeclareClass] = {"ArgumentsPattern" -> {_, _.}};
SyntaxInformation[DeclareDefaults] = {"ArgumentsPattern" -> {_, _}};
SyntaxInformation[DeclareInvariant] = {"ArgumentsPattern" -> {_, _}};
SyntaxInformation[Retrieve] = {"ArgumentsPattern" -> {_, _}};
SyntaxInformation[UnlockedQ] = {"ArgumentsPattern" -> {_}};
SyntaxInformation[ValidQ] = {"ArgumentsPattern" -> {_, _., OptionsPattern[]}};

Macros`SetArgumentCount[Affix, {1, 2}];
Macros`SetArgumentCount[AffixTo, 2];
Macros`SetArgumentCount[ClassSet, 2];
Macros`SetArgumentCount[ClassSetDelayed, 2];
Macros`SetArgumentCount[ClassQ, 1];
Macros`SetArgumentCount[DeclareClass, {1, 2}];
Macros`SetArgumentCount[DeclareDefaults, 2];
Macros`SetArgumentCount[DeclareInvariant, 2];
Macros`SetArgumentCount[Retrieve, {1, 2}];
Macros`SetArgumentCount[UnlockedQ, 1];

(*Notation*)
Symbolize[ParsedBoxWrapper[OverscriptBox["=", "\:02c2"]]];
Symbolize[ParsedBoxWrapper[OverscriptBox[":=", "\:02c2"]]];

InfixNotation[ParsedBoxWrapper[OverscriptBox["=", "\:02c2"]], ClassSet];
InfixNotation[ ParsedBoxWrapper[OverscriptBox[":=", "\:02c2"]], ClassSetDelayed];

AddInputAlias["=<" -> ParsedBoxWrapper[OverscriptBox["=", "\:02c2"]]];
AddInputAlias[":=<" -> ParsedBoxWrapper[OverscriptBox[":=", "\:02c2"]]];

Begin["`Private`"];
	
	(*Toggles*)
	$PartOverload = True;
	
	(*Elisions icon*)
	$ElisionsIcon = Graphics[
		{
			GrayLevel[0.5],
			Polygon[{{20, 50}, {72, 94}, {80, 85}, {38, 50}, {80, 15}, {72, 6}, {20, 50}}]
		}, 
		PlotRange -> {{-5, 105}, {-5, 105}}, 
		Background -> GrayLevel[0.93], 
		FrameStyle -> Directive[Thickness[Tiny], GrayLevel[0.7]], 
		Axes -> False, 
		AspectRatio -> 1, 
		ImageSize -> Dynamic[{Automatic, (3.5*CurrentValue["FontCapHeight"])/AbsoluteCurrentValue[Magnification]}], 
		Frame -> True, 
		FrameTicks -> None, 
		FrameStyle -> Directive[Opacity[0.5], Thickness[Tiny], RGBColor[0.368417, 0.506779, 0.709798]]
	];
	
	(*Element operations*)
	Affix[(class_Symbol ? ClassQ)[association_Association ? AssociationQ], elems : (_Rule | _RuleDelayed | {(_Rule | _RuleDelayed) ...})] := Module[{result = Quiet @ class[Append[association, elems]], valid}, 
		
		valid = ValidQ[result];
		
		If[valid, result, Message[Affix::affail, class]] /; valid
		
	];
	Affix[elems : (_Rule | _RuleDelayed | {(_Rule | _RuleDelayed) ...})] := Affix[#, elems] &;
	Affix[(_Symbol ? ClassQ)[_Association ? AssociationQ], elems_] := Null /; Message[Affix::invdt, elems];
	Affix[expr_, _] /; !MatchQ[expr, (_Symbol ? ClassQ)[_Association ? AssociationQ]] := Null /; Message[Affix::deccls, expr];
	Affix[elems_] := Null /; Message[Affix::invdt, elems];
	
	SetAttributes[AffixTo, {HoldFirst}];
	AffixTo[instance_, elems : (_Rule | _RuleDelayed | {(_Rule | _RuleDelayed) ...})] /; MatchQ[instance, (_Symbol ? ClassQ)[_Association ? AssociationQ]] := Module[{result = Quiet @ Affix[instance, elems], valid},
		
		 valid = ValidQ[result];
		 
		 If[valid, instance = result, Message[AffixTo::affail, Head[instance]]] /; valid

	];
	AffixTo[instance_, elems_] /; MatchQ[instance, (_Symbol ? ClassQ)[_Association ? AssociationQ]] := Null /; Message[AffixTo::invdt, elems];
	AffixTo[expr_, _] /; !MatchQ[expr, (_Symbol ? ClassQ)[_Association ? AssociationQ]] := Null /; Message[AffixTo::deccls, expr];
	
	SetAttributes[ClassSet, {HoldFirst, SequenceHold}];
	ClassSet[lhs_List, rhs_List] /; Length[lhs] === Length[rhs] := ClassSet @@@ Thread[Hold[lhs, rhs]];
	ClassSet[Part[instance_, 0], value_Symbol ? ClassQ] /; MatchQ[instance, (_Symbol ? ClassQ)[_Association ? AssociationQ]] := Block[{$PartOverload = False},
		
		Module[{result = instance},
  
	  		Set[Part[result, 0], value];
	  
	  		If[ValidQ[result], instance = result, Message[ClassSet::cfail, value]];
	  
	  		value
	  
	  	]
	  	
	];
	ClassSet[Part[instance_, first : Except[0], args___], value_] /; MatchQ[instance, (_Symbol ? ClassQ)[_Association ? AssociationQ]] := Block[{$PartOverload = False},
		
		Module[{result = instance},
  
	  		Set[Part[result, 1, first, args], value];
	  
	  		If[ValidQ[result], instance = result, Message[ClassSet::affail, Head[instance]]];
	  
	  		value
	  
	  	]
	  	
	]; 
	ClassSet[Part[instance_, 0], value_] /; MatchQ[instance, (_Symbol ? ClassQ)[_Association ? AssociationQ]] := Null /; Message[ClassSet::ncls, value];
	ClassSet[lhs_List, rhs_] := Null /; Message[ClassSet::shape, HoldForm[lhs], rhs];
	ClassSet[expr : Except[_List | (Part[instance_, 0] /; MatchQ[instance, (_Symbol ? ClassQ)[_Association ? AssociationQ]])], _] := Null /; Message[ClassSet::pkdcls, HoldForm[expr]];
	
	SetAttributes[ClassSetDelayed, {HoldAll, SequenceHold}];
	ClassSetDelayed[Part[instance_, first : Except[0], args___], value_] /; MatchQ[instance, (_Symbol ? ClassQ)[_Association]] := Block[{$PartOverload = False},
		
		Module[{result = instance},
  
	  		SetDelayed[Part[result, 1, first, args], value];
	  		
	  		If[ValidQ[result], instance = result, Message[ClassSetDelayed::affail, Head[instance]]];
	  
	  		value
	  
	  	]
	  	
	];
	ClassSetDelayed[expr_, _] := Null /; Message[ClassSetDelayed::pkdcls, expr];
	
	Retrieve[(class_Symbol ? ClassQ)[association_Association ? AssociationQ], key_] := Lookup[Join[class["Defaults"], association], key];
	Retrieve[instances : {(_Symbol ? ClassQ)[_Association ? AssociationQ] ...}, key_] := Retrieve[key] /@ instances;
	Retrieve[key_] := Retrieve[#, key] &;
	Retrieve[expr_, key_] := Null /; Message[Retrieve::ninstl, expr];
	
	(*Class validation*)
	ClassQ[_Symbol] := False;
	ClassQ[arg_] := Null /; Message[ClassQ::sym, arg, 1]; 
	
	(*Symbol validation*)
	UnlockedQ[symbol_Symbol] := FreeQ[Attributes[symbol], Locked]
	UnlockedQ[arg_] := Null /; Message[UnlockedQ::sym, arg, 1];
	
	(*Default instance validation*)
	ValidQ[instance : (head_Symbol ? ClassQ)[_Association ? AssociationQ], opts : OptionsPattern[]] := ValidQ[instance, head, opts];
	ValidQ[instance : (head_Symbol ? ClassQ)[input_Association ? AssociationQ], class_Symbol ? ClassQ, opts : OptionsPattern[]] := Module[
		{
			association = Join[head["Defaults"], input],
			name = SymbolName[class],
			invariants = If[TrueQ[OptionValue["Local"] /. expr : Except[True | False] :> Message[ValidQ::opttf, "Local", expr]], class["ClassInvariant"], class["Invariant"]]
		},	
	
		If[
			
			MatchQ[invariants, {_Function ...}],
			
			If[TrueQ[OptionValue[ValidQ, FilterRules[Options[ValidQ], {opts}], "Verbose"] /. expr : Except[True | False] :> Message[ValidQ::opttf, "Verbose", expr]], Identity, Quiet] @ Check[And @@ (Function[{invariant}, TrueQ[invariant[association]] /. False :> (Message[ValidQ::exfail, name]; False)] /@ invariants), False, ValidQ::elfail],
			
			Message[ValidQ::noinv, name];
			False
			
		]
		
	];
	ValidQ[_, _Symbol ? ClassQ, OptionsPattern[]] := False;
	ValidQ[_, expr_, OptionsPattern[]] := Null /; Message[ValidQ::ncls, expr];
	expr : ValidQ[_, _Symbol ? ClassQ, args__] := Null /; Message[ValidQ::nonopt, {args}, 2, HoldForm[expr]];
	ValidQ[_, OptionsPattern[]] := False;
	
	(*Defaults declaration*)
	DeclareDefaults[class_Symbol ? ClassQ, newDefaults : (_Rule | _RuleDelayed | {(_Rule | _RuleDelayed) ...})] /; TrueQ[class["Editable"]] && UnlockedQ[class] := DeclareDefaults[class, Association[newDefaults]]; 
	DeclareDefaults[class_Symbol ? ClassQ, newDefaults : _Association : Association[]] /; TrueQ[class["Editable"]] && UnlockedQ[class] && AssociationQ[newDefaults] := Module[
		{
			classDefaults = Join[class["ClassDefaults"], newDefaults]
		},
		
		If[ValidQ[class[Join[class["ParentDefaults"], classDefaults]]],
			
			(*Set class defaults*)
			Module[{protected},
				
				Internal`WithLocalSettings[
				
					protected = Unprotect[class],
					
					class /: class["ClassDefaults"] = classDefaults;
					
					(instance_class[ToString[#]] := Retrieve[instance, #]) & /@ Keys[KeySelect[newDefaults, StringQ[#] && StringStartsQ[#, "$"] &]],						
					
					Protect@@protected
					
				];
				
			],
			
			Message[DeclareDefaults::ilinv, SymbolName[class]]; 
			
			$Failed
			
		]
						  
  	];
  	DeclareDefaults[class_Symbol ? ClassQ, newDefaults : _Association : Association[]] /; AssociationQ[newDefaults] := Null /; Message[DeclareDefaults::lock, class];
	DeclareDefaults[class_Symbol ? ClassQ, newDefaults_] /; TrueQ[class["Editable"]] && UnlockedQ[class] := Null /; Message[DeclareDefaults::invru, newDefaults];
	DeclareDefaults[class_, _] /; !MatchQ[class, _Symbol ? ClassQ] := Null /; Message[DeclareDefaults::ncls, class];
	
	(*Invariant declaration*)
	DeclareInvariant[class_Symbol ? ClassQ, newInvariant : ({_Function ...} | _Function) : {}] /; TrueQ[class["Editable"]] && UnlockedQ[class] := Module[
		{
			classInvariant = DeleteDuplicates @ Join[Flatten[{newInvariant}], class["ClassInvariant"]]
		},
		
		If[
			
			And @@ (Function[{invariant}, invariant[class["Defaults"]]] /@ DeleteDuplicates @ Join[classInvariant, class["ParentInvariant"]]), 
			
			(*Set class invariant*)
			Module[{protected},
				
				Internal`WithLocalSettings[
				
					protected = Unprotect[class],
					
					class /: class["ClassInvariant"] = classInvariant,						
					
					Protect@@protected
					
				];
				
			],
			
			Message[DeclareInvariant::ilinv, SymbolName[class]]; 
			
			$Failed
			
		]
				
	];
	DeclareInvariant[class_Symbol ? ClassQ, newInvariant : ({_Function ...} | _Function) : {}] := Null /; Message[DeclareInvariant::lock, class];
	DeclareInvariant[_Symbol ? ClassQ, newInvariant_] := Null /; Message[DeclareInvariant::funarg, newInvariant];
	DeclareInvariant[class_, _] /; !MatchQ[class, _Symbol ? ClassQ] := Null /; Message[DeclareInvariant::ncls, class];
	
	(*Class declaration*)
	DeclareClass[class_Symbol] /; Not[ClassQ[class]] && UnlockedQ[class] := Module[
		{
	    	className = SymbolName[class]
	    },
		
		Internal`WithLocalSettings[
		
			Unprotect[class],
			
			(*Usage*)
			SetUsage[class, className <> "[args$] represents an instance of the class " <> className <> ", with elements stored in the arguments args$."];
			
			(*Syntax*)
			SyntaxInformation[class] = {"ArgumentsPattern" -> {___, OptionsPattern[]}};
					    
			Begin["`Private`"];
			
				(*Declare class*)
				class /: ClassQ[class] = True;
				
				(*Default instantiation*)
				class[elems : (_Rule | _RuleDelayed | {(_Rule | _RuleDelayed) ...}) : {}] := class[Association[elems]];
			
				(*Normal*)
				class /: Normal[class[association_Association ? AssociationQ]] := Join[class["Defaults"], association];
				
				(*Part*)
				class /: Part[instance : class[association_Association ? AssociationQ], first : Except[0], args___] /; $PartOverload := Part[Join[class["Defaults"], association], first, args];
						
				(*Class property inheritance*)
				Module[{guard = True},
					
					(input : class[___]) /; guard := Block[{guard = False, result},
						
						result = Quiet[input, {class::propx, class::subpx}];
							
						result /; result =!= Unevaluated[input]
						
					]
				
				];
				class[str_String] := Null /; Message[class::propx, str];
				class[str_String, subprops__] := Null /; Message[class::subpx, str, {subprops}];
				class[properties : {_String ..}] := class /@ properties;
				
				(*Instance property inheritence*)
				Module[{guard = True},
					
					(input : _class[___]) /; guard := Block[{guard = False, result},
						
						result = Quiet[input, {class::propx, class::subpx}];
							
						result /; result =!= Unevaluated[input]
						
					]
				
				];
				_class[str_String] := Null /; Message[class::propx, str];
				_class[str_String, subprops__] := Null /; Message[class::subpx, str, {subprops}];
			    instance_class[properties : {_String ..}] := instance /@ properties;
			
				(*Set properties*)
				class["Base"] = class;
				class["Class"] = class;
				class["ClassDefaults"] = Association[];
				class["ClassInvariant"] = {};
				class["Defaults"] := class["ClassDefaults"];
				class["Editable"] = True;
				class["Format"] = Function[{instance, fmt},
			    	
			    	Module[{head = Head[instance], alwaysGrid, sometimesGrid},
			    	
				    	alwaysGrid = {
				   			BoxForm`MakeSummaryItem[{"Base: ", head["Base"]}, fmt],
				   			BoxForm`MakeSummaryItem[{"Valid: ", ValidQ[instance]}, fmt]
				   		};
				    	
				    	sometimesGrid = {
				    		BoxForm`MakeSummaryItem[{"Parents: ", ElisionsDump`expandablePane[head["Parents"]]}, fmt],
				   			BoxForm`MakeSummaryItem[{"Keys: ", ElisionsDump`expandablePane[Style @@@ Normal @ KeySort @ Merge[{Thread[head["Defaults", "Public", "Keys"] -> Plain], Thread[(Select[Keys @@ instance, StringQ[#] && StringStartsQ[#, "$"] &]) -> Bold]}, Prepend[#, Italic] &]]}, fmt],
				   			BoxForm`MakeSummaryItem[{"ByteCount: ", ByteCount[instance]}, fmt]
				   		};
				    
				    	BoxForm`ArrangeSummaryBox[
					   		head, 	
					   		SymbolName[head] <> "[<>]", (*Ideally this should be adapted in the output form when BoxForm`UseTextFormattingQ is True.*) 
					   		$ElisionsIcon, 
					   		alwaysGrid,
					   		sometimesGrid,
					   		fmt,
					   		"Interpretable" -> False					   		
				    	]
			    	
			   		]
			   	
			    ];
			    class["Instance"] = class[Association[]];
				class["Invariant"] := DeleteDuplicates @ Join[class["ClassInvariant"], class["ParentInvariant"]];
				class["ParentDefaults"] = Association[];
				class["ParentInvariant"] = {};
				class["Parents"] = {};
 				class["Properties"] := Sort @ DeleteDuplicates @ Cases[Cases[DownValues[class], (x_HoldPattern :> _) :> x, 1], (Verbatim[Pattern][_, class] | class)[property_String] :> property, Infinity];
				_class["Properties"] := Sort @ DeleteDuplicates @ Cases[Cases[SubValues[class], (x_HoldPattern :> _) :> First[x], 1], (Verbatim[Pattern][_, Verbatim[_class]] | Verbatim[_class])[string_String] :> string, Infinity];
				
			    class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", "All"] := class[association];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", "Private"] := KeySelect[class[association], Not[StringQ[#] && StringStartsQ[#, "$"]] &];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", "Public"] := KeySelect[class[association], StringQ[#] && StringStartsQ[#, "$"] &];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", select : ("All" | "Private" | "Public") : "All", "Association"] := class[association, select];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", select : ("All" | "Private" | "Public") : "All", "Dataset"] := Dataset[class[association, select]];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", select : ("All" | "Private" | "Public") : "All", "Keys"] := Keys[class[association, select]];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", select : ("All" | "Private" | "Public") : "All", "Normal"] := Normal[class[association, select]];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", select : ("All" | "Private" | "Public") : "All", "Rules"] := class[association, select, "Normal"];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", select : ("All" | "Private" | "Public") : "All", "Values"] := Values[class[association, select]];
				class[invariant : "ClassInvariant" | "Invariant" | "ParentInvariant", "Function"] := Function[{association}, And @@ (#[association] & /@ class[invariant])];
				class[invariant : "ClassInvariant" | "Invariant" | "ParentInvariant", "List"] := class[invariant];
				class[symbol : "Base" | "Class", "String"] := SymbolName[class[symbol]];
			    class[symbol : "Base" | "Class", "Symbol"] := symbol;
			    class["Parents", "String"] := SymbolName /@ class["Parents"];
			    class["Parents", "Symbol"] := class["Parenst"];
			    
				(*Descriptions*)
				class["Base", "Description"] = "The parent base class that has no parents.";
				class["Class", "Description"] = "The class.";
				class["ClassDefaults", "Description"] = "The defaults specified for the class, excluding the inherited defaults.";
				class["ClassInvariant", "Description"] = "The invariant specified for the class, excluding the inherited invariant.";
				class["Defaults", "Description"] = "The defaults specified for the class.";
				class["Editable", "Description"] = "The boole indicating whether additional children, defaults, and invariants can be supplied for the class.";
				class["Format", "Description"] = "The function that determines the formatting of the class through MakeBoxes.";
			    class["Instance", "Description"] = "A default instance of the class.";
				class["Invariant", "Description"] = "The invariant specified for the class.";
				class["Parent", "Description"] = "The parent class. This property is only defined for classes who have a parent class.";
				class["ParentDefaults", "Description"] = "The defaults specified for the parent class, inherited by class unless overwritten by DeclareDefaults.";
				class["ParentInvariant", "Description"] = "The invariant specified for the parent class, inherited by class unless overwritten by DeclareInvariant.";
				class["Parents", "Description"] = "The list of all parents of the class.";
				class["Properties", "Description"] = "A list of requestable class properties.";
				_class["Properties", "Description"] = "A list of requestable instance properties.";
							
				(*Define default instance format*)
			    class /: MakeBoxes[input : class[_Association], fmt_] := class["Format"][input, fmt];
			    
			End[],
   	
   			(*Set attributes*)
   		    Attributes[class] = {Protected, ReadProtected}
			
		];
		
	];
	DeclareClass[class_Symbol ? ClassQ] /; ClassQ[class] && UnlockedQ[class] := Null /; Message[DeclareClass::aldec, class];
	DeclareClass[class_Symbol] /; Not[ClassQ[class]] && (!UnlockedQ[class]) := Null /; Message[DeclareClass::locked, class];
	DeclareClass[arg_] /; !MatchQ[arg, _Symbol] := Null /; Message[DeclareClass::sym, arg, 1];
	
	DeclareClass[parent_Symbol ? ClassQ, class_Symbol] /; TrueQ[parent["Editable"]] && Not[ClassQ[class]] && UnlockedQ[class] := Module[
		{
	    	className = SymbolName[class]
	    },
		
		Internal`WithLocalSettings[
		
			Unprotect[class],
			
			(*Usage*)
			SetUsage[class, className <> "[args$] represents an instance of the class " <> className <> ", with elements stored in the arguments args$."];
			
			(*Syntax*)
			SyntaxInformation[class] = {"ArgumentsPattern" -> {___, OptionsPattern[]}};
					    
			Begin["`Private`"];
			
				(*Declare class*)
				class /: ClassQ[class] = True;
				
				(*Default instantiation*)
				class[elems : (_Rule | _RuleDelayed | {(_Rule | _RuleDelayed) ...}) : {}] := class[Association[elems]];
			
				(*Normal*)
				class /: Normal[class[association_Association ? AssociationQ]] := Join[class["Defaults"], association];
				
				(*Part*)
				class /: Part[instance : class[association_Association ? AssociationQ], first : Except[0], args___] /; $PartOverload := Part[Join[class["Defaults"], association], first, args];
					
				(*Class property inheritance*)
				Module[{guard},
		  			
		  			(input : class[str_String, subprops___]) /; !TrueQ[guard[Unevaluated @ input]] := Block[{guard, result},
		    
		      			guard[Unevaluated @ input] = True;
		      
		      			result = Quiet[input, {class::propx, class::subpx}];
		      
		      			If[
		       				
		       				result =!= Unevaluated[input],
		       				
		       				result,
		       				
		       				Update[class]; 
		       				parent[str, subprops]
		       			
		       			]
		      
		      		]
		      			
		  		];
		  		class[properties : {_String ..}] := class /@ properties;
				
				(*Instance property inheritence*)
				Module[{guard},
		  
		  			(input : class[assoc_Association ? AssociationQ][str_String, subprops___]) /; !TrueQ[guard[Unevaluated @ input]] := Block[{guard, result},
		    
		      			guard[Unevaluated @ input] = True;
		      
		      			result = Quiet[input, {class::propx, class::subpx}];
		      
		      			If[
		       				
		       				result =!= Unevaluated[input],
		       				
		       				result,
		       				
		       				Update[class];
		       				parent[Join[class["Defaults"], assoc]][str, subprops]
		       				
		       			]
		      
		      		]
		      			
		  		];
		  		instance_class[properties : {_String ..}] := instance /@ properties;
			
				(*Set properties*)
				class["Class"] = class;
				class["ClassDefaults"] = Association[];
				class["ClassInvariant"] = {};
				class["Defaults"] := Join[class["ParentDefaults"], class["ClassDefaults"]];
				class["Editable"] = True;
				class["Instance"] = class[Association[]];
				class["Invariant"] := DeleteDuplicates @ Join[class["ClassInvariant"], class["ParentInvariant"]];
				class["ParentDefaults"] := parent["Defaults"];
				class["ParentInvariant"] := parent["Invariant"];
				class["Parent"] = parent;
				class["Parents"] = Prepend[parent["Parents"], parent];
				class["Properties"] := With[{parentprops = parent["Properties"]}, 
			     
			     	Sort @ DeleteDuplicates @ Join[
			     		DeleteDuplicates @ Cases[Cases[Cases[DownValues[class], (x_HoldPattern :> _) :> x, 1], (Verbatim[Pattern][_, class] | class)[property_String] :> property, Infinity], Except[Alternatives @@ parentprops]],
			     		parentprops
			     	]
			     	
			    ];
				_class["Properties"] := With[{baseprops = parent[]["Properties"]}, 
			     
			     	Sort @ DeleteDuplicates @ Join[
			     		DeleteDuplicates @ Cases[Cases[Cases[SubValues[class], (x_HoldPattern :> _) :> First[x], 1], (Verbatim[Pattern][_, Verbatim[_class]] | Verbatim[_class])[string_String] :> string, Infinity], Except[Alternatives @@ baseprops]],
			     		baseprops
			     	]
			     	
			    ];
								
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", "All"] := class[association];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", "Private"] := KeySelect[class[association], Not[StringQ[#] && StringStartsQ[#, "$"]] &];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", "Public"] := KeySelect[class[association], StringQ[#] && StringStartsQ[#, "$"] &];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", select : ("All" | "Private" | "Public") : "All", "Association"] := class[association, select];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", select : ("All" | "Private" | "Public") : "All", "Dataset"] := Dataset[class[association, select]];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", select : ("All" | "Private" | "Public") : "All", "Keys"] := Keys[class[association, select]];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", select : ("All" | "Private" | "Public") : "All", "Normal"] := Normal[class[association, select]];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", select : ("All" | "Private" | "Public") : "All", "Rules"] := class[association, select, "Normal"];
				class[association : "ClassDefaults" | "Defaults" | "ParentDefaults", select : ("All" | "Private" | "Public") : "All", "Values"] := Values[class[association, select]];
				class[invariant : "ClassInvariant" | "Invariant" | "ParentInvariant", "Function"] := Function[{association}, And @@ (#[association] & /@ class[invariant])];
				class[invariant : "ClassInvariant" | "Invariant" | "ParentInvariant", "List"] := class[invariant];
				class[symbol : "Base" | "Class" | "Parent", "String"] := SymbolName[class[symbol]];
			    class[symbol : "Base" | "Class" | "Parent", "Symbol"] := symbol;
			    class["Parents", "String"] := SymbolName /@ class["Parents"];
			    class["Parents", "Symbol"] := class["Parenst"];
			    
			    (*Define default instance format*)
			    class /: MakeBoxes[input : class[_Association], fmt_] := class["Format"][input, fmt];
			   			
			End[],
			
   			(*Set attributes*)
   		    Attributes[class] = {Protected, ReadProtected}
			
		];
		
	];
	DeclareClass[parent_Symbol ? ClassQ, class_Symbol] /; TrueQ[parent["Editable"]] && ClassQ[class] && UnlockedQ[class] := Null /; Message[DeclareClass::aldec, class];
	DeclareClass[parent_Symbol ? ClassQ, class_Symbol] /; !TrueQ[parent["Editable"]] && Not[ClassQ[class]] && UnlockedQ[class] := Null /; Message[DeclareClass::nedt, parent];
	DeclareClass[parent_Symbol ? ClassQ, class_Symbol] /; TrueQ[parent["Editable"]] && Not[ClassQ[class]] && !UnlockedQ[class] := Null /; Message[DeclareClass::locked, class];
	DeclareClass[parent_Symbol ? ClassQ, class_] /; !MatchQ[class, _Symbol] := Null /; Message[DeclareClass::sym, 2, class];
	DeclareClass[parent_, _] /; !MatchQ[parent, _Symbol ? ClassQ] := Null /; Message[DeclareClass::ncls, parent];
	
	(*Bulletproofing*)
	SetAttributes[
		{
			Affix,
			AffixTo,
			ClassSet,
			ClassSetDelayed,
			ClassQ,
			DeclareClass,
			DeclareDefaults,
			DeclareInvariant,
			Retrieve,
			UnlockedQ,
			ValidQ
		}, 
		{Protected, ReadProtected}
	];
	
End[];

(*End package*)
EndPackage[];