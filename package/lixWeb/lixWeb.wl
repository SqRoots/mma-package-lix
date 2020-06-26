(* ::Package:: *)

BeginPackage["lixWeb`"]

HexToMmaRGBColor::usage =
        "HexToMmaRGBColor[\"#333\"]\:ff0c#123456\:989c\:8272\:8f6cRGBColor
		 \:652f\:6301\:ff1a#333\:ff0c#333333\:ff0c333\:ff0c333333"
TableToHTML::usage =
		"\:4ece\:4e00\:5f20\:4e8c\:7ef4\:8868\:521b\:5efa HTML \:8868\:683c\:ff0c\:7b2c1\:884c\:4e3a\:6807\:9898\:884c"

TableToMarkdown::usage =
		"\:4ece\:4e00\:5f20\:4e8c\:7ef4\:8868\:521b\:5efa Markdown \:8868\:683c\:ff0c\:7b2c1\:884c\:4e3a\:6807\:9898\:884c
		\"Alignment\"\:53ef\:8bbe\:7f6e\:4e3a\:ff1a\"Left\",\"Right\",\"Center\"\:ff0c\:4e0d\:533a\:5206\:5927\:5c0f\:5199
		\"Escape\"\:53ef\:8bbe\:7f6e\:4e3a\:ff1aTrue, False
"

TableToText::usage =
		"\:5c06\:8868\:683c\:8f6c\:4e3a\:6587\:672c
		 \:5236\:8868\:7b26\:4e3a\:9ed8\:8ba4\:5206\:9694\:7b26"
		 
Begin["`Private`"]

(* #123456\:989c\:8272\:8f6cRGBColor *)
HexToMmaRGBColor[str_]:=Module[{str0=StringReplace[str,"#"->""],defaultColor=RGBColor[0,0,0]},
	If[StringLength[str0]==6,Return[RGBColor[FromDigits[#,16]/255.&/@StringPartition[str,2]]]];
	If[StringLength[str0]==3,Return[RGBColor[FromDigits[#,16]/255.&/@StringPartition[str,1]]]];
	Return[defaultColor]
	]
	
(*\:4ece\:4e00\:5f20\:4e8c\:7ef4\:8868\:521b\:5efa HTML \:8868\:683c\:ff0c\:7b2c1\:884c\:4e3a\:6807\:9898\:884c*)
TableToHTML[table_]:=StringRiffle[table[[1]],{"<table><thead><th>","</th><th>","</th></thead>"}]<>
StringRiffle[table[[2;;]],{"<tbody><tr>","</tr><tr>","</tr></tbody></table>"},{"<td>","</td><td>","</td>"}]

(*\:4ece\:4e00\:5f20\:4e8c\:7ef4\:8868\:521b\:5efa Markdown \:8868\:683c\:ff0c\:7b2c1\:884c\:4e3a\:6807\:9898\:884c*)
(*\:5b57\:7b26\:4e32\:957f\:5ea6\:ff1a\:534a\:89d2\:8ba11\:ff0c\:5168\:89d2\:8ba12*)
fStringLength[str_] := 
  Total[If[# > 255, 2, 1] & /@ ToCharacterCode[str]];
(*\:5b57\:7b26\:4e32\:8865\:9f50*)
fStringPad[str_, n_, alignment_ : "left", padding_ : " "] := Block[{
    align = ToLowerCase[alignment],
    padLen = n - fStringLength[str]
    },
   Switch[align,
    "left",
    str <> StringJoin[ConstantArray[padding, padLen]],
    "right",
    StringJoin[ConstantArray[padding, padLen]] <> str,
    "center",
    StringJoin[ConstantArray[padding, Floor[padLen/2.0]]] <> str <> 
     StringJoin[ConstantArray[padding, Ceiling[padLen/2.0]]],
    _,
    str <> StringJoin[ConstantArray[padding, padLen]]
    ]
   ];
TableToMarkdown[table_, 
   OptionsPattern[{"Alignment" -> "Left", "Escape" -> True}]] := 
  Block[{
    align = ToLowerCase[OptionValue["Alignment"]],
    tableEscape = 
     If[OptionValue["Escape"], 
      Map[StringReplace[
         ToString[#], {"&" -> "&#38;", "\"" -> "&#34;", 
          "|" -> "&#166;", "<" -> "&#60;", ">" -> "&#62;"}] &, 
       table, {2}], table],
    colCount = Length[table[[1]]],
    colMaxLen,
    paddingTable,
    alignString
    },
   colMaxLen = Max /@ Map[fStringLength, Transpose@tableEscape, {2}];
   colMaxLen = Max[#, 5] & /@ colMaxLen;
   
   alignString = 
    Switch[align, "left", ":---", "right", "---:", "center", 
     ":---:", _, ":---"];
   alignString = fStringPad[alignString, #, align] & /@ colMaxLen;
   
   paddingTable = Transpose@tableEscape;
   paddingTable = 
    Transpose@
     Table[fStringPad[#, colMaxLen[[k]], align] & /@ 
       paddingTable[[k]], {k, colCount}];
   paddingTable = {paddingTable[[1]]}~Join~{alignString}~Join~
     paddingTable[[2 ;;]];
   
   StringRiffle[paddingTable, "\n", {"| ", " | ", " |"}]
   ];

(*\:5c06\:4eceExcel\:4e2d\:590d\:5236\:51fa\:6765\:7684\:6587\:672c\:8f6c\:4e3aMma\:4e2d\:7684\:8868\:683c*)
TableToText[table_,OptionsPattern["separator"->"\t"]]:=StringRiffle[table,"\n",OptionValue["separator"]]

End[]
EndPackage[]
