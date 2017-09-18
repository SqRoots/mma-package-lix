(* ::Package:: *)

BeginPackage["lixWeb`"]

HexToMmaRGBColor::usage =
        "HexToMmaRGBColor[\"#333\"]\:ff0c#123456\:989c\:8272\:8f6cRGBColor
		 \:652f\:6301\:ff1a#333\:ff0c#333333\:ff0c333\:ff0c333333"
TableToHTML::usage =
		"\:4ece\:4e00\:5f20\:4e8c\:7ef4\:8868\:521b\:5efa HTML \:8868\:683c\:ff0c\:7b2c1\:884c\:4e3a\:6807\:9898\:884c"

TableToMarkdown::usage =
		"\:4ece\:4e00\:5f20\:4e8c\:7ef4\:8868\:521b\:5efa Markdown \:8868\:683c\:ff0c\:7b2c1\:884c\:4e3a\:6807\:9898\:884c"

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
TableToMarkdown[table_]:=Module[{t=Map[StringReplace[ToString[#],{"&"->"&#38;","\""->"&#34;","|"->"&#166;","<"->"&#60;",">"->"&#62;"}]&,table,{2}]},
	StringRiffle[t[[1]],{"| "," | "," |\n"}]<>
	StringRiffle[ConstantArray["---",Length[t[[1]]]],{"| "," | "," |\n"}]<>
	StringRiffle[t[[2;;]],"\n",{"| "," | "," |"}]
];

(*\:5c06\:4eceExcel\:4e2d\:590d\:5236\:51fa\:6765\:7684\:6587\:672c\:8f6c\:4e3aMma\:4e2d\:7684\:8868\:683c*)
TableToText[table_,OptionsPattern["separator"->"\t"]]:=StringRiffle[table,"\n",OptionValue["separator"]]

End[]
EndPackage[]
