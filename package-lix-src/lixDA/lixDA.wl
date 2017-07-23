(* ::Package:: *)

BeginPackage["lixDA`"]

RunsTest::usage = "RunsTest[da,\[Alpha]] \:5728\:663e\:8457\:6c34\:5e73\[Alpha]\:4e0b\:ff0c\:68c0\:9a8c\:5e8f\:5217da\:662f\:5426\:968f\:673a\:ff0c\:4ee5\:53ca\:7edf\:8ba1\:91cf\:5bf9\:5e94\:7684\:6982\:7387\:503c\:ff08\:592a\:5927\:6216\:592a\:5c0f\:90fd\:4e0d\:597d\:ff01\:ff09.";

GiniCoefficient::usage = "\:57fa\:5c3c\:7cfb\:6570\:3002
	\:8f93\:5165\:5217\:8868\:6216\:6570\:5b57\:ff0c\:6bcf\:4e2a\:5143\:7d20\:5fc5\:9700\:662f\:6570\:5b57\:ff0c\:591a\:7ef4\:5217\:8868\:4f1a\:88ab\:538b\:5e73\:ff0c\:6570\:5b57\:8fd4\:56de0\:3002
	\:53c2\:6570\:ff1ahttps://en.wikipedia.org/wiki/Gini_coefficient"

ROCS::usage=""

ROCData::usage="ROC\:57fa\:672c\:6570\:636e
ROCData[data,,\"ConfidenceLevel\"->0.975]
\:5176\:4e2ddata\:ff0c\:4e3an\:884c2\:5217\:6570\:636e\:ff0c\:6700\:540e1\:5217\:4e3a\:9634\:9633\:6807\:5fd7\:ff0c1\:8868\:9633\:ff0c0\:8868\:9634
\:53c2\:8003\:6587\:732e\:300a\:533b\:5b66\:7edf\:8ba1\:5b66-[\:7b2c2\:7248]-[8\:5e74\:5236\:7528\:4e66]-[\:989c\:8679\:4e3b\:7f16]\:300b
\:5206\:5e03\:5047\:5b9a\:ff1a\:975e\:53c2\:6570
\:53c2\:6570\:ff1a
	\:7f6e\:4fe1\:5ea6\:7ea7\:522b\:ff1a\"ConfidenceLevel\"\:ff0c\:9ed8\:8ba4\:4e3a0.975\:ff081-0.05/2\:ff09\:ff0c
\:8fd4\:56de\:503c\:ff1a
	ROC\:66f2\:7ebf\:5750\:6807\:70b9\:ff1a\"ROCPoints\"\:ff0c
	ROC\:66f2\:7ebf\:4e0b\:65b9\:9762\:79ef\:ff1a\"AUC\",
	ROC\:66f2\:7ebf\:4e0b\:65b9\:9762\:79ef\:7684\:7f6e\:4fe1\:533a\:95f4\:ff1a\"Interval\"
	\:9762\:79ef\:7684\:6807\:51c6\:8befSE(A)\:ff1a\"SEA\"
	\:9633\:6027/\:9634\:6027\:6837\:672c\:ff1a\"positiveSample\"\:ff0c\"negativeSample\"
	\:9633\:6027/\:9634\:6027\:6837\:672c\:91cf\:ff1a\"nPositive\"\:ff0c\"nNegative\"
	\:663e\:8457\:6027P\:503c\:ff08\:4e0e\:9762\:79ef0.5\:76f8\:6bd4\:ff09\:ff1a\"p0.5\"
	"

ROCCurve::usage="ROC\:66f2\:7ebf
ROCCurve[data,OptionsPattern[{\"SubTitle\"\[Rule]\"\:526f\:6807\:9898\"}]]
\:5176\:4e2ddata\:ff0c\:4e3an\:884c2\:5217\:6570\:636e\:ff0c\:6700\:540e1\:5217\:4e3a\:9634\:9633\:6807\:5fd7\:ff0c1\:8868\:9633\:ff0c0\:8868\:9634"

ROCGroupCompare::usage="\:6210\:7ec4\:6bd4\:8f83\:663e\:8457\:6c34\:5e73\:ff1a\:4e24\:79cd\:8bca\:65ad\:65b9\:5f0f\:4f7f\:7528\:4e0d\:540c\:6837\:672c
ROCGroupCompare[data1,data2]
\:5176\:4e2ddata\:ff0c\:4e3an\:884c2\:5217\:6570\:636e\:ff0c\:6700\:540e1\:5217\:4e3a\:9634\:9633\:6807\:5fd7\:ff0c1\:8868\:9633\:ff0c0\:8868\:9634"

ROCPairCompare::usage="\:6709\:95ee\:9898\:ff01\:914d\:5bf9\:6bd4\:8f83\:663e\:8457\:6c34\:5e73\:ff1a\:4e24\:79cd\:8bca\:65ad\:65b9\:5f0f\:4f7f\:7528\:76f8\:540c\:6837\:672c
ROCPairCompare[data1,data2]
\:5176\:4e2ddata\:ff0c\:4e3an\:884c2\:5217\:6570\:636e\:ff0c\:6700\:540e1\:5217\:4e3a\:9634\:9633\:6807\:5fd7\:ff0c1\:8868\:9633\:ff0c0\:8868\:9634"



Begin["`Private`"]
(*============= RunsTest =============*)
RunsTest=Function[{da,\[Alpha]},
n1=Count[da,1];
n2=Count[da,0];
r=Total[Abs[Differences[da]]]+1;
rb=(2 n1 n2)/(n1+n2)+1;
s2r=(2 n1 n2 (2 n1 n2-n1-n2))/((n1+n2)^2 (n1+n2-1));
z=(r-rb)/Sqrt[s2r];
If[n1>10 && n2>10,
{If[Abs[z]>InverseCDF[NormalDistribution[0,1],1-\[Alpha]/2],Style["\:5e8f\:5217\:4e0d\:968f\:673a",Red],Style["\:5e8f\:5217\:968f\:673a",Blue]],"\:ff08\:6570\:636e\:91cf,\:6e38\:7a0b\:6570\:ff09\:ff1a"<>"\:ff08"<>ToString[Length[da]]<>","<>ToString[r]<>"\:ff09","\:7edf\:8ba1\:91cf\:ff1a"<>ToString[N[z]],"\:6982\:7387\:ff1a"<>ToString[CDF[NormalDistribution[0,1],z]//N]},
Style["n1\:6216n2\:5c0f\:4e8e10\:ff0c\:4e0d\:6ee1\:8db3\:6e38\:7a0b\:6a21\:578b\:ff01",Red,Bold]]
];


(*============= GiniCoefficient =============*)
GiniCoefficient[data_]:=If[ListQ[data],
 Block[{da=Sort[Flatten[data]],n},
  n=N[Length[da]];
  Return[(2.0Range[n].da/Total[da]-(n+1))/n]
 ],
 0
]

(*============= ROC =============*)
ROCS[yp_,yn_]:=(Sign[yp-yn]+1.0)/2;
(*ROC\:57fa\:672c\:6570\:636e*)
ROCData[da_,OptionsPattern[{"ConfidenceLevel"->0.975}]]:=Block[{
sDa=Reverse[Union[da[[;;,1]]]],
positiveSample=Sort[Select[da,#[[-1]]==1&][[;;,1]]],
negativeSample=Sort[Select[da,#[[-1]]==0&][[;;,1]]],
splitPoints,nPositive,nNegative,rocPoints,r1,r2,r3,r4,t1,t2,t3,a,q1,q2,SEA,zaSEA
},
nPositive=Length[positiveSample]//N;
nNegative=Length[negativeSample]//N;
splitPoints={sDa[[1]]+1}~Join~MovingAverage[sDa,2]~Join~{sDa[[-1]]-1};
rocPoints=Table[{Count[negativeSample,x_/;x>s]/nNegative,Count[positiveSample,x_/;x>s]/nPositive},{s,splitPoints}];
r1=BinCounts[positiveSample,{splitPoints}];
r2=BinCounts[negativeSample,{splitPoints}];
r3=nPositive-Accumulate[r1];
r4=nNegative-Reverse[Accumulate[Reverse[r2]]];
t1=r2.r3+r1.r2/2//N;
t2=r1.(r4^2+r4 r2+r2^2/3)//N;
t3=r2.(r3^2+r3 r1+r1^2/3)//N;
a=t1/nPositive/nNegative;
q1=t3/nPositive^2/nNegative;
q2=t2/nPositive/nNegative^2;
SEA=Sqrt[(a(1-a)+(nPositive-1)(q1-a^2)+(nNegative-1)(q2-a^2))/nPositive/nNegative];
zaSEA=InverseCDF[NormalDistribution[0,1],OptionValue["ConfidenceLevel"]]SEA;
Return[<|
"nPositive"->nPositive,
"nNegative"->nNegative,
"positiveSample"->positiveSample,
"negativeSample"->negativeSample,
"ROCPoints"->rocPoints,
"AUC"->a,
"SEA"->SEA,
"Interval"->{a-zaSEA,a+zaSEA},
"p0.5"->1-CDF[NormalDistribution[],(a-0.5)/SEA]
|>]
];
(*ROC\:66f2\:7ebf*)
ROCCurve[da_,OptionsPattern[{"SubTitle"->"\:526f\:6807\:9898"}]]:=Block[{rocPoints=da["ROCPoints"]},
Graphics[{{Darker[Green,0.3],AbsoluteThickness[1.7],Line[rocPoints]},{Darker[Green,0.6],AbsolutePointSize[5],Point[rocPoints]},{Dashing[{0.02,0.04-0.02}],GrayLevel[0.5],Line[{{0,0},{1,1}}]}},Frame->True,FrameTicks->{{True,False},{True,False}},FrameLabel->{Style["False Positive Rate",14,FontFamily->"Times New Roman"],Style["True Positive Rate",14,FontFamily->"Times New Roman"]},PlotLabel->Column[{Style["ROC\:66f2\:7ebf",18,Bold,FontFamily->"\:5fae\:8f6f\:96c5\:9ed1"],Style[OptionValue["SubTitle"],14,Bold,FontFamily->"\:5fae\:8f6f\:96c5\:9ed1"]},Alignment->Center],Epilog->{{White,EdgeForm[Black],Rectangle[{0.78,0.49},{1.01,0.55},RoundingRadius->0.01]},{Darker[Green,0.3],AbsoluteThickness[2],Line[{{0.8,0.52},{0.9,0.52}}]},{Darker[Green,0.6],AbsolutePointSize[5],Point[{0.85,0.52}]},Inset[Style["ANN"],{0.96,0.52}]},ImageSize->500,ImageMargins->10,Background->White]
];
(*\:6210\:7ec4\:6bd4\:8f83\:663e\:8457\:6c34\:5e73\:ff1a\:4e24\:79cd\:8bca\:65ad\:65b9\:5f0f\:4f7f\:7528\:4e0d\:540c\:6837\:672c*)
ROCGroupCompare[da1_,da2_]:=Block[{
rocData1=ROCData[da1],
rocData2=ROCData[da2]
},
Return[1-CDF[Abs[rocData1["AUC"]-rocData2["AUC"]]/Sqrt[rocData1["SEA"]^2+rocData1["SEA"]^2]]]
];
(*\:914d\:5bf9\:6bd4\:8f83\:663e\:8457\:6c34\:5e73\:ff1a\:4e24\:79cd\:8bca\:65ad\:65b9\:5f0f\:4f7f\:7528\:76f8\:540c\:6837\:672c*)
ROCPairCompare[da1_,da2_]:=Block[{
rocData1=ROCData[da1],
rocData2=ROCData[da2],
nPositive,nNegative,
positiveSample1,negativeSample1,positiveSample2,negativeSample2,
a1,a2,
STP,STN,
covA1A2
},
nPositive=rocData1["nPositive"];
nNegative=rocData1["nNegative"];
positiveSample1=rocData1["positiveSample"];
negativeSample1=rocData1["negativeSample"];
positiveSample2=rocData2["positiveSample"];
negativeSample2=rocData2["negativeSample"];
a1=rocData1["AUC"];
a2=rocData2["AUC"];
STP=Sum[(ROCS[positiveSample1[[i]],negativeSample1[[j]]]/nNegative-a1)(ROCS[positiveSample2[[i]],negativeSample2[[j]]]/nNegative-a2),{i,nPositive},{j,nNegative}]/(nPositive-1);
STN=Sum[(ROCS[positiveSample1[[j]],negativeSample1[[i]]]/nPositive-a1)(ROCS[positiveSample2[[j]],negativeSample2[[i]]]/nPositive-a2),{i,nNegative},{j,nPositive}]/(nNegative-1);
covA1A2=STP/nPositive+STN/nNegative;
Print[Abs[a1-a2]/Sqrt[rocData1["SEA"]^2+rocData2["SEA"]^2-2covA1A2]];
Return[1-CDF[NormalDistribution[],Abs[a1-a2]/Sqrt[rocData1["SEA"]^2+rocData2["SEA"]^2-2covA1A2]]]
];

End[]

EndPackage[]
