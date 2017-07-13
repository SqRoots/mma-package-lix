(* ::Package:: *)

BeginPackage["lixDA`"]

RunsTest::usage = "RunsTest[da,\[Alpha]] \:5728\:663e\:8457\:6c34\:5e73\[Alpha]\:4e0b\:ff0c\:68c0\:9a8c\:5e8f\:5217da\:662f\:5426\:968f\:673a\:ff0c\:4ee5\:53ca\:7edf\:8ba1\:91cf\:5bf9\:5e94\:7684\:6982\:7387\:503c\:ff08\:592a\:5927\:6216\:592a\:5c0f\:90fd\:4e0d\:597d\:ff01\:ff09.";
GiniCoefficient::usage = "\:57fa\:5c3c\:7cfb\:6570\:3002
	\:8f93\:5165\:5217\:8868\:6216\:6570\:5b57\:ff0c\:6bcf\:4e2a\:5143\:7d20\:5fc5\:9700\:662f\:6570\:5b57\:ff0c\:591a\:7ef4\:5217\:8868\:4f1a\:88ab\:538b\:5e73\:ff0c\:6570\:5b57\:8fd4\:56de0\:3002
	\:53c2\:6570\:ff1ahttps://en.wikipedia.org/wiki/Gini_coefficient"
ROC::usage = "ROC[data,\"GreaterEqualQ\"\[Rule]True]\:ff0c
\:53c2\:6570\"GreaterEqualQ\"\:9ed8\:8ba4\:4e3a\:771f\:ff0c\:5373\:5305\:542b\:80af\:5b9a\:5206\:7c7b\:7684\:8fb9\:754c\:503c\:ff1b
\:8fd4\:56de\:ff1a
	ROC\:66f2\:7ebf\:5750\:6807\:70b9 \"ROCPoints\"\:ff0c
	ROC\:66f2\:7ebf\:56fe \"ROCCurve\"\:ff0c
	ROC\:66f2\:7ebf\:4e0b\:65b9\:9762\:79ef \"ROCAreaUnderCurve\"\:3002"



Begin["`Private`"]

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


GiniCoefficient[data_]:=If[ListQ[data],
 Block[{da=Sort[Flatten[data]],n},
  n=N[Length[da]];
  Return[(2.0Range[n].da/Total[da]-(n+1))/n]
 ],
 0
]

ROC[da_List,OptionsPattern[{"GreaterEqualQ"->True,"Title"->"\:526f\:6807\:9898"}]]:=Module[{
sda=Sort[da[[;;,1]],Greater],
splitPoints,
negativeSample=Sort[Select[da,#[[2]]==0&][[;;,1]]],
positiveSample=Sort[Select[da,#[[2]]==1&][[;;,1]]],
nsN,
psN,
xs,
ys,
ROCPoints,
ROCCurve,
ROCAreaUnderCurve
},
splitPoints={sda[[1]]+1}~Join~MovingAverage[sda,2]~Join~{sda[[-1]]-1};

nsN=Length[negativeSample]//N;
psN=Length[positiveSample]//N;

If[nsN psN==0,Return[<|"ROCPoints"->None,"ROCCurve"->None,"ROCAreaUnderCurve"->None|>]];

ROCPoints=If[OptionValue["GreaterEqualQ"]||OptionValue["GreaterEqualQ"]==1,
Table[{Count[negativeSample,x_/;x>=s]/nsN,Count[positiveSample,x_/;x>=s]/psN},{s,splitPoints}],Table[{Count[negativeSample,x_/;x>s]/nsN,Count[positiveSample,x_/;x>s]/psN},{s,splitPoints}]
];
ROCCurve=Graphics[{
{Darker[Green,0.3],AbsoluteThickness[1.7],Line[ROCPoints]},
{Darker[Green,0.6],AbsolutePointSize[5],Point[ROCPoints]},
{Dashing[{0.02,0.04-0.02}],GrayLevel[0.5],Line[{{0,0},{1,1}}]}
},
Frame->True,
FrameTicks->{{True,False},{True,False}},
FrameLabel->{Style["False Positive Rate",14,FontFamily->"Times New Roman"],Style["True Positive Rate",14,FontFamily->"Times New Roman"]},
PlotLabel->Column[{Style["ROC\:66f2\:7ebf",18,Bold,FontFamily->"\:5fae\:8f6f\:96c5\:9ed1"],Style[OptionValue["Title"],14,Bold,FontFamily->"\:5fae\:8f6f\:96c5\:9ed1"]},Alignment->Center],
Epilog->{
{White,EdgeForm[Black],Rectangle[{0.78,0.49},{1.01,0.55},RoundingRadius->0.01]},
{Darker[Green,0.3],AbsoluteThickness[2],Line[{{0.8,0.52},{0.9,0.52}}]},
{Darker[Green,0.6],AbsolutePointSize[5],Point[{0.85,0.52}]},
Inset[Style["ANN"],{0.96,0.52}]
},
ImageSize->500,
ImageMargins->10,
Background->White];
xs=ROCPoints[[;;,1]];
ys=ROCPoints[[;;,2]];
ROCAreaUnderCurve=1/2(Rest[xs].Most[ys]-Most[xs].Rest[ys]+xs[[-1]]ys[[-1]]-xs[[1]]ys[[1]]);

Return[<|
"ROCPoints"->ROCPoints,
"ROCCurve"->ROCCurve,
"ROCAreaUnderCurve"->ROCAreaUnderCurve
|>]
]

End[]

EndPackage[]
