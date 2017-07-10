(* ::Package:: *)

BeginPackage["lixDB`",{"DatabaseLink`","JLink`"}]
ConnectToOracle::usage="ConnectToOracle[userName,password,host,port,db,jarPath]"

ConnectToMySQL::usage="ConnectToOracle[userName,password,host,port,db]"

Begin["`Private`"]
currentPath="E:/GitHub/mma-package-lix/package-lix/lixDB/";
DBinfo=<||>;
driverPath=FileNameJoin[{currentPath,"drivers"}];
config=Get[FileNameJoin[{currentPath,"lixDB-config.txt"}]];

ConnectToOracle[
	userName_:DBinfo["oracle"]["userName"],
	password_:DBinfo["oracle"]["password"],
	host_:DBinfo["oracle"]["host"],
	port_:DBinfo["oracle"]["port"],
	sid_:DBinfo["oracle"]["sid"],
	jarPath_:currentPath]:=Module[{connectString="jdbc:oracle:thin:@"<>host<>":"<>port<>":"<>sid<>""},
AddToClassPath[jarPath];(*\:6307\:5b9a\:9a71\:52a8\[OpenCurlyDoubleQuote]jdbc-oracle.jar\[CloseCurlyDoubleQuote]\:7684\:6240\:5728\:76ee\:5f55*)
Return[OpenSQLConnection[JDBC["oracle.jdbc.driver.OracleDriver",connectString],"Username"->userName,"Password"->password]]
]

ConnectToMySQL[
	userName_:DBinfo["mysql"]["userName"],
	password_:DBinfo["mysql"]["password"],
	host_:DBinfo["mysql"]["host"],
	port_:DBinfo["mysql"]["port"],
	db_:DBinfo["mysql"]["db"]
	]:=Module[{connectString=host<>":"<>port<>"/"<>db},

Return[OpenSQLConnection[
	JDBC["MySQL(Connector/J)",connectString],
	"Name"->"lixDB_mysql",
	"Catalog"->Automatic,
	"Description"->"lixDB",
	"Username"->userName,
	"Password"->password,
	"Properties"->{},
	"ReadOnly"->Automatic,
	"RelativePath"->False,
	"TransactionIsolationLevel"->Automatic,
	"UseConnectionPool"->Automatic]
	]
]
End[]
EndPackage[]
