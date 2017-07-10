# mma-package-lix
> 自己编写的Mma工具包，目前尚不成熟，请慎用！

## 使用方法
1. 下载并解压：
2. 修改数据库配制文件“./package-lix-src/lixDB/lixDB-config-o.txt”，并保存为“lixDB-config.txt”
3. 以管理员身份打开 Mathematica 前端笔记本；（一般不需要使用管理员身份打开）
4. 打开“**initialization.nb**”，并运行之。

## 备份
备份是自动地，运行“initialization.nb”之后，原 init.m 文件将被备份为“init-backup-xxx.m”。

## 恢复
与**使用方法**类似，运行“**recovery.nb**”，“init.m”将恢复到上次状态。

**注意**：使用“recovery.nb”之后，将使用最后一份“init-backup-xxx.m”覆盖当前“init.m”，而当前“init.m”将不可以找回！

## 目录说明
```
├─init（初始化文件）
├─package-lix-dist（包--init.m中指定要加载的包--不上传）
│  ├─lixDA
│  ├─lixDB
│  │  └─drivers
│  └─lixWeb
└─package-lix-src（包--源文件，用于开发）
    ├─lixDA
    ├─lixDB
    │  ├─drivers
    │  └─lixDB-config.txt(数据库配置文件--不上传)
    └─lixWeb
```

## 功能列表

### 数据库
| 函数名称 | 说明 |
| --- | --- |
| ConnectToOracle | 连接到Oracle，含jar包 |
| ConnectToMySQL | 连接到MySQL |

### 数据分析与机器学习
| 函数名称 | 说明 |
| --- | --- |
| RunsTest | 游程检验（随机序列测试） |
| GiniCoefficient | 基尼系数 |
| ROC | ROC曲线：坐标，图像，AUC（曲线下方的面积） |

### Web: HTML, JSON...
| 函数名称 | 说明 |
| --- | --- |
| HexToMmaRGBColor | &#34;#123456&#34;或&#34;#123&#34;转为RGBColor[x,x,x] |
| TableToHTML | 表转HTML |
| TableToMarkdown | 表转Markdown |
| TableToText | 表转Text，默认等价于StringRiffe[table, "\n", "\t"] |
