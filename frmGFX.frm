VERSION 5.00
Begin VB.Form frmGFX 
   Caption         =   "Form1"
   ClientHeight    =   9885
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   9465
   LinkTopic       =   "Form1"
   ScaleHeight     =   9885
   ScaleWidth      =   9465
   StartUpPosition =   3  'Windows Default
   Begin VB.PictureBox picMTräd3 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1980
      Left            =   4680
      Picture         =   "frmGFX.frx":0000
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   122
      TabIndex        =   69
      Top             =   3240
      Width           =   1890
   End
   Begin VB.PictureBox picSTräd3 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1980
      Left            =   7080
      Picture         =   "frmGFX.frx":B842
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   122
      TabIndex        =   68
      Top             =   3960
      Width           =   1890
   End
   Begin VB.PictureBox picMTräd2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1995
      Left            =   4680
      Picture         =   "frmGFX.frx":17084
      ScaleHeight     =   129
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   120
      TabIndex        =   67
      Top             =   2760
      Width           =   1860
   End
   Begin VB.PictureBox picSTräd2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1995
      Left            =   5040
      Picture         =   "frmGFX.frx":2262E
      ScaleHeight     =   129
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   120
      TabIndex        =   66
      Top             =   2640
      Width           =   1860
   End
   Begin VB.PictureBox picMTräd1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1980
      Left            =   2880
      Picture         =   "frmGFX.frx":2DBD8
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   120
      TabIndex        =   65
      Top             =   3960
      Width           =   1860
   End
   Begin VB.PictureBox picSTräd1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1980
      Left            =   4920
      Picture         =   "frmGFX.frx":3901A
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   120
      TabIndex        =   64
      Top             =   4320
      Width           =   1860
   End
   Begin VB.PictureBox picMGran 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   2010
      Left            =   2760
      Picture         =   "frmGFX.frx":4445C
      ScaleHeight     =   130
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   125
      TabIndex        =   63
      Top             =   1920
      Width           =   1935
   End
   Begin VB.PictureBox picSGran 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   2010
      Left            =   2760
      Picture         =   "frmGFX.frx":5038E
      ScaleHeight     =   130
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   125
      TabIndex        =   62
      Top             =   1920
      Width           =   1935
   End
   Begin VB.PictureBox picMWell 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   6120
      Picture         =   "frmGFX.frx":5C2C0
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   64
      TabIndex        =   61
      Top             =   1920
      Width           =   1020
   End
   Begin VB.PictureBox picSWell 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   7560
      Picture         =   "frmGFX.frx":5F302
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   64
      TabIndex        =   60
      Top             =   2040
      Width           =   1020
   End
   Begin VB.PictureBox picMWater 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   570
      Left            =   6600
      Picture         =   "frmGFX.frx":62344
      ScaleHeight     =   34
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   33
      TabIndex        =   59
      Top             =   1920
      Width           =   555
   End
   Begin VB.PictureBox picSWater 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   570
      Left            =   7440
      Picture         =   "frmGFX.frx":630CE
      ScaleHeight     =   34
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   33
      TabIndex        =   58
      Top             =   1920
      Width           =   555
   End
   Begin VB.PictureBox picMTower2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1980
      Left            =   6240
      Picture         =   "frmGFX.frx":63E58
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   60
      TabIndex        =   57
      Top             =   0
      Width           =   960
   End
   Begin VB.PictureBox picSTower2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1980
      Left            =   7080
      Picture         =   "frmGFX.frx":6989A
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   60
      TabIndex        =   56
      Top             =   0
      Width           =   960
   End
   Begin VB.PictureBox picMTower1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1965
      Left            =   5520
      Picture         =   "frmGFX.frx":6F2DC
      ScaleHeight     =   127
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   96
      TabIndex        =   55
      Top             =   360
      Width           =   1500
   End
   Begin VB.PictureBox picSTower1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1965
      Left            =   6960
      Picture         =   "frmGFX.frx":781FE
      ScaleHeight     =   127
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   96
      TabIndex        =   54
      Top             =   480
      Width           =   1500
   End
   Begin VB.PictureBox picMStatue2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1455
      Left            =   7200
      Picture         =   "frmGFX.frx":81120
      ScaleHeight     =   93
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   80
      TabIndex        =   53
      Top             =   7200
      Width           =   1260
   End
   Begin VB.PictureBox picSStatue2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1455
      Left            =   8040
      Picture         =   "frmGFX.frx":86892
      ScaleHeight     =   93
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   80
      TabIndex        =   52
      Top             =   7200
      Width           =   1260
   End
   Begin VB.PictureBox picMStatue1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1530
      Left            =   5520
      Picture         =   "frmGFX.frx":8C004
      ScaleHeight     =   98
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   72
      TabIndex        =   51
      Top             =   7080
      Width           =   1140
   End
   Begin VB.PictureBox picSStatue1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1530
      Left            =   6360
      Picture         =   "frmGFX.frx":912F6
      ScaleHeight     =   98
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   72
      TabIndex        =   50
      Top             =   7080
      Width           =   1140
   End
   Begin VB.PictureBox picMRuin 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3960
      Left            =   480
      Picture         =   "frmGFX.frx":965E8
      ScaleHeight     =   260
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   194
      TabIndex        =   49
      Top             =   3000
      Width           =   2970
   End
   Begin VB.PictureBox picSRuin 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3960
      Left            =   840
      Picture         =   "frmGFX.frx":BB74A
      ScaleHeight     =   260
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   194
      TabIndex        =   48
      Top             =   2880
      Width           =   2970
   End
   Begin VB.PictureBox picMPortal 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1440
      Left            =   5520
      Picture         =   "frmGFX.frx":E08AC
      ScaleHeight     =   92
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   95
      TabIndex        =   47
      Top             =   5880
      Width           =   1485
   End
   Begin VB.PictureBox picSPortal 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1440
      Left            =   6960
      Picture         =   "frmGFX.frx":E706E
      ScaleHeight     =   92
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   95
      TabIndex        =   46
      Top             =   5760
      Width           =   1485
   End
   Begin VB.PictureBox picMHouse13 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3630
      Left            =   1560
      Picture         =   "frmGFX.frx":ED830
      ScaleHeight     =   238
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   253
      TabIndex        =   45
      Top             =   3720
      Width           =   3855
   End
   Begin VB.PictureBox picSHouse13 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3630
      Left            =   720
      Picture         =   "frmGFX.frx":119B02
      ScaleHeight     =   238
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   253
      TabIndex        =   44
      Top             =   3360
      Width           =   3855
   End
   Begin VB.PictureBox picMHouse12 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3915
      Left            =   1680
      Picture         =   "frmGFX.frx":145DD4
      ScaleHeight     =   257
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   192
      TabIndex        =   43
      Top             =   3840
      Width           =   2940
   End
   Begin VB.PictureBox picSHouse12 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3915
      Left            =   1560
      Picture         =   "frmGFX.frx":16A056
      ScaleHeight     =   257
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   192
      TabIndex        =   42
      Top             =   3600
      Width           =   2940
   End
   Begin VB.PictureBox picMHouse11 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   4005
      Left            =   2160
      Picture         =   "frmGFX.frx":18E2D8
      ScaleHeight     =   263
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   195
      TabIndex        =   41
      Top             =   3600
      Width           =   2985
   End
   Begin VB.PictureBox picSHouse11 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   4005
      Left            =   720
      Picture         =   "frmGFX.frx":1B3F2E
      ScaleHeight     =   263
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   195
      TabIndex        =   40
      Top             =   2880
      Width           =   2985
   End
   Begin VB.PictureBox picMHouse10 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   4005
      Left            =   2280
      Picture         =   "frmGFX.frx":1D9B84
      ScaleHeight     =   263
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   193
      TabIndex        =   39
      Top             =   4200
      Width           =   2955
   End
   Begin VB.PictureBox picSHouse10 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   4005
      Left            =   1440
      Picture         =   "frmGFX.frx":1FEFA2
      ScaleHeight     =   263
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   193
      TabIndex        =   38
      Top             =   3480
      Width           =   2955
   End
   Begin VB.PictureBox picMHouse9 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3435
      Left            =   0
      Picture         =   "frmGFX.frx":2243C0
      ScaleHeight     =   225
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   192
      TabIndex        =   37
      Top             =   3120
      Width           =   2940
   End
   Begin VB.PictureBox picSHouse9 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3435
      Left            =   0
      Picture         =   "frmGFX.frx":243E42
      ScaleHeight     =   225
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   192
      TabIndex        =   36
      Top             =   3000
      Width           =   2940
   End
   Begin VB.PictureBox picMHouse8 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3930
      Left            =   720
      Picture         =   "frmGFX.frx":2638C4
      ScaleHeight     =   258
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   256
      TabIndex        =   35
      Top             =   4080
      Width           =   3900
   End
   Begin VB.PictureBox picSHouse8 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3930
      Left            =   480
      Picture         =   "frmGFX.frx":293F06
      ScaleHeight     =   258
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   256
      TabIndex        =   34
      Top             =   3360
      Width           =   3900
   End
   Begin VB.PictureBox picMHouse7 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3840
      Left            =   240
      Picture         =   "frmGFX.frx":2C4548
      ScaleHeight     =   252
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   252
      TabIndex        =   33
      Top             =   3840
      Width           =   3840
   End
   Begin VB.PictureBox picSHouse7 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3840
      Left            =   480
      Picture         =   "frmGFX.frx":2F2DBA
      ScaleHeight     =   252
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   252
      TabIndex        =   32
      Top             =   3600
      Width           =   3840
   End
   Begin VB.PictureBox picMHouse6 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3165
      Left            =   0
      Picture         =   "frmGFX.frx":32162C
      ScaleHeight     =   207
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   190
      TabIndex        =   31
      Top             =   4920
      Width           =   2910
   End
   Begin VB.PictureBox picSHouse6 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3165
      Left            =   120
      Picture         =   "frmGFX.frx":33E4F2
      ScaleHeight     =   207
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   190
      TabIndex        =   30
      Top             =   4920
      Width           =   2910
   End
   Begin VB.PictureBox picMHouse5 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3375
      Left            =   240
      Picture         =   "frmGFX.frx":35B3B8
      ScaleHeight     =   221
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   186
      TabIndex        =   29
      Top             =   5040
      Width           =   2850
   End
   Begin VB.PictureBox picSHouse5 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3375
      Left            =   120
      Picture         =   "frmGFX.frx":37976A
      ScaleHeight     =   221
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   186
      TabIndex        =   28
      Top             =   5040
      Width           =   2850
   End
   Begin VB.PictureBox picMHouse4 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3465
      Left            =   0
      Picture         =   "frmGFX.frx":397B1C
      ScaleHeight     =   227
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   154
      TabIndex        =   27
      Top             =   6120
      Width           =   2370
   End
   Begin VB.PictureBox picSHouse4 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3465
      Left            =   0
      Picture         =   "frmGFX.frx":3B16CE
      ScaleHeight     =   227
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   154
      TabIndex        =   26
      Top             =   6240
      Width           =   2370
   End
   Begin VB.PictureBox picMHouse3 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3360
      Left            =   3720
      Picture         =   "frmGFX.frx":3CB280
      ScaleHeight     =   220
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   254
      TabIndex        =   25
      Top             =   2640
      Width           =   3870
   End
   Begin VB.PictureBox picSHouse3 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3360
      Left            =   4560
      Picture         =   "frmGFX.frx":3F4352
      ScaleHeight     =   220
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   254
      TabIndex        =   24
      Top             =   2640
      Width           =   3870
   End
   Begin VB.PictureBox picMHouse2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3060
      Left            =   0
      Picture         =   "frmGFX.frx":41D424
      ScaleHeight     =   200
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   162
      TabIndex        =   23
      Top             =   3120
      Width           =   2490
   End
   Begin VB.PictureBox picSHouse2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3060
      Left            =   0
      Picture         =   "frmGFX.frx":4351A6
      ScaleHeight     =   200
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   162
      TabIndex        =   22
      Top             =   3120
      Width           =   2490
   End
   Begin VB.PictureBox picMHouse1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3285
      Left            =   0
      Picture         =   "frmGFX.frx":44CF28
      ScaleHeight     =   215
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   224
      TabIndex        =   21
      Top             =   3120
      Width           =   3420
   End
   Begin VB.PictureBox picSHouse1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3285
      Left            =   0
      Picture         =   "frmGFX.frx":4703CA
      ScaleHeight     =   215
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   224
      TabIndex        =   20
      Top             =   3120
      Width           =   3420
   End
   Begin VB.PictureBox picMGateO 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1470
      Left            =   4200
      Picture         =   "frmGFX.frx":49386C
      ScaleHeight     =   94
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   96
      TabIndex        =   19
      Top             =   1080
      Width           =   1500
   End
   Begin VB.PictureBox picSGateO 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1470
      Left            =   4200
      Picture         =   "frmGFX.frx":49A26E
      ScaleHeight     =   94
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   96
      TabIndex        =   18
      Top             =   1080
      Width           =   1500
   End
   Begin VB.PictureBox picMGateC 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1470
      Left            =   2640
      Picture         =   "frmGFX.frx":4A0C70
      ScaleHeight     =   94
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   96
      TabIndex        =   17
      Top             =   1080
      Width           =   1500
   End
   Begin VB.PictureBox picSGateC 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1470
      Left            =   2640
      Picture         =   "frmGFX.frx":4A7672
      ScaleHeight     =   94
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   96
      TabIndex        =   16
      Top             =   1080
      Width           =   1500
   End
   Begin VB.PictureBox picMDebree 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1455
      Left            =   4440
      Picture         =   "frmGFX.frx":4AE074
      ScaleHeight     =   93
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   103
      TabIndex        =   15
      Top             =   0
      Width           =   1605
   End
   Begin VB.PictureBox picSDebree 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1455
      Left            =   4440
      Picture         =   "frmGFX.frx":4B520E
      ScaleHeight     =   93
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   103
      TabIndex        =   14
      Top             =   0
      Width           =   1605
   End
   Begin VB.PictureBox picMCrate2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1050
      Left            =   2760
      Picture         =   "frmGFX.frx":4BC3A8
      ScaleHeight     =   66
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   54
      TabIndex        =   13
      Top             =   0
      Width           =   870
   End
   Begin VB.PictureBox picSCrate2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1050
      Left            =   3600
      Picture         =   "frmGFX.frx":4BEE32
      ScaleHeight     =   66
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   54
      TabIndex        =   12
      Top             =   0
      Width           =   870
   End
   Begin VB.PictureBox picMCrate1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   1680
      Picture         =   "frmGFX.frx":4C18BC
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   65
      TabIndex        =   11
      Top             =   600
      Width           =   1035
   End
   Begin VB.PictureBox picSCrate1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   1680
      Picture         =   "frmGFX.frx":4C49FE
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   65
      TabIndex        =   10
      Top             =   600
      Width           =   1035
   End
   Begin VB.PictureBox picMBucket 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   570
      Left            =   1680
      Picture         =   "frmGFX.frx":4C7B40
      ScaleHeight     =   34
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   31
      TabIndex        =   9
      Top             =   0
      Width           =   525
   End
   Begin VB.PictureBox picSBucket 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   570
      Left            =   2160
      Picture         =   "frmGFX.frx":4C8842
      ScaleHeight     =   34
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   31
      TabIndex        =   8
      Top             =   0
      Width           =   525
   End
   Begin VB.PictureBox picMBench2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1050
      Left            =   960
      Picture         =   "frmGFX.frx":4C9544
      ScaleHeight     =   66
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   33
      TabIndex        =   7
      Top             =   2040
      Width           =   555
   End
   Begin VB.PictureBox picSBench2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1050
      Left            =   1440
      Picture         =   "frmGFX.frx":4CAF4E
      ScaleHeight     =   66
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   33
      TabIndex        =   6
      Top             =   2040
      Width           =   555
   End
   Begin VB.PictureBox picMBench1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   0
      Picture         =   "frmGFX.frx":4CC958
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   30
      TabIndex        =   5
      Top             =   2040
      Width           =   510
   End
   Begin VB.PictureBox picSBench1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   480
      Picture         =   "frmGFX.frx":4CE09A
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   30
      TabIndex        =   4
      Top             =   2040
      Width           =   510
   End
   Begin VB.PictureBox picMbarrel2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   0
      Picture         =   "frmGFX.frx":4CF7DC
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   50
      TabIndex        =   3
      Top             =   1080
      Width           =   810
   End
   Begin VB.PictureBox picSbarrel2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   840
      Picture         =   "frmGFX.frx":4D1E1E
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   50
      TabIndex        =   2
      Top             =   1080
      Width           =   810
   End
   Begin VB.PictureBox picSbarrel1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1110
      Left            =   840
      Picture         =   "frmGFX.frx":4D4460
      ScaleHeight     =   70
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   56
      TabIndex        =   1
      Top             =   0
      Width           =   900
   End
   Begin VB.PictureBox picMbarrel1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1110
      Left            =   0
      Picture         =   "frmGFX.frx":4D7292
      ScaleHeight     =   70
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   56
      TabIndex        =   0
      Top             =   0
      Width           =   900
   End
End
Attribute VB_Name = "frmGFX"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
