VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form frmMapEditor 
   BackColor       =   &H00000000&
   Caption         =   "Ultimate Chaos Map Editor - Untitled.map"
   ClientHeight    =   12270
   ClientLeft      =   165
   ClientTop       =   450
   ClientWidth     =   17160
   LinkTopic       =   "Form1"
   ScaleHeight     =   12270
   ScaleWidth      =   17160
   StartUpPosition =   2  'CenterScreen
   WindowState     =   2  'Maximized
   Begin VB.PictureBox picMap 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      Height          =   11295
      Left            =   0
      ScaleHeight     =   749
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   1156
      TabIndex        =   96
      Top             =   1800
      Width           =   17400
      Begin VB.Frame frmPaint 
         BackColor       =   &H8000000D&
         Caption         =   "Paint Grounds"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000E&
         Height          =   4575
         Left            =   15480
         TabIndex        =   110
         Top             =   0
         Width           =   1695
         Visible         =   0   'False
         Begin VB.Image imgGrass1 
            Height          =   645
            Left            =   120
            Picture         =   "frmMapEditor.frx":0000
            Stretch         =   -1  'True
            Top             =   240
            Width           =   645
         End
         Begin VB.Image imgGrass2 
            Height          =   645
            Left            =   960
            Picture         =   "frmMapEditor.frx":35A6
            Stretch         =   -1  'True
            Top             =   240
            Width           =   645
         End
         Begin VB.Image imgSand1 
            Height          =   645
            Left            =   120
            Picture         =   "frmMapEditor.frx":6B4C
            Stretch         =   -1  'True
            Top             =   960
            Width           =   645
         End
         Begin VB.Image imgSand2 
            Height          =   645
            Left            =   960
            Picture         =   "frmMapEditor.frx":A0F2
            Stretch         =   -1  'True
            Top             =   960
            Width           =   645
         End
         Begin VB.Image imgSnow 
            Height          =   645
            Left            =   120
            Picture         =   "frmMapEditor.frx":D698
            Stretch         =   -1  'True
            Top             =   1680
            Width           =   645
         End
         Begin VB.Image imgStone1 
            Height          =   645
            Left            =   960
            Picture         =   "frmMapEditor.frx":10C3E
            Stretch         =   -1  'True
            Top             =   1680
            Width           =   645
         End
         Begin VB.Image imgStone2 
            Height          =   645
            Left            =   120
            Picture         =   "frmMapEditor.frx":141E4
            Stretch         =   -1  'True
            Top             =   2400
            Width           =   645
         End
         Begin VB.Image imgStone3 
            Height          =   645
            Left            =   960
            Picture         =   "frmMapEditor.frx":1778A
            Stretch         =   -1  'True
            Top             =   2400
            Width           =   645
         End
         Begin VB.Image imgStone4 
            Height          =   645
            Left            =   120
            Picture         =   "frmMapEditor.frx":1AD30
            Stretch         =   -1  'True
            Top             =   3120
            Width           =   645
         End
         Begin VB.Image imgWater1 
            Height          =   645
            Left            =   960
            Picture         =   "frmMapEditor.frx":1E2D6
            Stretch         =   -1  'True
            Top             =   3120
            Width           =   645
         End
         Begin VB.Image imgWater2 
            Height          =   645
            Left            =   120
            Picture         =   "frmMapEditor.frx":2187C
            Stretch         =   -1  'True
            Top             =   3840
            Width           =   645
         End
      End
      Begin VB.Frame frmMisc 
         BackColor       =   &H8000000D&
         Caption         =   "Plants and Misc"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000E&
         Height          =   5295
         Left            =   14160
         TabIndex        =   109
         Top             =   0
         Width           =   3135
         Visible         =   0   'False
         Begin VB.Image Image11 
            Height          =   1215
            Left            =   2520
            Picture         =   "frmMapEditor.frx":24E22
            Stretch         =   -1  'True
            Top             =   3720
            Width           =   495
         End
         Begin VB.Image Image9 
            Height          =   735
            Left            =   1080
            Picture         =   "frmMapEditor.frx":27204
            Stretch         =   -1  'True
            Top             =   4440
            Width           =   735
         End
         Begin VB.Image Image3 
            Height          =   615
            Left            =   1080
            Picture         =   "frmMapEditor.frx":2A47A
            Stretch         =   -1  'True
            Top             =   3720
            Width           =   735
         End
         Begin VB.Image Image8 
            Height          =   975
            Left            =   2040
            Picture         =   "frmMapEditor.frx":2D4BC
            Stretch         =   -1  'True
            Top             =   2520
            Width           =   855
         End
         Begin VB.Image Image7 
            Height          =   495
            Left            =   1920
            Picture         =   "frmMapEditor.frx":363DE
            Stretch         =   -1  'True
            Top             =   3720
            Width           =   495
         End
         Begin VB.Image Image6 
            Height          =   975
            Left            =   120
            Picture         =   "frmMapEditor.frx":37168
            Stretch         =   -1  'True
            Top             =   3600
            Width           =   735
         End
         Begin VB.Image Image5 
            Height          =   975
            Left            =   1080
            Picture         =   "frmMapEditor.frx":3CBAA
            Stretch         =   -1  'True
            Top             =   2520
            Width           =   855
         End
         Begin VB.Image Image4 
            Height          =   975
            Left            =   120
            Picture         =   "frmMapEditor.frx":4231C
            Stretch         =   -1  'True
            Top             =   2520
            Width           =   855
         End
         Begin VB.Image Image2 
            Height          =   975
            Left            =   2160
            Picture         =   "frmMapEditor.frx":4760E
            Stretch         =   -1  'True
            Top             =   1320
            Width           =   735
         End
         Begin VB.Image Image1 
            Height          =   975
            Left            =   1080
            Picture         =   "frmMapEditor.frx":6C770
            Stretch         =   -1  'True
            Top             =   1320
            Width           =   855
         End
         Begin VB.Image imgTräd3 
            Height          =   975
            Left            =   2160
            Picture         =   "frmMapEditor.frx":72F32
            Stretch         =   -1  'True
            Top             =   240
            Width           =   855
         End
         Begin VB.Image imgTräd2 
            Height          =   1095
            Left            =   120
            Picture         =   "frmMapEditor.frx":7E774
            Stretch         =   -1  'True
            Top             =   1320
            Width           =   855
         End
         Begin VB.Image imgTräd1 
            Height          =   975
            Left            =   1200
            Picture         =   "frmMapEditor.frx":89D1E
            Stretch         =   -1  'True
            Top             =   240
            Width           =   735
         End
         Begin VB.Image imgGran 
            Height          =   975
            Left            =   120
            Picture         =   "frmMapEditor.frx":95160
            Stretch         =   -1  'True
            Top             =   240
            Width           =   855
         End
      End
      Begin VB.Frame frmMore 
         BackColor       =   &H8000000D&
         Caption         =   "Ground/Houses"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000E&
         Height          =   5295
         Left            =   14280
         TabIndex        =   97
         Top             =   0
         Width           =   3015
         Visible         =   0   'False
         Begin VB.PictureBox picWater2 
            AutoSize        =   -1  'True
            DragIcon        =   "frmMapEditor.frx":A1092
            DragMode        =   1  'Automatic
            Height          =   780
            Left            =   960
            Picture         =   "frmMapEditor.frx":A14D4
            ScaleHeight     =   48
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   48
            TabIndex        =   108
            Top             =   3600
            Width           =   780
         End
         Begin VB.PictureBox picWater 
            AutoSize        =   -1  'True
            DragIcon        =   "frmMapEditor.frx":A3016
            DragMode        =   1  'Automatic
            Height          =   780
            Left            =   960
            Picture         =   "frmMapEditor.frx":A3458
            ScaleHeight     =   48
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   48
            TabIndex        =   107
            Top             =   2760
            Width           =   780
         End
         Begin VB.PictureBox picStone4 
            AutoSize        =   -1  'True
            DragIcon        =   "frmMapEditor.frx":A4F9A
            DragMode        =   1  'Automatic
            Height          =   780
            Left            =   120
            Picture         =   "frmMapEditor.frx":A53DC
            ScaleHeight     =   48
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   48
            TabIndex        =   106
            Top             =   4440
            Width           =   780
         End
         Begin VB.PictureBox picStone3 
            AutoSize        =   -1  'True
            DragIcon        =   "frmMapEditor.frx":A6F1E
            DragMode        =   1  'Automatic
            Height          =   780
            Left            =   120
            Picture         =   "frmMapEditor.frx":A7360
            ScaleHeight     =   48
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   48
            TabIndex        =   105
            Top             =   3600
            Width           =   780
         End
         Begin VB.PictureBox picStone2 
            AutoSize        =   -1  'True
            DragIcon        =   "frmMapEditor.frx":A8EA2
            DragMode        =   1  'Automatic
            Height          =   780
            Left            =   120
            Picture         =   "frmMapEditor.frx":A92E4
            ScaleHeight     =   48
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   48
            TabIndex        =   104
            Top             =   2760
            Width           =   780
         End
         Begin VB.PictureBox picStone 
            AutoSize        =   -1  'True
            DragIcon        =   "frmMapEditor.frx":AAE26
            DragMode        =   1  'Automatic
            Height          =   780
            Left            =   120
            Picture         =   "frmMapEditor.frx":AB268
            ScaleHeight     =   48
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   48
            TabIndex        =   103
            Top             =   1920
            Width           =   780
         End
         Begin VB.PictureBox picSnow 
            AutoSize        =   -1  'True
            DragIcon        =   "frmMapEditor.frx":ACDAA
            DragMode        =   1  'Automatic
            Height          =   780
            Left            =   960
            Picture         =   "frmMapEditor.frx":AD1EC
            ScaleHeight     =   48
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   48
            TabIndex        =   102
            Top             =   1920
            Width           =   780
         End
         Begin VB.PictureBox picSand 
            AutoSize        =   -1  'True
            DragIcon        =   "frmMapEditor.frx":AED2E
            DragMode        =   1  'Automatic
            Height          =   780
            Left            =   960
            Picture         =   "frmMapEditor.frx":AF170
            ScaleHeight     =   48
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   48
            TabIndex        =   101
            Top             =   240
            Width           =   780
         End
         Begin VB.PictureBox picGrass3 
            AutoSize        =   -1  'True
            DragIcon        =   "frmMapEditor.frx":B0CB2
            DragMode        =   1  'Automatic
            Height          =   780
            Left            =   120
            Picture         =   "frmMapEditor.frx":B10F4
            ScaleHeight     =   48
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   48
            TabIndex        =   100
            Top             =   1080
            Width           =   780
         End
         Begin VB.PictureBox picGrass2 
            AutoSize        =   -1  'True
            DragIcon        =   "frmMapEditor.frx":B2C36
            DragMode        =   1  'Automatic
            Height          =   780
            Left            =   960
            Picture         =   "frmMapEditor.frx":B3078
            ScaleHeight     =   48
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   48
            TabIndex        =   99
            Top             =   1080
            Width           =   780
         End
         Begin VB.PictureBox picGrass 
            AutoSize        =   -1  'True
            DragIcon        =   "frmMapEditor.frx":B4BBA
            DragMode        =   1  'Automatic
            Height          =   780
            Left            =   120
            Picture         =   "frmMapEditor.frx":B4FFC
            ScaleHeight     =   48
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   48
            TabIndex        =   98
            Top             =   240
            Width           =   780
         End
         Begin VB.Image imgHouse13 
            Height          =   1095
            Left            =   1800
            Picture         =   "frmMapEditor.frx":B6B3E
            Stretch         =   -1  'True
            Top             =   2640
            Width           =   1095
         End
         Begin VB.Image imgHouse12 
            Height          =   1095
            Left            =   1800
            Picture         =   "frmMapEditor.frx":E2E10
            Stretch         =   -1  'True
            Top             =   1440
            Width           =   1095
         End
         Begin VB.Image imgHouse11 
            Height          =   1095
            Left            =   1800
            Picture         =   "frmMapEditor.frx":107092
            Stretch         =   -1  'True
            Top             =   240
            Width           =   1095
         End
      End
      Begin VB.Shape shpSet 
         BorderColor     =   &H8000000E&
         Height          =   255
         Left            =   0
         Shape           =   4  'Rounded Rectangle
         Top             =   0
         Width           =   255
         Visible         =   0   'False
      End
   End
   Begin VB.PictureBox picSWater2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   1440
      Picture         =   "frmMapEditor.frx":12CCE8
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   95
      Top             =   2880
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picMWater2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   360
      Picture         =   "frmMapEditor.frx":13028E
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   94
      Top             =   2880
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picSWater1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   1560
      Picture         =   "frmMapEditor.frx":133834
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   93
      Top             =   2640
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picMWater1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   480
      Picture         =   "frmMapEditor.frx":136DDA
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   92
      Top             =   2640
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picSStone4 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   1440
      Picture         =   "frmMapEditor.frx":13A380
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   91
      Top             =   3120
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picMStone4 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   360
      Picture         =   "frmMapEditor.frx":13D926
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   90
      Top             =   3120
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picSStone3 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   1440
      Picture         =   "frmMapEditor.frx":140ECC
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   89
      Top             =   2640
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picMStone3 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   360
      Picture         =   "frmMapEditor.frx":144472
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   88
      Top             =   2640
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picSStone2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   1320
      Picture         =   "frmMapEditor.frx":147A18
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   87
      Top             =   2880
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picMStone2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   240
      Picture         =   "frmMapEditor.frx":14AFBE
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   86
      Top             =   2880
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picSStone1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   1440
      Picture         =   "frmMapEditor.frx":14E564
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   85
      Top             =   2520
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picMStone1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   360
      Picture         =   "frmMapEditor.frx":151B0A
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   84
      Top             =   2520
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picSSnow 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   1440
      Picture         =   "frmMapEditor.frx":1550B0
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   83
      Top             =   2640
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picMSnow 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   360
      Picture         =   "frmMapEditor.frx":158656
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   82
      Top             =   2640
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picSSand2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   1800
      Picture         =   "frmMapEditor.frx":15BBFC
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   81
      Top             =   2400
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picMSand2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   720
      Picture         =   "frmMapEditor.frx":15F1A2
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   80
      Top             =   2400
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picSSand1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   1320
      Picture         =   "frmMapEditor.frx":162748
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   79
      Top             =   1920
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picMSand1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   240
      Picture         =   "frmMapEditor.frx":165CEE
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   78
      Top             =   1920
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picSGrass2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   1560
      Picture         =   "frmMapEditor.frx":169294
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   77
      Top             =   2400
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picMGrass2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   480
      Picture         =   "frmMapEditor.frx":16C83A
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   76
      Top             =   2400
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picSGrass1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   1320
      Picture         =   "frmMapEditor.frx":16FDE0
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   75
      Top             =   2160
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox picMGrass1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1065
      Left            =   240
      Picture         =   "frmMapEditor.frx":173386
      ScaleHeight     =   67
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   67
      TabIndex        =   74
      Top             =   2160
      Width           =   1065
      Visible         =   0   'False
   End
   Begin VB.PictureBox Picture1 
      Height          =   255
      Left            =   0
      ScaleHeight     =   195
      ScaleWidth      =   195
      TabIndex        =   72
      Top             =   1560
      Width           =   255
      Visible         =   0   'False
   End
   Begin VB.PictureBox picMbarrel1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1110
      Left            =   240
      Picture         =   "frmMapEditor.frx":17692C
      ScaleHeight     =   70
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   56
      TabIndex        =   71
      Top             =   2040
      Width           =   900
   End
   Begin VB.PictureBox picSbarrel1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1110
      Left            =   1080
      Picture         =   "frmMapEditor.frx":17975E
      ScaleHeight     =   70
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   56
      TabIndex        =   70
      Top             =   2040
      Width           =   900
   End
   Begin VB.PictureBox picSbarrel2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   1080
      Picture         =   "frmMapEditor.frx":17C590
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   50
      TabIndex        =   69
      Top             =   3120
      Width           =   810
   End
   Begin VB.PictureBox picMbarrel2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   240
      Picture         =   "frmMapEditor.frx":17EBD2
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   50
      TabIndex        =   68
      Top             =   3120
      Width           =   810
   End
   Begin VB.PictureBox picSBench1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   720
      Picture         =   "frmMapEditor.frx":181214
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   30
      TabIndex        =   67
      Top             =   4080
      Width           =   510
   End
   Begin VB.PictureBox picMBench1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   240
      Picture         =   "frmMapEditor.frx":182956
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   30
      TabIndex        =   66
      Top             =   4080
      Width           =   510
   End
   Begin VB.PictureBox picSBench2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1050
      Left            =   1680
      Picture         =   "frmMapEditor.frx":184098
      ScaleHeight     =   66
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   33
      TabIndex        =   65
      Top             =   4080
      Width           =   555
   End
   Begin VB.PictureBox picMBench2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1050
      Left            =   1200
      Picture         =   "frmMapEditor.frx":185AA2
      ScaleHeight     =   66
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   33
      TabIndex        =   64
      Top             =   4080
      Width           =   555
   End
   Begin VB.PictureBox picSBucket 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   570
      Left            =   2400
      Picture         =   "frmMapEditor.frx":1874AC
      ScaleHeight     =   34
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   31
      TabIndex        =   63
      Top             =   2040
      Width           =   525
   End
   Begin VB.PictureBox picMBucket 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   570
      Left            =   1920
      Picture         =   "frmMapEditor.frx":1881AE
      ScaleHeight     =   34
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   31
      TabIndex        =   62
      Top             =   2040
      Width           =   525
   End
   Begin VB.PictureBox picSCrate1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   1920
      Picture         =   "frmMapEditor.frx":188EB0
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   65
      TabIndex        =   61
      Top             =   2640
      Width           =   1035
   End
   Begin VB.PictureBox picMCrate1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   1920
      Picture         =   "frmMapEditor.frx":18BFF2
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   65
      TabIndex        =   60
      Top             =   2640
      Width           =   1035
   End
   Begin VB.PictureBox picSCrate2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1050
      Left            =   3840
      Picture         =   "frmMapEditor.frx":18F134
      ScaleHeight     =   66
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   54
      TabIndex        =   59
      Top             =   2040
      Width           =   870
   End
   Begin VB.PictureBox picMCrate2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1050
      Left            =   3000
      Picture         =   "frmMapEditor.frx":191BBE
      ScaleHeight     =   66
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   54
      TabIndex        =   58
      Top             =   2040
      Width           =   870
   End
   Begin VB.PictureBox picSDebree 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1455
      Left            =   4680
      Picture         =   "frmMapEditor.frx":194648
      ScaleHeight     =   93
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   103
      TabIndex        =   57
      Top             =   2040
      Width           =   1605
   End
   Begin VB.PictureBox picMDebree 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1455
      Left            =   4680
      Picture         =   "frmMapEditor.frx":19B7E2
      ScaleHeight     =   93
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   103
      TabIndex        =   56
      Top             =   2040
      Width           =   1605
   End
   Begin VB.PictureBox picSGateC 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1470
      Left            =   2880
      Picture         =   "frmMapEditor.frx":1A297C
      ScaleHeight     =   94
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   96
      TabIndex        =   55
      Top             =   3120
      Width           =   1500
   End
   Begin VB.PictureBox picMGateC 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1470
      Left            =   2880
      Picture         =   "frmMapEditor.frx":1A937E
      ScaleHeight     =   94
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   96
      TabIndex        =   54
      Top             =   3120
      Width           =   1500
   End
   Begin VB.PictureBox picSGateO 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1470
      Left            =   4440
      Picture         =   "frmMapEditor.frx":1AFD80
      ScaleHeight     =   94
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   96
      TabIndex        =   53
      Top             =   3120
      Width           =   1500
   End
   Begin VB.PictureBox picMGateO 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1470
      Left            =   4440
      Picture         =   "frmMapEditor.frx":1B6782
      ScaleHeight     =   94
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   96
      TabIndex        =   52
      Top             =   3120
      Width           =   1500
   End
   Begin VB.PictureBox picSHouse1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3285
      Left            =   240
      Picture         =   "frmMapEditor.frx":1BD184
      ScaleHeight     =   215
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   224
      TabIndex        =   51
      Top             =   5160
      Width           =   3420
   End
   Begin VB.PictureBox picMHouse1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3285
      Left            =   240
      Picture         =   "frmMapEditor.frx":1E0626
      ScaleHeight     =   215
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   224
      TabIndex        =   50
      Top             =   5160
      Width           =   3420
   End
   Begin VB.PictureBox picSHouse2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3060
      Left            =   240
      Picture         =   "frmMapEditor.frx":203AC8
      ScaleHeight     =   200
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   162
      TabIndex        =   49
      Top             =   5160
      Width           =   2490
   End
   Begin VB.PictureBox picMHouse2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3060
      Left            =   240
      Picture         =   "frmMapEditor.frx":21B84A
      ScaleHeight     =   200
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   162
      TabIndex        =   48
      Top             =   5160
      Width           =   2490
   End
   Begin VB.PictureBox picSHouse3 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3360
      Left            =   4800
      Picture         =   "frmMapEditor.frx":2335CC
      ScaleHeight     =   220
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   254
      TabIndex        =   47
      Top             =   4680
      Width           =   3870
   End
   Begin VB.PictureBox picMHouse3 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3360
      Left            =   3960
      Picture         =   "frmMapEditor.frx":25C69E
      ScaleHeight     =   220
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   254
      TabIndex        =   46
      Top             =   4680
      Width           =   3870
   End
   Begin VB.PictureBox picSHouse4 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3465
      Left            =   240
      Picture         =   "frmMapEditor.frx":285770
      ScaleHeight     =   227
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   154
      TabIndex        =   45
      Top             =   8280
      Width           =   2370
   End
   Begin VB.PictureBox picMHouse4 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3465
      Left            =   240
      Picture         =   "frmMapEditor.frx":29F322
      ScaleHeight     =   227
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   154
      TabIndex        =   44
      Top             =   8160
      Width           =   2370
   End
   Begin VB.PictureBox picSHouse5 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3375
      Left            =   360
      Picture         =   "frmMapEditor.frx":2B8ED4
      ScaleHeight     =   221
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   186
      TabIndex        =   43
      Top             =   7080
      Width           =   2850
   End
   Begin VB.PictureBox picMHouse5 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3375
      Left            =   480
      Picture         =   "frmMapEditor.frx":2D7286
      ScaleHeight     =   221
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   186
      TabIndex        =   42
      Top             =   7080
      Width           =   2850
   End
   Begin VB.PictureBox picSHouse6 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3165
      Left            =   360
      Picture         =   "frmMapEditor.frx":2F5638
      ScaleHeight     =   207
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   190
      TabIndex        =   41
      Top             =   6960
      Width           =   2910
   End
   Begin VB.PictureBox picMHouse6 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3165
      Left            =   240
      Picture         =   "frmMapEditor.frx":3124FE
      ScaleHeight     =   207
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   190
      TabIndex        =   40
      Top             =   6960
      Width           =   2910
   End
   Begin VB.PictureBox picSHouse7 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3840
      Left            =   720
      Picture         =   "frmMapEditor.frx":32F3C4
      ScaleHeight     =   252
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   252
      TabIndex        =   39
      Top             =   5640
      Width           =   3840
   End
   Begin VB.PictureBox picMHouse7 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3840
      Left            =   480
      Picture         =   "frmMapEditor.frx":35DC36
      ScaleHeight     =   252
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   252
      TabIndex        =   38
      Top             =   5880
      Width           =   3840
   End
   Begin VB.PictureBox picSHouse8 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3930
      Left            =   720
      Picture         =   "frmMapEditor.frx":38C4A8
      ScaleHeight     =   258
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   256
      TabIndex        =   37
      Top             =   5400
      Width           =   3900
   End
   Begin VB.PictureBox picMHouse8 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3930
      Left            =   960
      Picture         =   "frmMapEditor.frx":3BCAEA
      ScaleHeight     =   258
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   256
      TabIndex        =   36
      Top             =   6120
      Width           =   3900
   End
   Begin VB.PictureBox picSHouse9 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3435
      Left            =   240
      Picture         =   "frmMapEditor.frx":3ED12C
      ScaleHeight     =   225
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   192
      TabIndex        =   35
      Top             =   5040
      Width           =   2940
   End
   Begin VB.PictureBox picMHouse9 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3435
      Left            =   240
      Picture         =   "frmMapEditor.frx":40CBAE
      ScaleHeight     =   225
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   192
      TabIndex        =   34
      Top             =   5160
      Width           =   2940
   End
   Begin VB.PictureBox picSHouse10 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   4005
      Left            =   1680
      Picture         =   "frmMapEditor.frx":42C630
      ScaleHeight     =   263
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   193
      TabIndex        =   33
      Top             =   5520
      Width           =   2955
   End
   Begin VB.PictureBox picMHouse10 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   4005
      Left            =   2520
      Picture         =   "frmMapEditor.frx":451A4E
      ScaleHeight     =   263
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   193
      TabIndex        =   32
      Top             =   6240
      Width           =   2955
   End
   Begin VB.PictureBox picSHouse11 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   4005
      Left            =   960
      Picture         =   "frmMapEditor.frx":476E6C
      ScaleHeight     =   263
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   195
      TabIndex        =   31
      Top             =   4920
      Width           =   2985
   End
   Begin VB.PictureBox picMHouse11 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   4005
      Left            =   2400
      Picture         =   "frmMapEditor.frx":49CAC2
      ScaleHeight     =   263
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   195
      TabIndex        =   30
      Top             =   5640
      Width           =   2985
   End
   Begin VB.PictureBox picSHouse12 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3915
      Left            =   1800
      Picture         =   "frmMapEditor.frx":4C2718
      ScaleHeight     =   257
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   192
      TabIndex        =   29
      Top             =   5640
      Width           =   2940
   End
   Begin VB.PictureBox picMHouse12 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3915
      Left            =   1920
      Picture         =   "frmMapEditor.frx":4E699A
      ScaleHeight     =   257
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   192
      TabIndex        =   28
      Top             =   5880
      Width           =   2940
   End
   Begin VB.PictureBox picSHouse13 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3630
      Left            =   960
      Picture         =   "frmMapEditor.frx":50AC1C
      ScaleHeight     =   238
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   253
      TabIndex        =   27
      Top             =   5400
      Width           =   3855
   End
   Begin VB.PictureBox picMHouse13 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3630
      Left            =   1800
      Picture         =   "frmMapEditor.frx":536EEE
      ScaleHeight     =   238
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   253
      TabIndex        =   26
      Top             =   5760
      Width           =   3855
   End
   Begin VB.PictureBox picSPortal 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1440
      Left            =   7200
      Picture         =   "frmMapEditor.frx":5631C0
      ScaleHeight     =   92
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   95
      TabIndex        =   25
      Top             =   7800
      Width           =   1485
   End
   Begin VB.PictureBox picMPortal 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1440
      Left            =   5760
      Picture         =   "frmMapEditor.frx":569982
      ScaleHeight     =   92
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   95
      TabIndex        =   24
      Top             =   7920
      Width           =   1485
   End
   Begin VB.PictureBox picSRuin 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3960
      Left            =   1080
      Picture         =   "frmMapEditor.frx":570144
      ScaleHeight     =   260
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   194
      TabIndex        =   23
      Top             =   4920
      Width           =   2970
   End
   Begin VB.PictureBox picMRuin 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3960
      Left            =   720
      Picture         =   "frmMapEditor.frx":5952A6
      ScaleHeight     =   260
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   194
      TabIndex        =   22
      Top             =   5040
      Width           =   2970
   End
   Begin VB.PictureBox picSStatue1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1530
      Left            =   6600
      Picture         =   "frmMapEditor.frx":5BA408
      ScaleHeight     =   98
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   72
      TabIndex        =   21
      Top             =   9120
      Width           =   1140
   End
   Begin VB.PictureBox picMStatue1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1530
      Left            =   5760
      Picture         =   "frmMapEditor.frx":5BF6FA
      ScaleHeight     =   98
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   72
      TabIndex        =   20
      Top             =   9120
      Width           =   1140
   End
   Begin VB.PictureBox picSStatue2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1455
      Left            =   8280
      Picture         =   "frmMapEditor.frx":5C49EC
      ScaleHeight     =   93
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   80
      TabIndex        =   19
      Top             =   9240
      Width           =   1260
   End
   Begin VB.PictureBox picMStatue2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1455
      Left            =   7440
      Picture         =   "frmMapEditor.frx":5CA15E
      ScaleHeight     =   93
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   80
      TabIndex        =   18
      Top             =   9240
      Width           =   1260
   End
   Begin VB.PictureBox picSTower1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1965
      Left            =   7200
      Picture         =   "frmMapEditor.frx":5CF8D0
      ScaleHeight     =   127
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   96
      TabIndex        =   17
      Top             =   2520
      Width           =   1500
   End
   Begin VB.PictureBox picMTower1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1965
      Left            =   5760
      Picture         =   "frmMapEditor.frx":5D87F2
      ScaleHeight     =   127
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   96
      TabIndex        =   16
      Top             =   2400
      Width           =   1500
   End
   Begin VB.PictureBox picSTower2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1980
      Left            =   7320
      Picture         =   "frmMapEditor.frx":5E1714
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   60
      TabIndex        =   15
      Top             =   2040
      Width           =   960
   End
   Begin VB.PictureBox picMTower2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1980
      Left            =   6480
      Picture         =   "frmMapEditor.frx":5E7156
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   60
      TabIndex        =   14
      Top             =   2040
      Width           =   960
   End
   Begin VB.PictureBox picSWater 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   570
      Left            =   7680
      Picture         =   "frmMapEditor.frx":5ECB98
      ScaleHeight     =   34
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   33
      TabIndex        =   13
      Top             =   3960
      Width           =   555
   End
   Begin VB.PictureBox picMWater 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   570
      Left            =   6840
      Picture         =   "frmMapEditor.frx":5ED922
      ScaleHeight     =   34
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   33
      TabIndex        =   12
      Top             =   3960
      Width           =   555
   End
   Begin VB.PictureBox picSWell 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   7800
      Picture         =   "frmMapEditor.frx":5EE6AC
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   64
      TabIndex        =   11
      Top             =   4080
      Width           =   1020
   End
   Begin VB.PictureBox picMWell 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1020
      Left            =   6360
      Picture         =   "frmMapEditor.frx":5F16EE
      ScaleHeight     =   64
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   64
      TabIndex        =   10
      Top             =   3960
      Width           =   1020
   End
   Begin VB.PictureBox picSGran 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   2010
      Left            =   3000
      Picture         =   "frmMapEditor.frx":5F4730
      ScaleHeight     =   130
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   125
      TabIndex        =   9
      Top             =   3960
      Width           =   1935
   End
   Begin VB.PictureBox picMGran 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   2010
      Left            =   3000
      Picture         =   "frmMapEditor.frx":600662
      ScaleHeight     =   130
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   125
      TabIndex        =   8
      Top             =   3960
      Width           =   1935
   End
   Begin VB.PictureBox picSTräd1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1980
      Left            =   5160
      Picture         =   "frmMapEditor.frx":60C594
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   120
      TabIndex        =   7
      Top             =   6360
      Width           =   1860
   End
   Begin VB.PictureBox picMTräd1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1980
      Left            =   3120
      Picture         =   "frmMapEditor.frx":6179D6
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   120
      TabIndex        =   6
      Top             =   6000
      Width           =   1860
   End
   Begin VB.PictureBox picSTräd2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1995
      Left            =   5280
      Picture         =   "frmMapEditor.frx":622E18
      ScaleHeight     =   129
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   120
      TabIndex        =   5
      Top             =   4680
      Width           =   1860
   End
   Begin VB.PictureBox picMTräd2 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1995
      Left            =   4920
      Picture         =   "frmMapEditor.frx":62E3C2
      ScaleHeight     =   129
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   120
      TabIndex        =   4
      Top             =   4800
      Width           =   1860
   End
   Begin VB.PictureBox picSTräd3 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1980
      Left            =   7320
      Picture         =   "frmMapEditor.frx":63996C
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   122
      TabIndex        =   3
      Top             =   6000
      Width           =   1890
   End
   Begin VB.PictureBox picMTräd3 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1980
      Left            =   4920
      Picture         =   "frmMapEditor.frx":6451AE
      ScaleHeight     =   128
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   122
      TabIndex        =   2
      Top             =   5280
      Width           =   1890
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   16680
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Height          =   195
      Left            =   1680
      TabIndex        =   111
      Top             =   1560
      Width           =   45
   End
   Begin VB.Label lblPaint 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Paint"
      BeginProperty Font 
         Name            =   "Abduction"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00C0C0C0&
      Height          =   270
      Left            =   13920
      TabIndex        =   73
      Top             =   1440
      Width           =   975
   End
   Begin VB.Label lblMisc 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Misc"
      BeginProperty Font 
         Name            =   "Abduction"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00C0C0C0&
      Height          =   270
      Left            =   15120
      TabIndex        =   1
      Top             =   1440
      Width           =   780
   End
   Begin VB.Label lblMore 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "More"
      BeginProperty Font 
         Name            =   "Abduction"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00C0C0C0&
      Height          =   270
      Left            =   16200
      TabIndex        =   0
      Top             =   1440
      Width           =   780
   End
   Begin VB.Image imgHouse10 
      Height          =   1410
      Left            =   16080
      Picture         =   "frmMapEditor.frx":6509F0
      Stretch         =   -1  'True
      Top             =   0
      Width           =   1275
   End
   Begin VB.Image imgHouse9 
      Height          =   1410
      Left            =   14760
      Picture         =   "frmMapEditor.frx":675E0E
      Stretch         =   -1  'True
      Top             =   0
      Width           =   1275
   End
   Begin VB.Image imgHouse8 
      Height          =   1410
      Left            =   13440
      Picture         =   "frmMapEditor.frx":695890
      Stretch         =   -1  'True
      Top             =   0
      Width           =   1275
   End
   Begin VB.Image imgHouse7 
      Height          =   1410
      Left            =   12120
      Picture         =   "frmMapEditor.frx":6C5ED2
      Stretch         =   -1  'True
      Top             =   0
      Width           =   1275
   End
   Begin VB.Image imgHouse6 
      Height          =   1410
      Left            =   10800
      Picture         =   "frmMapEditor.frx":6F4744
      Stretch         =   -1  'True
      Top             =   0
      Width           =   1275
   End
   Begin VB.Image imgHouse5 
      Height          =   1410
      Left            =   9480
      Picture         =   "frmMapEditor.frx":71160A
      Stretch         =   -1  'True
      Top             =   0
      Width           =   1275
   End
   Begin VB.Image imgHouse4 
      Height          =   1410
      Left            =   8160
      Picture         =   "frmMapEditor.frx":72F9BC
      Stretch         =   -1  'True
      Top             =   0
      Width           =   1275
   End
   Begin VB.Image imgHouse3 
      Height          =   1410
      Left            =   6840
      Picture         =   "frmMapEditor.frx":74956E
      Stretch         =   -1  'True
      Top             =   0
      Width           =   1275
   End
   Begin VB.Image imgHouse2 
      Height          =   1410
      Left            =   5520
      Picture         =   "frmMapEditor.frx":772640
      Stretch         =   -1  'True
      Top             =   0
      Width           =   1275
   End
   Begin VB.Image imgHouse1 
      Height          =   1410
      Left            =   4200
      Picture         =   "frmMapEditor.frx":78A3C2
      Stretch         =   -1  'True
      Top             =   0
      Width           =   1275
   End
   Begin VB.Image imgGateO 
      Height          =   690
      Left            =   3600
      Picture         =   "frmMapEditor.frx":7AD864
      Stretch         =   -1  'True
      Top             =   720
      Width           =   555
   End
   Begin VB.Image imgGateC 
      Height          =   690
      Left            =   3600
      Picture         =   "frmMapEditor.frx":7B4266
      Stretch         =   -1  'True
      Top             =   0
      Width           =   555
   End
   Begin VB.Image imgDebree 
      Height          =   1410
      Left            =   2280
      Picture         =   "frmMapEditor.frx":7BAC68
      Stretch         =   -1  'True
      Top             =   0
      Width           =   1275
   End
   Begin VB.Image imgCrate2 
      Height          =   690
      Left            =   1680
      Picture         =   "frmMapEditor.frx":7C1E02
      Stretch         =   -1  'True
      Top             =   0
      Width           =   555
   End
   Begin VB.Image imgCrate1 
      Height          =   690
      Left            =   1080
      Picture         =   "frmMapEditor.frx":7C488C
      Stretch         =   -1  'True
      Top             =   720
      Width           =   555
   End
   Begin VB.Image imgBucket 
      Height          =   690
      Left            =   1080
      Picture         =   "frmMapEditor.frx":7C79CE
      Stretch         =   -1  'True
      Top             =   0
      Width           =   555
   End
   Begin VB.Image imgBench2 
      Height          =   690
      Left            =   600
      Picture         =   "frmMapEditor.frx":7C86D0
      Stretch         =   -1  'True
      Top             =   720
      Width           =   555
   End
   Begin VB.Image imgBench1 
      Height          =   690
      Left            =   600
      Picture         =   "frmMapEditor.frx":7CA0DA
      Stretch         =   -1  'True
      Top             =   0
      Width           =   555
   End
   Begin VB.Image imgBarrel2 
      Height          =   690
      Left            =   0
      Picture         =   "frmMapEditor.frx":7CB81C
      Stretch         =   -1  'True
      Top             =   720
      Width           =   555
   End
   Begin VB.Image imgBarrel1 
      Height          =   690
      Left            =   0
      Picture         =   "frmMapEditor.frx":7CDE5E
      Stretch         =   -1  'True
      Top             =   0
      Width           =   555
   End
   Begin VB.Menu main 
      Caption         =   "Main"
      Begin VB.Menu new 
         Caption         =   "New Map"
      End
      Begin VB.Menu load 
         Caption         =   "Load Map"
      End
      Begin VB.Menu save 
         Caption         =   "Save Map"
      End
      Begin VB.Menu line 
         Caption         =   "-"
      End
      Begin VB.Menu exit 
         Caption         =   "Exit"
      End
   End
End
Attribute VB_Name = "frmMapEditor"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim Responce ' Public variable for MsgBoxes responce.
Dim Changed  ' See if the picture have been changed.
Dim MOUSEDWN As Boolean
Dim Paint As Boolean
Dim ToBuild As Integer, ToPaint As Integer
Private Sub exit_Click()
If Changed = 1 Then
    Responce = MsgBox("Save the map before you exit?", vbQuestion + vbYesNoCancel)

    If Responce = vbCancel Then
        Exit Sub
    ElseIf Responce = vbYes Then
        save_Click
    Else
    ' Do nothing here.
    End If
End If

Changed = 0 ' No changes have been made.

End ' Terminate the program.
End Sub

Private Sub Form_Load()
ToBuild = 0
End Sub

Private Sub Form_Unload(Cancel As Integer)
If Changed = 1 Then
    Responce = MsgBox("Save the map before you exit?", vbQuestion + vbYesNoCancel)

    If Responce = vbCancel Then
        Exit Sub
    ElseIf Responce = vbYes Then
        save_Click
    Else
    ' Do nothing here.
    End If
End If

Changed = 0 ' No changes have been made.

End ' Terminate the program.
End Sub

Private Sub Image1_Click()
shpSet.Width = picMPortal.ScaleWidth
shpSet.Height = picMPortal.ScaleHeight
ToBuild = 28
shpSet.Visible = True
End Sub
Private Sub Image2_Click()
shpSet.Width = picMRuin.ScaleWidth
shpSet.Height = picMRuin.ScaleHeight
ToBuild = 29
shpSet.Visible = True
End Sub

Private Sub Image3_Click()
shpSet.Width = picMWell.ScaleWidth
shpSet.Height = picMWell.ScaleHeight
ToBuild = 34
shpSet.Visible = True
End Sub

Private Sub Image4_Click()
shpSet.Width = picMStatue1.ScaleWidth
shpSet.Height = picMStatue1.ScaleHeight
ToBuild = 30
shpSet.Visible = True
End Sub

Private Sub Image5_Click()
shpSet.Width = picMStatue2.ScaleWidth
shpSet.Height = picMStatue2.ScaleHeight
ToBuild = 31
shpSet.Visible = True
End Sub

Private Sub Image6_Click()
shpSet.Width = picMTower2.ScaleWidth
shpSet.Height = picMTower2.ScaleHeight
ToBuild = 33
shpSet.Visible = True
End Sub

Private Sub Image7_Click()
shpSet.Width = picMWater.ScaleWidth
shpSet.Height = picMWater.ScaleHeight
ToBuild = 35
shpSet.Visible = True
End Sub

Private Sub Image8_Click()
shpSet.Width = picMTower1.ScaleWidth
shpSet.Height = picMTower1.ScaleHeight
ToBuild = 32
shpSet.Visible = True
End Sub

Private Sub imgBarrel1_Click()
shpSet.Width = picMbarrel1.ScaleWidth
shpSet.Height = picMbarrel1.ScaleHeight
ToBuild = 1
shpSet.Visible = True
End Sub

Private Sub imgBarrel2_Click()
shpSet.Width = picMbarrel2.ScaleWidth
shpSet.Height = picMbarrel2.ScaleHeight
ToBuild = 2
shpSet.Visible = True
End Sub

Private Sub imgBench1_Click()
shpSet.Width = picMBench1.ScaleWidth
shpSet.Height = picMBench1.ScaleHeight
ToBuild = 3
shpSet.Visible = True
End Sub

Private Sub imgBench2_Click()
shpSet.Width = picMBench2.ScaleWidth
shpSet.Height = picMBench2.ScaleHeight
ToBuild = 4
shpSet.Visible = True
End Sub

Private Sub imgBucket_Click()
shpSet.Width = picMBucket.ScaleWidth
shpSet.Height = picMBucket.ScaleHeight
ToBuild = 5
shpSet.Visible = True
End Sub

Private Sub imgCrate1_Click()
shpSet.Width = picMCrate1.ScaleWidth
shpSet.Height = picMCrate1.ScaleHeight
ToBuild = 6
shpSet.Visible = True
End Sub

Private Sub imgCrate2_Click()
shpSet.Width = picMCrate2.ScaleWidth
shpSet.Height = picMCrate2.ScaleHeight
ToBuild = 7
shpSet.Visible = True
End Sub

Private Sub imgDebree_Click()
shpSet.Width = picMDebree.ScaleWidth
shpSet.Height = picMDebree.ScaleHeight
ToBuild = 8
shpSet.Visible = True
End Sub

Private Sub imgGateC_Click()
shpSet.Width = picMGateC.ScaleWidth
shpSet.Height = picMGateC.ScaleHeight
ToBuild = 9
shpSet.Visible = True
End Sub

Private Sub imgGateO_Click()
shpSet.Width = picMGateO.ScaleWidth
shpSet.Height = picMGateO.ScaleHeight
ToBuild = 10
shpSet.Visible = True
End Sub

Private Sub imgGran_Click()
shpSet.Width = picMGran.ScaleWidth
shpSet.Height = picMGran.ScaleHeight
ToBuild = 24
shpSet.Visible = True
End Sub

Private Sub imgGrass1_Click()
Paint = True
ToPaint = 1
End Sub

Private Sub imgGrass2_Click()
Paint = True
ToPaint = 2
End Sub

Private Sub imgHouse1_Click()
shpSet.Width = picMHouse1.ScaleWidth
shpSet.Height = picMHouse1.ScaleHeight
ToBuild = 11
shpSet.Visible = True
End Sub

Private Sub imgHouse10_Click()
shpSet.Width = picMHouse10.ScaleWidth
shpSet.Height = picMHouse10.ScaleHeight
ToBuild = 20
shpSet.Visible = True
End Sub

Private Sub imgHouse11_Click()
shpSet.Width = picMHouse11.ScaleWidth
shpSet.Height = picMHouse11.ScaleHeight
ToBuild = 21
shpSet.Visible = True
End Sub

Private Sub imgHouse12_Click()
shpSet.Width = picMHouse12.ScaleWidth
shpSet.Height = picMHouse12.ScaleHeight
ToBuild = 22
shpSet.Visible = True
End Sub

Private Sub imgHouse13_Click()
shpSet.Width = picMHouse13.ScaleWidth
shpSet.Height = picMHouse13.ScaleHeight
ToBuild = 23
shpSet.Visible = True
End Sub

Private Sub imgHouse2_Click()
shpSet.Width = picMHouse2.ScaleWidth
shpSet.Height = picMHouse2.ScaleHeight
ToBuild = 12
shpSet.Visible = True
End Sub

Private Sub imgHouse3_Click()
shpSet.Width = picMHouse3.ScaleWidth
shpSet.Height = picMHouse3.ScaleHeight
ToBuild = 13
shpSet.Visible = True
End Sub

Private Sub imgHouse4_Click()
shpSet.Width = picMHouse4.ScaleWidth
shpSet.Height = picMHouse4.ScaleHeight
ToBuild = 14
shpSet.Visible = True
End Sub

Private Sub imgHouse5_Click()
shpSet.Width = picMHouse5.ScaleWidth
shpSet.Height = picMHouse5.ScaleHeight
ToBuild = 15
shpSet.Visible = True
End Sub

Private Sub imgHouse6_Click()
shpSet.Width = picMHouse6.ScaleWidth
shpSet.Height = picMHouse6.ScaleHeight
ToBuild = 16
shpSet.Visible = True
End Sub

Private Sub imgHouse7_Click()
shpSet.Width = picMHouse7.ScaleWidth
shpSet.Height = picMHouse7.ScaleHeight
ToBuild = 17
shpSet.Visible = True
End Sub

Private Sub imgHouse8_Click()
shpSet.Width = picMHouse8.ScaleWidth
shpSet.Height = picMHouse8.ScaleHeight
ToBuild = 18
shpSet.Visible = True
End Sub

Private Sub imgHouse9_Click()
shpSet.Width = picMHouse9.ScaleWidth
shpSet.Height = picMHouse9.ScaleHeight
ToBuild = 19
shpSet.Visible = True
End Sub

Private Sub imgSand1_Click()
Paint = True
ToPaint = 3
End Sub

Private Sub imgSand2_Click()
Paint = True
ToPaint = 4
End Sub

Private Sub imgSnow_Click()
Paint = True
ToPaint = 5
End Sub

Private Sub imgStone1_Click()
Paint = True
ToPaint = 6
End Sub

Private Sub imgStone2_Click()
Paint = True
ToPaint = 7
End Sub

Private Sub imgStone3_Click()
Paint = True
ToPaint = 8
End Sub

Private Sub imgStone4_Click()
Paint = True
ToPaint = 9
End Sub

Private Sub imgTräd1_Click()
shpSet.Width = picMTräd1.ScaleWidth
shpSet.Height = picMTräd1.ScaleHeight
ToBuild = 25
shpSet.Visible = True
End Sub

Private Sub imgTräd2_Click()
shpSet.Width = picMTräd2.ScaleWidth
shpSet.Height = picMTräd2.ScaleHeight
ToBuild = 27
shpSet.Visible = True
End Sub

Private Sub imgTräd3_Click()
shpSet.Width = picMTräd3.ScaleWidth
shpSet.Height = picMTräd3.ScaleHeight
ToBuild = 26
shpSet.Visible = True
End Sub

Private Sub imgWater1_Click()
Paint = True
ToPaint = 10
End Sub

Private Sub imgWater2_Click()
Paint = True
ToPaint = 11
End Sub

Private Sub lblMisc_Click()
If frmMisc.Visible = False Then
lblMisc.ForeColor = vbWhite
lblMore.ForeColor = &HC0C0C0
lblPaint.ForeColor = &HC0C0C0
frmMisc.Visible = True
frmMore.Visible = False
frmPaint.Visible = False
Else
lblMisc.ForeColor = &HC0C0C0
frmMisc.Visible = False
End If
End Sub

Private Sub lblMore_Click()
If frmMore.Visible = False Then
lblMore.ForeColor = vbWhite
lblMisc.ForeColor = &HC0C0C0
lblPaint.ForeColor = &HC0C0C0
frmMore.Visible = True
frmMisc.Visible = False
frmPaint.Visible = False
Else
lblMore.ForeColor = &HC0C0C0
frmMore.Visible = False
End If
End Sub

Private Sub lblPaint_Click()
If frmPaint.Visible = False Then
lblPaint.ForeColor = vbWhite
lblMisc.ForeColor = &HC0C0C0
lblMore.ForeColor = &HC0C0C0
frmPaint.Visible = True
frmMisc.Visible = False
frmMore.Visible = False
Else
lblPaint.ForeColor = &HC0C0C0
frmPaint.Visible = False
End If
End Sub

Private Sub load_Click()
If Changed = 1 Then
    Responce = MsgBox("Save the map before you exit?", vbQuestion + vbYesNoCancel)

    If Responce = vbCancel Then
        Exit Sub
    ElseIf Responce = vbYes Then
        save_Click
    Else
    ' Do nothing here.
    End If
End If

CommonDialog1.Filter = "Map Files (*.map)|*.map|" ' Show only *.map-files.
CommonDialog1.Action = 1 ' Show the Open Window.
If CommonDialog1.FileName = "" Then GoTo Cancel ' If no file is selected
' then goto cancel.

picMap.Picture = LoadPicture(CommonDialog1.FileName) 'Load picture from file.

' Show the name of the map in the caption of the form.
frmMapEditor.Caption = "Ultimate Chaos Map Editor - " + CommonDialog1.FileTitle

Changed = 0 ' No changes have been made.

Cancel:
End Sub

Private Sub new_Click()
If Changed = 1 Then
    Responce = MsgBox("Save the map before you exit?", vbQuestion + vbYesNoCancel)

    If Responce = vbCancel Then
        Exit Sub
    ElseIf Responce = vbYes Then
        save_Click
    Else
    ' Do nothing here.
    End If
End If

picMap.Picture = LoadPicture() ' Make a new map.

' Show the name "NyKarta.map" in the Caption of the form.
frmMapEditor.Caption = "Ultimate Chaos Map Editor - Untitled.map"

Changed = 0 ' No changes have been made.
End Sub
Private Sub picMap_DragDrop(Source As Control, X As Single, Y As Single)
Changed = 1 ' The bitmap have changed.

' Center the Source.
X = X - Source.ScaleWidth / 2
Y = Y - Source.ScaleHeight / 2

' Sprites.

 Call BitBlt(picMap.hDC, X, Y, Source.ScaleWidth, Source.ScaleHeight, Source.hDC, 0, 0, SRCCOPY)
 'Refresh Screen.
 picMap.Refresh

' Backgrounds.

    For X = picMap.ScaleLeft To picMap.ScaleWidth Step Source.ScaleWidth
        For Y = picMap.ScaleTop To picMap.ScaleHeight Step Source.ScaleHeight
        Call BitBlt(picMap.hDC, X, Y, Source.ScaleWidth, Source.ScaleHeight, Source.hDC, 0, 0, SRCCOPY)
        ' Refresh Screen.
        picMap.Refresh
        Next
    Next

End Sub

Private Sub picMap_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
If Button = 1 Then
If Paint = True Then
MOUSEDWN = True
Else
If shpSet.BorderColor = vbRed Then
Else
If ToBuild = 1 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMbarrel1.Width, picMbarrel1.Height, picMbarrel1.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMbarrel1.Width, picMbarrel1.Height, picSbarrel1.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 2 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMbarrel2.Width, picMbarrel2.Height, picMbarrel2.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMbarrel2.Width, picMbarrel2.Height, picSbarrel2.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 3 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMBench1.Width, picMBench1.Height, picMBench1.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMBench1.Width, picMBench1.Height, picSBench1.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 4 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMBench2.Width, picMBench2.Height, picMBench2.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMBench2.Width, picMBench2.Height, picSBench2.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 5 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMBucket.Width, picMBucket.Height, picMBucket.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMBucket.Width, picMBucket.Height, picSBucket.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 6 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMCrate1.Width, picMCrate1.Height, picMCrate1.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMCrate1.Width, picMCrate1.Height, picSCrate1.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 7 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMCrate2.Width, picMCrate2.Height, picMCrate2.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMCrate2.Width, picMCrate2.Height, picSCrate2.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 8 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMDebree.Width, picMDebree.Height, picMDebree.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMDebree.Width, picMDebree.Height, picSDebree.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 9 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMGateC.Width, picMGateC.Height, picMGateC.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMGateC.Width, picMGateC.Height, picSGateC.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 10 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMGateO.Width, picMGateO.Height, picMGateO.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMGateO.Width, picMGateO.Height, picSGateO.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 11 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMHouse1.Width, picMHouse1.Height, picMHouse1.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMHouse1.Width, picMHouse1.Height, picSHouse1.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 12 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMHouse2.Width, picMHouse2.Height, picMHouse2.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMHouse2.Width, picMHouse2.Height, picSHouse2.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 13 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMHouse3.Width, picMHouse3.Height, picMHouse3.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMHouse3.Width, picMHouse3.Height, picSHouse3.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 14 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMHouse4.Width, picMHouse4.Height, picMHouse4.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMHouse4.Width, picMHouse4.Height, picSHouse4.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 15 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMHouse5.Width, picMHouse5.Height, picMHouse5.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMHouse5.Width, picMHouse5.Height, picSHouse5.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 16 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMHouse6.Width, picMHouse6.Height, picMHouse6.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMHouse6.Width, picMHouse6.Height, picSHouse6.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 17 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMHouse7.Width, picMHouse7.Height, picMHouse7.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMHouse7.Width, picMHouse7.Height, picSHouse7.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 18 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMHouse8.Width, picMHouse8.Height, picMHouse8.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMHouse8.Width, picMHouse8.Height, picSHouse8.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 19 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMHouse9.Width, picMHouse9.Height, picMHouse9.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMHouse9.Width, picMHouse9.Height, picSHouse9.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 20 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMHouse10.Width, picMHouse10.Height, picMHouse10.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMHouse10.Width, picMHouse10.Height, picSHouse10.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 21 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMHouse11.Width, picMHouse11.Height, picMHouse11.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMHouse11.Width, picMHouse11.Height, picSHouse11.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 22 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMHouse12.Width, picMHouse12.Height, picMHouse12.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMHouse12.Width, picMHouse12.Height, picSHouse12.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 23 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMHouse13.Width, picMHouse13.Height, picMHouse13.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMHouse13.Width, picMHouse13.Height, picSHouse13.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 24 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMGran.Width, picMGran.Height, picMGran.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMGran.Width, picMGran.Height, picSGran.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 25 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMTräd1.Width, picMTräd1.Height, picMTräd1.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMTräd1.Width, picMTräd1.Height, picSTräd1.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 26 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMTräd3.Width, picMTräd3.Height, picMTräd3.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMTräd3.Width, picMTräd3.Height, picSTräd3.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 27 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMTräd2.Width, picMTräd2.Height, picMTräd2.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMTräd2.Width, picMTräd2.Height, picSTräd2.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 28 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMPortal.Width, picMPortal.Height, picMPortal.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMPortal.Width, picMPortal.Height, picSPortal.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 29 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMRuin.Width, picMRuin.Height, picMRuin.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMRuin.Width, picMRuin.Height, picSRuin.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 30 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMStatue1.Width, picMStatue1.Height, picMStatue1.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMStatue1.Width, picMStatue1.Height, picSStatue1.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 31 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMStatue2.Width, picMStatue2.Height, picMStatue2.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMStatue2.Width, picMStatue2.Height, picSStatue2.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 32 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMTower1.Width, picMTower1.Height, picMTower1.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMTower1.Width, picMTower1.Height, picSTower1.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 33 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMTower2.Width, picMTower2.Height, picMTower2.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMTower2.Width, picMTower2.Height, picSTower2.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 34 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMWell.Width, picMWell.Height, picMWell.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMWell.Width, picMWell.Height, picSWell.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToBuild = 35 Then
Changed = 1
Call BitBlt(picMap.hDC, X, Y, picMWater.Width, picMWater.Height, picMWater.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, picMWater.Width, picMWater.Height, picSWater.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
End If
End If
End If
Label1.Caption = X & " " & Y
If Button = 2 Then
Paint = False
ToBuild = 0
ToPaint = 0
shpSet.Visible = False
End If
End Sub

Private Sub picMap_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
If shpSet.Visible = True Then
shpSet.Top = Y
shpSet.Left = X
End If
If MOUSEDWN = True Then
X = X - 33
Y = Y - 33
If ToPaint = 1 Then
Call BitBlt(picMap.hDC, X, Y, 67, 67, picMGrass1.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, 67, 67, picSGrass1.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToPaint = 2 Then
Call BitBlt(picMap.hDC, X, Y, 67, 67, picMGrass2.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, 67, 67, picSGrass2.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToPaint = 3 Then
Call BitBlt(picMap.hDC, X, Y, 67, 67, picMSand1.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, 67, 67, picSSand1.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToPaint = 4 Then
Call BitBlt(picMap.hDC, X, Y, 67, 67, picMSand2.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, 67, 67, picSSand2.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToPaint = 5 Then
Call BitBlt(picMap.hDC, X, Y, 67, 67, picMSnow.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, 67, 67, picSSnow.hDC, 0, 0, SRCINVERT)
End If
If ToPaint = 6 Then
Call BitBlt(picMap.hDC, X, Y, 67, 67, picMStone1.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, 67, 67, picSStone1.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToPaint = 7 Then
Call BitBlt(picMap.hDC, X, Y, 67, 67, picMStone2.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, 67, 67, picSStone2.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToPaint = 8 Then
Call BitBlt(picMap.hDC, X, Y, 67, 67, picMStone3.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, 67, 67, picSStone3.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToPaint = 9 Then
Call BitBlt(picMap.hDC, X, Y, 67, 67, picMStone4.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, 67, 67, picSStone4.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToPaint = 10 Then
Call BitBlt(picMap.hDC, X, Y, 67, 67, picMWater1.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, 67, 67, picSWater1.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
If ToPaint = 11 Then
Call BitBlt(picMap.hDC, X, Y, 67, 67, picMWater2.hDC, 0, 0, SRCAND)
Call BitBlt(picMap.hDC, X, Y, 67, 67, picSWater2.hDC, 0, 0, SRCINVERT)
picMap.Refresh
End If
End If
End Sub

Private Sub picMap_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
MOUSEDWN = False
Paint = False
End Sub
Private Sub save_Click()

On Error GoTo Cancel
CommonDialog1.Filter = "Map Files (*.map)|*.map|"
CommonDialog1.Action = 2
SavePicture picMap.Image, CommonDialog1.FileName 'Save picture to file.

' Show the name of the map in the caption of the form.
frmMapEditor.Caption = "Ultimate Chaos Map Editor - " + CommonDialog1.FileTitle

Changed = 0 ' No changes have been made.

Cancel:
End Sub
