VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} UserForm1 
   Caption         =   "UserForm1"
   ClientHeight    =   3120
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   4710
   OleObjectBlob   =   "计算效率系数.frx":0000
   StartUpPosition =   1  '所有者中心
End
Attribute VB_Name = "UserForm1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub CommandButton1_Click()
n = InputBox("请输入观测数", n)
Sheet1.Cells(1, 5) = "NS"
Sheet1.Cells(1, 6) = "SUM_U"
Sheet1.Cells(1, 7) = "SUM_D"
a = 2
num = n
sum1 = 0
sum2 = 0
For i = 1 To n
    sum1 = sum1 + (Sheet1.Cells(a, 3) - Sheet1.Cells(a, 4)) * (Sheet1.Cells(a, 3) - Sheet1.Cells(a, 4))
    Sum = Sum + Sheet1.Cells(a, 3)
    a = a + 1
Next i
Sheet1.Cells(2, 6) = sum1
a = 2
For i = 1 To n
    sum2 = sum2 + (Sheet1.Cells(a, 3) - Sum / num) * (Sheet1.Cells(a, 3) - Sum / num)
    a = a + 1
Next i
Sheet1.Cells(2, 7) = sum2
ns = 1 - sum1 / sum2

Sheet1.Cells(2, 5) = ns
End Sub
