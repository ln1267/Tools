VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} cahzhi1 
   Caption         =   "UserForm1"
   ClientHeight    =   3180
   ClientLeft      =   45
   ClientTop       =   375
   ClientWidth     =   4710
   OleObjectBlob   =   "climate_chazhi.frx":0000
   StartUpPosition =   1  '所有者中心
End
Attribute VB_Name = "cahzhi1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub CommandButton1_Click()
a = 2
For cell = 1 To 684
b = 5
    For y = 1960 To 1981
        For m = 1 To 12
        
        If (Sheet1.Cells(a, 1) = Sheet6.Cells(cell + 1, 1) And Sheet1.Cells(a, 5) = y And Sheet1.Cells(a, 6) = m) Then
        
        Sheet6.Cells(cell + 1, b) = Sheet1.Cells(a, 8)
        b = b + 1
        a = a + 1
        Else
        
        Sheet6.Cells(cell + 1, b) = "NA"
        b = b + 1
        End If
        
        Next m
    Next y
Next cell

MsgBox ("ok!")

End Sub
