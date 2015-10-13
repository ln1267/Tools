VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} UserForm1 
   Caption         =   "UserForm1"
   ClientHeight    =   5370
   ClientLeft      =   45
   ClientTop       =   375
   ClientWidth     =   4770
   OleObjectBlob   =   "计算土壤质地.frx":0000
   StartUpPosition =   1  '所有者中心
End
Attribute VB_Name = "UserForm1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False



Private Sub CommandButton2_Click()

b = 0
For j = 1 To 5
a = 2
For i = 1 To 1776
    
    If Sheet1.Cells(a, b + 2) > 90 Then
         Sheet1.Cells(a, b + 5) = "sand"
    ElseIf Sheet1.Cells(a, b + 2) > 80 Then Sheet1.Cells(a, b + 5) = "loam sand"
    
    ElseIf Sheet1.Cells(a, b + 2) < 80 And Sheet1.Cells(a, b + 2) > 50 And Sheet1.Cells(a, b + 4) < 20 Then Sheet1.Cells(a, b + 5) = "sand loam"
        
    ElseIf Sheet1.Cells(a, b + 2) < 80 And Sheet1.Cells(a, b + 2) > 45 And Sheet1.Cells(a, b + 4) > 20 And Sheet1.Cells(a, b + 4) < 35 Then Sheet1.Cells(a, b + 5) = "sand clay loam"
        
    ElseIf Sheet1.Cells(a, b + 2) < 80 And Sheet1.Cells(a, b + 2) > 45 And Sheet1.Cells(a, b + 4) < 55 And Sheet1.Cells(a, b + 4) > 35 Then Sheet1.Cells(a, b + 5) = "sand clay"
        
    ElseIf Sheet1.Cells(a, b + 2) < 45 And Sheet1.Cells(a, b + 4) > 40 And Sheet1.Cells(a, b + 4) < 60 And Sheet1.Cells(a, b + 3) < 40 Then Sheet1.Cells(a, b + 5) = "clay"
    
    ElseIf Sheet1.Cells(a, b + 2) < 45 And Sheet1.Cells(a, b + 4) > 40 And Sheet1.Cells(a, b + 4) < 60 And Sheet1.Cells(a, b + 3) > 40 Then Sheet1.Cells(a, b + 5) = "silt clay"
        
    ElseIf Sheet1.Cells(a, b + 2) < 20 And Sheet1.Cells(a, b + 4) < 40 And Sheet1.Cells(a, b + 4) > 28 Then Sheet1.Cells(a, b + 5) = "silt clay loam"
    
    ElseIf Sheet1.Cells(a, b + 2) > 20 And Sheet1.Cells(a, b + 2) < 45 And Sheet1.Cells(a, b + 4) < 40 And Sheet1.Cells(a, b + 4) > 28 Then Sheet1.Cells(a, b + 5) = "clay loam"
    
    ElseIf Sheet1.Cells(a, b + 3) > 80 Then Sheet1.Cells(a, b + 5) = "silt"
    
    ElseIf Sheet1.Cells(a, b + 4) < 20 And Sheet1.Cells(a, b + 3) > 50 And Sheet1.Cells(a, b + 3) < 80 Then Sheet1.Cells(a, b + 5) = "silt loam"
    
    ElseIf Sheet1.Cells(a, b + 4) > 20 And Sheet1.Cells(a, b + 4) < 28 And Sheet1.Cells(a, b + 3) > 50 Then Sheet1.Cells(a, b + 5) = "silt loam"
    
    ElseIf Sheet1.Cells(a, b + 4) > 20 And Sheet1.Cells(a, b + 4) < 28 And Sheet1.Cells(a, b + 3) < 50 And Sheet1.Cells(a, b + 2) < 45 Then Sheet1.Cells(a, b + 5) = "silt loam"
    ElseIf Sheet1.Cells(a, b + 2) < 50 And Sheet1.Cells(a, b + 3) < 50 And Sheet1.Cells(a, b + 4) < 20 Then Sheet1.Cells(a, b + 5) = "loam"
    
    Else
    Sheet1.Cells(a, b + 5) = "?"
    
            
    End If
    
    a = a + 1
Next i
b = b + 4
Next j
End Sub

Private Sub CommandButton3_Click()



b = 0
For i = 1 To 5
a = 2
    For j = 1 To 1776
    
    
    Sheet2.Cells(a, 1) = Sheet1.Cells(a, 1)
    If Sheet1.Cells(a, b + 5) = "sand" Or Sheet1.Cells(a, b + 5) = "loam sand" Or Sheet1.Cells(a, b + 5) = "sand loam" Then Sheet2.Cells(a, 2 + i) = "A"
    If Sheet1.Cells(a, b + 5) = "silt loam" Or Sheet1.Cells(a, b + 5) = "loam" Then Sheet2.Cells(a, 2 + i) = "B"
    If Sheet1.Cells(a, b + 5) = "sand clay loam" Then Sheet2.Cells(a, 2 + i) = "C"
    If Sheet1.Cells(a, b + 5) = "clay loam" Or Sheet1.Cells(a, b + 5) = "silt clay loam" Or Sheet1.Cells(a, b + 5) = "sand clay" Or Sheet1.Cells(a, b + 5) = "silt clay" Or Sheet1.Cells(a, b + 5) = "clay" Then Sheet2.Cells(a, 2 + i) = "D"
    
    
    a = a + 1
    Next j
    b = b + 4
Next i

End Sub

Private Sub CommandButton4_Click()
H1 = 0
H2 = 0
H3 = 0
H4 = 0
a = 2
For i = 1 To 1776
H1 = 0
H2 = 0
H3 = 0
H4 = 0
    For j = 1 To 3
    If Sheet2.Cells(a, 2 + j) = "A" Then
        H1 = H1 + 1
    ElseIf Sheet2.Cells(a, 2 + j) = "B" Then H2 = H2 + 1
    ElseIf Sheet2.Cells(a, 2 + j) = "C" Then H3 = H3 + 1
    ElseIf Sheet2.Cells(a, 2 + j) = "D" Then H4 = H4 + 1
    End If
    Next j
    Sheet2.Cells(a, 6) = H1 / 0.03
    Sheet2.Cells(a, 7) = H2 / 0.03
    Sheet2.Cells(a, 8) = H3 / 0.03
    Sheet2.Cells(a, 9) = H4 / 0.03

    a = a + 1
Next i







End Sub
