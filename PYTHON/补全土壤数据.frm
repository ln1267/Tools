VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} ��ȫ�������� 
   Caption         =   "UserForm1"
   ClientHeight    =   3180
   ClientLeft      =   45
   ClientTop       =   375
   ClientWidth     =   4710
   OleObjectBlob   =   "��ȫ��������.frx":0000
   StartUpPosition =   1  '����������
End
Attribute VB_Name = "��ȫ��������"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False


Private Sub CommandButton1_Click()
Dim i, j, HUC_num As Integer
Dim a, b As Double

a = 2
b = 2
Sheet2.Cells(1, 1) = "ID"
Sheet2.Cells(1, 2) = "Watershed"
        For j = 3 To 13
            Sheet2.Cells(1, j) = Sheet1.Cells(1, j)
        Next j
HUC_num = InputBox("��������������", HUC_num)

For i = 1 To HUC_num
    If (Sheet1.Cells(a, 2) = CStr(i)) Then
        Sheet2.Cells(b, 1) = i
        Sheet2.Cells(b, 2) = i
        For j = 3 To 13
            Sheet2.Cells(b, j) = Sheet1.Cells(a, j)
        Next j
        a = a + 1
    Else
        Sheet2.Cells(b, 1) = i
        Sheet2.Cells(b, 2) = i
        For j = 3 To 13
            Sheet2.Cells(b, j) = Sheet1.Cells(a - 1, j)
        Next j
       
    End If
    b = b + 1
Next i
ActiveWorkbook.Close SaveChanges:=True
End Sub
