VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} climate 
   Caption         =   "UserForm1"
   ClientHeight    =   5610
   ClientLeft      =   45
   ClientTop       =   375
   ClientWidth     =   7875
   OleObjectBlob   =   "climate.frx":0000
   StartUpPosition =   1  '所有者中心
End
Attribute VB_Name = "climate"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub CommandButton1_Click()
For I = 17426 To 60289

For j = 243 To 757

If Sheet1.Cells(I, 1) = Sheet2.Cells(j, 1) Then

Sheet1.Cells(I, 2) = Sheet2.Cells(j, 2)
Sheet1.Cells(I, 3) = Sheet2.Cells(j, 3)
Sheet1.Cells(I, 4) = Sheet2.Cells(j, 4)

End If

Next j


Next I
MsgBox "done!"
End Sub

Private Sub CommandButton2_Click()
I = 2
j = 2
For cell = 1 To 675

    For y = 1 To 72

          ' Temp
            Sheet4.Cells(I, 1) = Sheet1.Cells(j, 1)
            Sheet4.Cells(I, 2) = Sheet1.Cells(j, 2)
            Sheet4.Cells(I, 3) = Sheet1.Cells(j, 3)
            Sheet4.Cells(I, 4) = Sheet1.Cells(j, 4)
            Sheet4.Cells(I, y + 4) = Sheet1.Cells(j, 8) / 10
            'pre
            Sheet3.Cells(I, 1) = Sheet1.Cells(j, 1)
            Sheet3.Cells(I, 2) = Sheet1.Cells(j, 2)
            Sheet3.Cells(I, 3) = Sheet1.Cells(j, 3)
            Sheet3.Cells(I, 4) = Sheet1.Cells(j, 4)
            If Sheet1.Cells(j, 7) = 32766 Then
            Sheet3.Cells(I, y + 4) = Sheet3.Cells(I, y + 4 - 12)
            Else
            Sheet3.Cells(I, y + 4) = Sheet1.Cells(j, 7) / 10
            End If
            
            j = j + 1

    
    Next y
    I = I + 1
Next cell
MsgBox "done"


End Sub

Private Sub CommandButton3_Click()

For cell = 1 To 675
    For y = 2009 To 2014
    
        For m = 1 To 12
            b = 0
           
            
            Sheet1.Cells(a, 9) = "ok"
            a = a + 1
            Else
            
                For I = y To 2009 Step -1
                    For j = m To 1 Step -1
                    
                    Sheet1.Cells(a - b, 9) = "NA"
                    b = b + 1
                    
                    Next j
                    
                Next I
            Exit For
            
            End If
            
        Next m
    
    Next y
Next cell
MsgBox "done"

End Sub
