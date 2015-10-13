VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} 计算多年的ET验证数据 
   Caption         =   "UserForm1"
   ClientHeight    =   3180
   ClientLeft      =   45
   ClientTop       =   375
   ClientWidth     =   4710
   OleObjectBlob   =   "计算多年的ET验证数据.frx":0000
   StartUpPosition =   1  '所有者中心
End
Attribute VB_Name = "计算多年的ET验证数据"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub CommandButton1_Click()

'补全验证数据 ET，GEP等


Dim i, j, h, HUC_num, y_start, y_end, HUC_start As Integer
Dim a, b As Double

HUC_num = InputBox("请输入流域单元数！", HUC_num)
y_start = InputBox("请输入起始年份！", y_start)
y_end = InputBox("请输入结束年份！", y_end)
'HUC_start = InputBox("请输入起始流域单元！", HUC_start)

HUC_start = 1

Sheet2.Cells(1, 1) = "HUC_8"
Sheet2.Cells(1, 2) = "YEAR"
Sheet2.Cells(1, 3) = "MONTH"
Sheet2.Cells(1, 4) = "ET_V"



a = 2
b = 2
For i = y_start To y_end
    For j = 1 To 12
        For h = HUC_start To HUC_num
           
            If (Sheet1.Cells(a, 1) = h) Then
                Sheet2.Cells(b, 1) = h
                Sheet2.Cells(b, 2) = i
                Sheet2.Cells(b, 3) = j
                Sheet2.Cells(b, 4) = Sheet1.Cells(a, 4)

                a = a + 1
                
            Else
                Sheet2.Cells(b, 1) = h
                Sheet2.Cells(b, 2) = i
                Sheet2.Cells(b, 3) = j
                Sheet2.Cells(b, 4) = -9999

            End If
            b = b + 1
        Next h
    Next j
Next i
Worksheets("sheet1").Activate
   Columns("A:A").Select
    ActiveWorkbook.Worksheets("Sheet1").Sort.SortFields.Clear
    ActiveWorkbook.Worksheets("Sheet1").Sort.SortFields.Add Key:=Range("A2:A1000000"), _
        SortOn:=xlSortOnValues, Order:=xlAscending, DataOption:=xlSortNormal
    ActiveWorkbook.Worksheets("Sheet1").Sort.SortFields.Add Key:=Range("B2:B1000000"), _
        SortOn:=xlSortOnValues, Order:=xlAscending, DataOption:=xlSortNormal
    ActiveWorkbook.Worksheets("Sheet1").Sort.SortFields.Add Key:=Range("C2:C1000000"), _
        SortOn:=xlSortOnValues, Order:=xlAscending, DataOption:=xlSortNormal
   
    With ActiveWorkbook.Worksheets("Sheet1").Sort
        .SetRange Range("A1:G1000000")
        .Header = xlYes
        .MatchCase = False
        .Orientation = xlTopToBottom
        .SortMethod = xlPinYin
        .Apply
    End With
ActiveWorkbook.Close SaveChanges:=True

End Sub
