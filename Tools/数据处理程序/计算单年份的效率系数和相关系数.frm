VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} 计算单年份的效率系数和相关系数 
   Caption         =   "UserForm1"
   ClientHeight    =   3120
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   4710
   OleObjectBlob   =   "计算单年份的效率系数和相关系数.frx":0000
   StartUpPosition =   1  '所有者中心
End
Attribute VB_Name = "计算单年份的效率系数和相关系数"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False





Private Sub CommandButton1_Click()

n = 12


Sheet1.Cells(1, 8) = "名称"
Sheet1.Cells(1, 9) = "value"
Sheet1.Cells(2, 8) = "RSQ_GEP"
Sheet1.Cells(3, 8) = "RSQ_GEP_2000"

Sheet1.Cells(10, 8) = "NS_GEP"
Sheet1.Cells(11, 8) = "NS_GEP_2000"

Sheet1.Cells(18, 8) = "MSE_GEP"
Sheet1.Cells(19, 8) = "MSE_GEP_2000"

'开始计算RSQ
Sheet1.Cells(2, 9) = "=RSQ(C:C,D:D)"
Sheet1.Cells(3, 9) = "=RSQ(C2:C13,D2:D13)"





'下面计算效率系数NS

a = 2
b = 2
num = n
sum1 = 0
sum2 = 0
For i = 1 To n
    sum1 = sum1 + (Sheet1.Cells(a, 3) - Sheet1.Cells(a, 4)) * (Sheet1.Cells(a, 3) - Sheet1.Cells(a, 4))
    Sum = Sum + Sheet1.Cells(a, 4)
    a = a + 1
Next i


For i = 1 To n
    sum2 = sum2 + (Sheet1.Cells(b, 4) - Sum / num) * (Sheet1.Cells(b, 4) - Sum / num)
    b = b + 1
Next i

ns = 1 - sum1 / sum2

Sheet1.Cells(10, 9) = ns
Sheet1.Cells(18, 9) = sum1 / n




'开始计算sheet2的GEP_G

Sheet2.Activate

n = 7

Sheet2.Cells(1, 8) = "名称"
Sheet2.Cells(1, 9) = "value"
Sheet2.Cells(2, 8) = "RSQ_GEP_G"
Sheet2.Cells(3, 8) = "RSQ_GEP_G_2000"

Sheet2.Cells(10, 8) = "NS_GEP_G"
Sheet2.Cells(11, 8) = "NS_GEP_G_2000"

Sheet2.Cells(18, 8) = "MSE_GEP_G"
Sheet2.Cells(19, 8) = "MSE_GEP_G_2000"


'开始计算RSQ
Sheet2.Cells(2, 9) = "=RSQ(C:C,D:D)"
Sheet2.Cells(3, 9) = "=RSQ(C2:C8,D2:D8)"





'下面计算效率系数NS

a = 2
b = 2
num = n
sum1 = 0
sum2 = 0
For i = 1 To n
    sum1 = sum1 + (Sheet2.Cells(a, 3) - Sheet2.Cells(a, 4)) * (Sheet2.Cells(a, 3) - Sheet2.Cells(a, 4))
    Sum = Sum + Sheet2.Cells(a, 4)
    a = a + 1
Next i


For i = 1 To n
    sum2 = sum2 + (Sheet2.Cells(b, 4) - Sum / num) * (Sheet2.Cells(b, 4) - Sum / num)
    b = b + 1
Next i

ns = 1 - sum1 / sum2

Sheet2.Cells(10, 9) = ns
Sheet2.Cells(18, 9) = sum1 / n




'begin Sheet3ET

Sheet3.Activate

n = 84
Sheet3.Cells(1, 8) = "??"
Sheet3.Cells(1, 9) = "value"
Sheet3.Cells(2, 8) = "RSQ_ET"
Sheet3.Cells(3, 8) = "RSQ_ET_2000"

Sheet3.Cells(10, 8) = "NS_ET"
Sheet3.Cells(11, 8) = "NS_ET_2000"

Sheet3.Cells(18, 8) = "MSE_ET"
Sheet3.Cells(19, 8) = "MSE_ET_2000"


'????RSQ
Sheet3.Cells(2, 9) = "=RSQ(D:D,F:F)"
Sheet3.Cells(3, 9) = "=RSQ(D2:D13,F2:F13)"





'????????NS

a = 2
b = 2
num = n
sum1 = 0
sum2 = 0
For i = 1 To n
    sum1 = sum1 + (Sheet3.Cells(a, 4) - Sheet3.Cells(a, 6)) * (Sheet3.Cells(a, 4) - Sheet3.Cells(a, 6))
    Sum = Sum + Sheet3.Cells(a, 6)
    a = a + 1
Next i


For i = 1 To n
    sum2 = sum2 + (Sheet3.Cells(b, 6) - Sum / num) * (Sheet3.Cells(b, 6) - Sum / num)
    b = b + 1
Next i

ns = 1 - sum1 / sum2

Sheet3.Cells(10, 9) = ns
Sheet3.Cells(18, 9) = sum1 / n



'开始计算Sheet4的ET_G

'begin Sheet4?ET_G

Sheet4.Activate

n = 49

Sheet4.Cells(1, 8) = "??"
Sheet4.Cells(1, 9) = "value"
Sheet4.Cells(2, 8) = "RSQ_ET_G"
Sheet4.Cells(3, 8) = "RSQ_ET_G_2000"


Sheet4.Cells(10, 8) = "NS_ET_G"
Sheet4.Cells(11, 8) = "NS_ET_G_2000"

Sheet4.Cells(18, 8) = "MSE_ET_G"
Sheet4.Cells(19, 8) = "MSE_ET_G_2000"

'????RSQ
Sheet4.Cells(2, 9) = "=RSQ(D:D,F:F)"
Sheet4.Cells(3, 9) = "=RSQ(D2:D8,F2:F8)"





'????????NS

a = 2
b = 2
num = n
sum1 = 0
sum2 = 0
For i = 1 To n
    sum1 = sum1 + (Sheet4.Cells(a, 4) - Sheet4.Cells(a, 6)) * (Sheet4.Cells(a, 4) - Sheet4.Cells(a, 6))
    Sum = Sum + Sheet4.Cells(a, 6)
    a = a + 1
Next i


For i = 1 To n
    sum2 = sum2 + (Sheet4.Cells(b, 6) - Sum / num) * (Sheet4.Cells(b, 6) - Sum / num)
    b = b + 1
Next i

ns = 1 - sum1 / sum2

Sheet4.Cells(10, 9) = ns
Sheet4.Cells(18, 9) = sum1 / n



'begin sheet5 FLOW
Sheet5.Activate
n = 84
Sheet5.Cells(1, 8) = "??"
Sheet5.Cells(1, 9) = "value"
Sheet5.Cells(2, 8) = "RSQ_FLOW"
Sheet5.Cells(3, 8) = "RSQ_FLOW_2000"

Sheet5.Cells(10, 8) = "NS_FLOW"
Sheet5.Cells(11, 8) = "NS_FLOW_2000"

Sheet5.Cells(18, 8) = "MSE_FLOW"
Sheet5.Cells(19, 8) = "MSE_FLOW_2000"


'????RSQ
Sheet5.Cells(2, 9) = "=RSQ(C:C,D:D)"
Sheet5.Cells(3, 9) = "=RSQ(C2:C13,D2:D13)"





'????????NS

a = 2
b = 2
num = n
sum1 = 0
sum2 = 0
For i = 1 To n
    sum1 = sum1 + (Sheet5.Cells(a, 3) - Sheet5.Cells(a, 4)) * (Sheet5.Cells(a, 3) - Sheet5.Cells(a, 4))
    Sum = Sum + Sheet5.Cells(a, 4)
    a = a + 1
Next i


For i = 1 To n
    sum2 = sum2 + (Sheet5.Cells(b, 4) - Sum / num) * (Sheet5.Cells(b, 4) - Sum / num)
    b = b + 1
Next i

ns = 1 - sum1 / sum2

Sheet5.Cells(10, 9) = ns
Sheet5.Cells(18, 9) = sum1 / n




'begin sheet6 FLOW_G

Sheet6.Activate

n = 49

Sheet6.Cells(1, 8) = "??"
Sheet6.Cells(1, 9) = "value"
Sheet6.Cells(2, 8) = "RSQ_FLOW_G"
Sheet6.Cells(3, 8) = "RSQ_FLOW_G_2000"

Sheet6.Cells(10, 8) = "NS_FLOW_G"
Sheet6.Cells(11, 8) = "NS_FLOW_G_2000"

Sheet6.Cells(18, 8) = "MSE_FLOW_G"
Sheet6.Cells(19, 8) = "MSE_FLOW_G_2000"


'????RSQ
Sheet6.Cells(2, 9) = "=RSQ(C:C,D:D)"
Sheet6.Cells(3, 9) = "=RSQ(C2:C8,D2:D8)"




'????????NS

a = 2
b = 2
num = n
sum1 = 0
sum2 = 0
For i = 1 To n
    sum1 = sum1 + (Sheet6.Cells(a, 3) - Sheet6.Cells(a, 4)) * (Sheet6.Cells(a, 3) - Sheet6.Cells(a, 4))
    Sum = Sum + Sheet6.Cells(a, 4)
    a = a + 1
Next i


For i = 1 To n
    sum2 = sum2 + (Sheet6.Cells(b, 4) - Sum / num) * (Sheet6.Cells(b, 4) - Sum / num)
    b = b + 1
Next i

ns = 1 - sum1 / sum2

Sheet6.Cells(10, 9) = ns
Sheet6.Cells(18, 9) = sum1 / n



a = 26

    For i = 1 To 24
    Sheet1.Cells(a, 8) = Sheet2.Cells(i + 1, 8)
    Sheet1.Cells(a, 9) = Sheet2.Cells(i + 1, 9)
    a = a + 1
    Next i

    For i = 1 To 24
    Sheet1.Cells(a, 8) = Sheet3.Cells(i + 1, 8)
    Sheet1.Cells(a, 9) = Sheet3.Cells(i + 1, 9)
    a = a + 1
    Next i
    
    For i = 1 To 24
    Sheet1.Cells(a, 8) = Sheet4.Cells(i + 1, 8)
    Sheet1.Cells(a, 9) = Sheet4.Cells(i + 1, 9)
    a = a + 1
    Next i
    
    For i = 1 To 24
    Sheet1.Cells(a, 8) = Sheet5.Cells(i + 1, 8)
    Sheet1.Cells(a, 9) = Sheet5.Cells(i + 1, 9)
    a = a + 1
    Next i
    
     For i = 1 To 24
    Sheet1.Cells(a, 8) = Sheet6.Cells(i + 1, 8)
    Sheet1.Cells(a, 9) = Sheet6.Cells(i + 1, 9)
    a = a + 1
    Next i
    Sheet1.Activate
    

ActiveWorkbook.Close SaveChanges:=True
End Sub


