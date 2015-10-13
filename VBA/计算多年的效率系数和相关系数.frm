VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} 计算多年的效率系数和相关系数 
   Caption         =   "UserForm1"
   ClientHeight    =   3120
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   4710
   OleObjectBlob   =   "计算多年的效率系数和相关系数.frx":0000
   StartUpPosition =   1  '所有者中心
End
Attribute VB_Name = "计算多年的效率系数和相关系数"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub CommandButton1_Click()

'计算00-06年Zagunao 流域的验证效率系数和相关系数
n = 84   ' 待计算的行数


Sheet1.Cells(1, 8) = "名称"
Sheet1.Cells(1, 9) = "value"
Sheet1.Cells(2, 8) = "RSQ_GEP"
Sheet1.Cells(3, 8) = "RSQ_GEP_2000"
Sheet1.Cells(4, 8) = "RSQ_GEP_2001"
Sheet1.Cells(5, 8) = "RSQ_GEP_2002"
Sheet1.Cells(6, 8) = "RSQ_GEP_2003"
Sheet1.Cells(7, 8) = "RSQ_GEP_2004"
Sheet1.Cells(8, 8) = "RSQ_GEP_2005"
Sheet1.Cells(9, 8) = "RSQ_GEP_2006"
Sheet1.Cells(10, 8) = "NS_GEP"
Sheet1.Cells(11, 8) = "NS_GEP_2000"
Sheet1.Cells(12, 8) = "NS_GEP_2001"
Sheet1.Cells(13, 8) = "NS_GEP_2002"
Sheet1.Cells(14, 8) = "NS_GEP_2003"
Sheet1.Cells(15, 8) = "NS_GEP_2004"
Sheet1.Cells(16, 8) = "NS_GEP_2005"
Sheet1.Cells(17, 8) = "NS_GEP_2006"
Sheet1.Cells(18, 8) = "MSE_GEP"
Sheet1.Cells(19, 8) = "MSE_GEP_2000"
Sheet1.Cells(20, 8) = "MSE_GEP_2001"
Sheet1.Cells(21, 8) = "MSE_GEP_2002"
Sheet1.Cells(22, 8) = "MSE_GEP_2003"
Sheet1.Cells(23, 8) = "MSE_GEP_2004"
Sheet1.Cells(24, 8) = "MSE_GEP_2005"
Sheet1.Cells(25, 8) = "MSE_GEP_2006"

'开始计算RSQ
Sheet1.Cells(2, 9) = "=RSQ(C:C,D:D)"
Sheet1.Cells(3, 9) = "=RSQ(C2:C13,D2:D13)"
Sheet1.Cells(4, 9) = "=RSQ(C14:C25,D14:D25)"
Sheet1.Cells(5, 9) = "=RSQ(C26:C37,D26:D37)"
Sheet1.Cells(6, 9) = "=RSQ(C38:C49,D38:D49)"
Sheet1.Cells(7, 9) = "=RSQ(C50:C61,D50:D61)"
Sheet1.Cells(8, 9) = "=RSQ(C62:C73,D62:D73)"
Sheet1.Cells(9, 9) = "=RSQ(C74:C85,D74:D85)"




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

a = 2
b = 2
n = n / 7
num = n
For m = 1 To 7
sum1 = 0
Sum = 0
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

Sheet1.Cells(10 + m, 9) = ns
Sheet1.Cells(18 + m, 9) = sum1 / n
Next m


'开始计算sheet2的GEP_G

Sheet2.Activate

n = 49

Sheet2.Cells(1, 8) = "名称"
Sheet2.Cells(1, 9) = "value"
Sheet2.Cells(2, 8) = "RSQ_GEP_G"
Sheet2.Cells(3, 8) = "RSQ_GEP_G_2000"
Sheet2.Cells(4, 8) = "RSQ_GEP_G_2001"
Sheet2.Cells(5, 8) = "RSQ_GEP_G_2002"
Sheet2.Cells(6, 8) = "RSQ_GEP_G_2003"
Sheet2.Cells(7, 8) = "RSQ_GEP_G_2004"
Sheet2.Cells(8, 8) = "RSQ_GEP_G_2005"
Sheet2.Cells(9, 8) = "RSQ_GEP_G_2006"
Sheet2.Cells(10, 8) = "NS_GEP_G"
Sheet2.Cells(11, 8) = "NS_GEP_G_2000"
Sheet2.Cells(12, 8) = "NS_GEP_G_2001"
Sheet2.Cells(13, 8) = "NS_GEP_G_2002"
Sheet2.Cells(14, 8) = "NS_GEP_G_2003"
Sheet2.Cells(15, 8) = "NS_GEP_G_2004"
Sheet2.Cells(16, 8) = "NS_GEP_G_2005"
Sheet2.Cells(17, 8) = "NS_GEP_G_2006"
Sheet2.Cells(18, 8) = "MSE_GEP_G"
Sheet2.Cells(19, 8) = "MSE_GEP_G_2000"
Sheet2.Cells(20, 8) = "MSE_GEP_G_2001"
Sheet2.Cells(21, 8) = "MSE_GEP_G_2002"
Sheet2.Cells(22, 8) = "MSE_GEP_G_2003"
Sheet2.Cells(23, 8) = "MSE_GEP_G_2004"
Sheet2.Cells(24, 8) = "MSE_GEP_G_2005"
Sheet2.Cells(25, 8) = "MSE_GEP_G_2006"

'开始计算RSQ
Sheet2.Cells(2, 9) = "=RSQ(C:C,D:D)"
Sheet2.Cells(3, 9) = "=RSQ(C2:C8,D2:D8)"
Sheet2.Cells(4, 9) = "=RSQ(C9:C15,D9:D15)"
Sheet2.Cells(5, 9) = "=RSQ(C16:C22,D16:D22)"
Sheet2.Cells(6, 9) = "=RSQ(C23:C29,D23:D29)"
Sheet2.Cells(7, 9) = "=RSQ(C30:C36,D30:D36)"
Sheet2.Cells(8, 9) = "=RSQ(C37:C43,D37:D43)"
Sheet2.Cells(9, 9) = "=RSQ(C44:C50,D44:D50)"




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

a = 2
b = 2
n = n / 7
num = n
For m = 1 To 7
sum1 = 0
Sum = 0
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

Sheet2.Cells(10 + m, 9) = ns
Sheet2.Cells(18 + m, 9) = sum1 / n
Next m


'begin Sheet3ET

Sheet3.Activate

n = 84
Sheet3.Cells(1, 8) = "??"
Sheet3.Cells(1, 9) = "value"
Sheet3.Cells(2, 8) = "RSQ_ET"
Sheet3.Cells(3, 8) = "RSQ_ET_2000"
Sheet3.Cells(4, 8) = "RSQ_ET_2001"
Sheet3.Cells(5, 8) = "RSQ_ET_2002"
Sheet3.Cells(6, 8) = "RSQ_ET_2003"
Sheet3.Cells(7, 8) = "RSQ_ET_2004"
Sheet3.Cells(8, 8) = "RSQ_ET_2005"
Sheet3.Cells(9, 8) = "RSQ_ET_2006"
Sheet3.Cells(10, 8) = "NS_ET"
Sheet3.Cells(11, 8) = "NS_ET_2000"
Sheet3.Cells(12, 8) = "NS_ET_2001"
Sheet3.Cells(13, 8) = "NS_ET_2002"
Sheet3.Cells(14, 8) = "NS_ET_2003"
Sheet3.Cells(15, 8) = "NS_ET_2004"
Sheet3.Cells(16, 8) = "NS_ET_2005"
Sheet3.Cells(17, 8) = "NS_ET_2006"
Sheet3.Cells(18, 8) = "MSE_ET"
Sheet3.Cells(19, 8) = "MSE_ET_2000"
Sheet3.Cells(20, 8) = "MSE_ET_2001"
Sheet3.Cells(21, 8) = "MSE_ET_2002"
Sheet3.Cells(22, 8) = "MSE_ET_2003"
Sheet3.Cells(23, 8) = "MSE_ET_2004"
Sheet3.Cells(24, 8) = "MSE_ET_2005"
Sheet3.Cells(25, 8) = "MSE_ET_2006"

'????RSQ
Sheet3.Cells(2, 9) = "=RSQ(D:D,F:F)"
Sheet3.Cells(3, 9) = "=RSQ(D2:D13,F2:F13)"
Sheet3.Cells(4, 9) = "=RSQ(D14:D25,F14:F25)"
Sheet3.Cells(5, 9) = "=RSQ(D26:D37,F26:F37)"
Sheet3.Cells(6, 9) = "=RSQ(D38:D49,F38:F49)"
Sheet3.Cells(7, 9) = "=RSQ(D50:D61,F50:F61)"
Sheet3.Cells(8, 9) = "=RSQ(D62:D73,F62:F73)"
Sheet3.Cells(9, 9) = "=RSQ(D74:D85,F74:F85)"




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

a = 2
b = 2
n = n / 7
num = n
For m = 1 To 7
sum1 = 0
Sum = 0
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

Sheet3.Cells(10 + m, 9) = ns
Sheet3.Cells(18 + m, 9) = sum1 / n
Next m

'开始计算Sheet4的ET_G

'begin Sheet4?ET_G

Sheet4.Activate

n = 49

Sheet4.Cells(1, 8) = "??"
Sheet4.Cells(1, 9) = "value"
Sheet4.Cells(2, 8) = "RSQ_ET_G"
Sheet4.Cells(3, 8) = "RSQ_ET_G_2000"
Sheet4.Cells(4, 8) = "RSQ_ET_G_2001"
Sheet4.Cells(5, 8) = "RSQ_ET_G_2002"
Sheet4.Cells(6, 8) = "RSQ_ET_G_2003"
Sheet4.Cells(7, 8) = "RSQ_ET_G_2004"
Sheet4.Cells(8, 8) = "RSQ_ET_G_2005"
Sheet4.Cells(9, 8) = "RSQ_ET_G_2006"
Sheet4.Cells(10, 8) = "NS_ET_G"
Sheet4.Cells(11, 8) = "NS_ET_G_2000"
Sheet4.Cells(12, 8) = "NS_ET_G_2001"
Sheet4.Cells(13, 8) = "NS_ET_G_2002"
Sheet4.Cells(14, 8) = "NS_ET_G_2003"
Sheet4.Cells(15, 8) = "NS_ET_G_2004"
Sheet4.Cells(16, 8) = "NS_ET_G_2005"
Sheet4.Cells(17, 8) = "NS_ET_G_2006"
Sheet4.Cells(18, 8) = "MSE_ET_G"
Sheet4.Cells(19, 8) = "MSE_ET_G_2000"
Sheet4.Cells(20, 8) = "MSE_ET_G_2001"
Sheet4.Cells(21, 8) = "MSE_ET_G_2002"
Sheet4.Cells(22, 8) = "MSE_ET_G_2003"
Sheet4.Cells(23, 8) = "MSE_ET_G_2004"
Sheet4.Cells(24, 8) = "MSE_ET_G_2005"
Sheet4.Cells(25, 8) = "MSE_ET_G_2006"

'????RSQ
Sheet4.Cells(2, 9) = "=RSQ(D:D,F:F)"
Sheet4.Cells(3, 9) = "=RSQ(D2:D8,F2:F8)"
Sheet4.Cells(4, 9) = "=RSQ(D9:D15,F9:F15)"
Sheet4.Cells(5, 9) = "=RSQ(D16:D22,F16:F22)"
Sheet4.Cells(6, 9) = "=RSQ(D23:D29,F23:F29)"
Sheet4.Cells(7, 9) = "=RSQ(D30:D36,F30:F36)"
Sheet4.Cells(8, 9) = "=RSQ(D37:D43,F37:F43)"
Sheet4.Cells(9, 9) = "=RSQ(D44:D50,F44:F50)"




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

a = 2
b = 2
n = n / 7
num = n
For m = 1 To 7
sum1 = 0
Sum = 0
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

Sheet4.Cells(10 + m, 9) = ns
Sheet4.Cells(18 + m, 9) = sum1 / n
Next m

'begin sheet5 FLOW
Sheet5.Activate
n = 84
Sheet5.Cells(1, 8) = "??"
Sheet5.Cells(1, 9) = "value"
Sheet5.Cells(2, 8) = "RSQ_FLOW"
Sheet5.Cells(3, 8) = "RSQ_FLOW_2000"
Sheet5.Cells(4, 8) = "RSQ_FLOW_2001"
Sheet5.Cells(5, 8) = "RSQ_FLOW_2002"
Sheet5.Cells(6, 8) = "RSQ_FLOW_2003"
Sheet5.Cells(7, 8) = "RSQ_FLOW_2004"
Sheet5.Cells(8, 8) = "RSQ_FLOW_2005"
Sheet5.Cells(9, 8) = "RSQ_FLOW_2006"
Sheet5.Cells(10, 8) = "NS_FLOW"
Sheet5.Cells(11, 8) = "NS_FLOW_2000"
Sheet5.Cells(12, 8) = "NS_FLOW_2001"
Sheet5.Cells(13, 8) = "NS_FLOW_2002"
Sheet5.Cells(14, 8) = "NS_FLOW_2003"
Sheet5.Cells(15, 8) = "NS_FLOW_2004"
Sheet5.Cells(16, 8) = "NS_FLOW_2005"
Sheet5.Cells(17, 8) = "NS_FLOW_2006"
Sheet5.Cells(18, 8) = "MSE_FLOW"
Sheet5.Cells(19, 8) = "MSE_FLOW_2000"
Sheet5.Cells(20, 8) = "MSE_FLOW_2001"
Sheet5.Cells(21, 8) = "MSE_FLOW_2002"
Sheet5.Cells(22, 8) = "MSE_FLOW_2003"
Sheet5.Cells(23, 8) = "MSE_FLOW_2004"
Sheet5.Cells(24, 8) = "MSE_FLOW_2005"
Sheet5.Cells(25, 8) = "MSE_FLOW_2006"

'????RSQ
Sheet5.Cells(2, 9) = "=RSQ(C:C,D:D)"
Sheet5.Cells(3, 9) = "=RSQ(C2:C13,D2:D13)"
Sheet5.Cells(4, 9) = "=RSQ(C14:C25,D14:D25)"
Sheet5.Cells(5, 9) = "=RSQ(C26:C37,D26:D37)"
Sheet5.Cells(6, 9) = "=RSQ(C38:C49,D38:D49)"
Sheet5.Cells(7, 9) = "=RSQ(C50:C61,D50:D61)"
Sheet5.Cells(8, 9) = "=RSQ(C62:C73,D62:D73)"
Sheet5.Cells(9, 9) = "=RSQ(C74:C85,D74:D85)"




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

a = 2
b = 2
n = n / 7
num = n
For m = 1 To 7
sum1 = 0
Sum = 0
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

Sheet5.Cells(10 + m, 9) = ns
Sheet5.Cells(18 + m, 9) = sum1 / n
Next m


'begin sheet6 FLOW_G

Sheet6.Activate

n = 49

Sheet6.Cells(1, 8) = "??"
Sheet6.Cells(1, 9) = "value"
Sheet6.Cells(2, 8) = "RSQ_FLOW_G"
Sheet6.Cells(3, 8) = "RSQ_FLOW_G_2000"
Sheet6.Cells(4, 8) = "RSQ_FLOW_G_2001"
Sheet6.Cells(5, 8) = "RSQ_FLOW_G_2002"
Sheet6.Cells(6, 8) = "RSQ_FLOW_G_2003"
Sheet6.Cells(7, 8) = "RSQ_FLOW_G_2004"
Sheet6.Cells(8, 8) = "RSQ_FLOW_G_2005"
Sheet6.Cells(9, 8) = "RSQ_FLOW_G_2006"
Sheet6.Cells(10, 8) = "NS_FLOW_G"
Sheet6.Cells(11, 8) = "NS_FLOW_G_2000"
Sheet6.Cells(12, 8) = "NS_FLOW_G_2001"
Sheet6.Cells(13, 8) = "NS_FLOW_G_2002"
Sheet6.Cells(14, 8) = "NS_FLOW_G_2003"
Sheet6.Cells(15, 8) = "NS_FLOW_G_2004"
Sheet6.Cells(16, 8) = "NS_FLOW_G_2005"
Sheet6.Cells(17, 8) = "NS_FLOW_G_2006"
Sheet6.Cells(18, 8) = "MSE_FLOW_G"
Sheet6.Cells(19, 8) = "MSE_FLOW_G_2000"
Sheet6.Cells(20, 8) = "MSE_FLOW_G_2001"
Sheet6.Cells(21, 8) = "MSE_FLOW_G_2002"
Sheet6.Cells(22, 8) = "MSE_FLOW_G_2003"
Sheet6.Cells(23, 8) = "MSE_FLOW_G_2004"
Sheet6.Cells(24, 8) = "MSE_FLOW_G_2005"
Sheet6.Cells(25, 8) = "MSE_FLOW_G_2006"

'????RSQ
Sheet6.Cells(2, 9) = "=RSQ(C:C,D:D)"
Sheet6.Cells(3, 9) = "=RSQ(C2:C8,D2:D8)"
Sheet6.Cells(4, 9) = "=RSQ(C9:C15,D9:D15)"
Sheet6.Cells(5, 9) = "=RSQ(C16:C22,D16:D22)"
Sheet6.Cells(6, 9) = "=RSQ(C23:C29,D23:D29)"
Sheet6.Cells(7, 9) = "=RSQ(C30:C36,D30:D36)"
Sheet6.Cells(8, 9) = "=RSQ(C37:C43,D37:D43)"
Sheet6.Cells(9, 9) = "=RSQ(C44:C50,D44:D50)"




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

a = 2
b = 2
n = n / 7
num = n
For m = 1 To 7
sum1 = 0
Sum = 0
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

Sheet6.Cells(10 + m, 9) = ns
Sheet6.Cells(18 + m, 9) = sum1 / n
Next m

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


