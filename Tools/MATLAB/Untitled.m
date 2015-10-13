a = rand(3,4,3);
a;
clear
a=gallery('integerdata',10,[2,3,4],1)
b=mean(a)
c=mean(a,1)
d=mean(a,2)
e=mean(a,3)