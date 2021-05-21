       implicit double precision(a-h, o-z)
       parameter (a=0,b=10,dx=0.1)
       x=a
c Find range of roots       
       do 39 while (x.le.b)
       	f1=f(x)
       	f2=f(x+dx)
       	if (f1*f2.le.0) then
       		sol1=x
       		sol2=x+dx
       	endif
       	x=x+dx
 39    continue
c Bisection Method       
       f1=f(sol1)
       f2=f(sol2)
       error=0.001
       
       do 1 i=1,100
       	if (abs((sol1-sol2)/sol1).le.error) go to 23
       	sol3=0.5*(sol1+sol2)
       	f3=f(sol3)
       	if (f2*f3.le.0) then
       		sol1=sol3
       		f1=f3
       	else
       		sol2=sol3
       		f2=f3
       	endif
 1     continue
 23    solution=(sol1+sol2)/2
       write(*,*)"Solution =", solution, i
	
       stop
       end
       
       double precision function f(x)
       implicit double precision(a-h, o-z)
       f=(x-5)*exp(x)+5
       return
       end              
