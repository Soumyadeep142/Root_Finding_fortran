       implicit double precision(a-h, o-z)
       parameter (a=0,b=1.5,dx=0.1)
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
 
       x0=sol1
       x1=sol2
       error=0.000001
       do 1 i=1,100
       	write(*,*)x0,x1 
       	f0=f(x0)
       	f1=f(x1)
       	if (abs(f1).le.error) go to 23
       		diff=-f1*(x1-x0)/(f1-f0)
       		x2=x1+diff
       		x0=x1
       		x1=x2

 1     continue
 23    solution=x1
       write(*,*)'solution=', solution, i
       
       stop 
       end

       
       double precision function f(x)
       implicit double precision(a-h, o-z)
       f=x**3-3*x**2+x+1
       return
       end              
