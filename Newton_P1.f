      implicit double precision(a-h, o-z)
      parameter(initial_guess=5)
      x=initial_guess
      error=0.00001
      
      do 39 i=1,50
     	dx=-f(x)/g(x)
      	x=x+dx
       if (abs(dx).le.error) goto 23
       	solution=x
        
 39   continue
       
 23   write(*,*) solution, sqrt(5.0)
      stop
      end
      
      double precision function f(x)
      implicit double precision (a-h, o-z)
      f=x*x-5
      return
      end
      
      double precision function g(x)
      implicit double precision(a-h, o-z)
      g=2*x
      return
      end
