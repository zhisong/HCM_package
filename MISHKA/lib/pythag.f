      real function pythag(a,b)
      real a,b
c
c     finds dsqrt(a**2+b**2) without overflow or destructive underflow
c
      real p,r,s,t,u
      p = max1(abs(a),abs(b))
      if (p .eq. 0.0d0) go to 20
      r = (dmin1(abs(a),abs(b))/p)**2
   10 continue
         t = 4.0d0 + r
         if (t .eq. 4.0d0) go to 20
         s = r/t
         u = 1.0d0 + 2.0d0*s
         p = u*p
         r = (s/u)**2 * r
      go to 10
   20 pythag = p
      return
      end
