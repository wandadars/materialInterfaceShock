c
c
c     Solves normal shock-material interface problem
c     with ideal or stiffened gas
c
c     Thomas L. Jackson
c     June 2014
c
c     The material has subscript 00
c     The gas upstream of the material interface has subscripts
c       0 for unshocked gas
c       1 for shocked gas
c
c     After the shock has collided with the material interface,
c     the incident shock creates a transmitted shock that travels
c     through the material with speed u_ti, the material interface
c     moves with speed u2=ut, and there is a reflected wave
c     that can either be a shock or an expansion wave that
c     travels with speed u_ri. In the material the shocked state
c     has subscript t, while in the incident gas the region
c     between the reflected wave and the interface has subscript 2.
c     See notes for more information.
c
c     To compile and execute
c        make
c        ./material_interface
c
      implicit double precision (a-h,o-z)
      common /coeff1/ r00,c00,gt,v00
      common /coeff2/ r0,gi,v0
      common /coeff3/ p0,p1
      common /coeff4/ r1,v1,ui,u1
      common /coeff5/ p00_infty,p0_infty
      common /coeff6/ scale
      common /transmitted/ pt,rt,ut,uti
      common /refected/ r2,u2,uri
      external func
c
c
c     icase = 1, Hurricane and Miller, 1998
c           = 2, Polachek & Seeger Case 15
c           = 3, Polachek & Seeger Case 3
c           = 4, nitromethane and aluminum
c           > 4, see fit3_input.f
c
      write(6,*)'Input icase'
      read(5,*) icase
      call inputs(icase)
c
c
c     Step 1, determine incident wave values
c       subscript 1
c
      v1 = v00*((gi+1.0d0)*p0+(gi-1.0d0)*p1+2.0d0*gi*p00_infty)/
     &     ((gi+1.0d0)*p1+(gi-1.0d0)*p0+2.0d0*gi*p00_infty)
      r1 = 1.0d0/v1

      ui = v00*sqrt((p1-p0)/(v00-v1))	! incident shock speed

      u1 = ui*(v00-v1)/v00		! piston speed

      c00 = sqrt(gi*(p0+p00_infty)/r00)	! sound speed in unshocked gas
      c11 = sqrt(gt*(p0+p0_infty)/r0)	! sound speed in unshocked material
      rm0 = ui/c00
c
c     write input parameters
c
      write(76,*)
      write(76,*)'Input parameters:'
      write(76,*)
      write(76,*)'c0=',c00
      write(76,*)'gamma=',gi
      write(76,*)'p_infty=',p00_infty
      write(76,*)'rho0=',r00
      write(76,*)'p0=',p0
      write(76,*)'Mach # =',rm0
      write(76,*)'gam2=',gt
      write(76,*)'p_infty=',p0_infty
      write(76,*)'rho2=',r0
      write(76,*)'p2=',p0
      write(76,*)'Sound speed in region 2, c2=',c11



      c1=sqrt(gi*(p1+p00_infty)/r1)
      write(76,*)
      write(76,*)'Incident wave'
      write(76,*)
      write(76,*)'  pressure jump    = ',p1/p0
      write(76,*)'  density jump     = ',r1/r00
      write(76,*)'  density          = ',r1
      write(76,*)'  piston speed     = ',u1
      write(76,*)'  shock pres (Pa)  = ',p1
      write(76,*)'  shock speed      = ',ui
      write(76,*)'  shock Mach no.   = ',ui/c00
      write(76,*)'  Sound speed in region 1, c1=',c1
      write(76,*)
c
c
c     Step 2, compute states after incident shock
c       has hit the material boundary
c       Use Secant method to find solution
c
      p20 = p1*scale
      f0 = func(p20)
      write(76,*) p20,f0
      p21 = p20*0.99
      f1 = func(p21)
      write(76,*) p21,f1
      ict = 0
  10  continue
      p2new = p21 - f1*(p21-p20)/(f1-f0)
      f2 = func(p2new)
      write(76,*) ict,p2new,f2
      if (abs(f2).gt.1.0d-9) then
         ict = ict + 1
         if (ict.gt.50) goto 99
         p20 = p21
         p21 = p2new
         f0 = f1
         f1 = f2
         goto 10
      endif
      p2 = p2new

      write(76,*)
      write(76,*)'Transmitted values'
      write(76,*)'  pressure = ',pt
      write(76,*)'  density  = ',rt 
      write(76,*)'  velocity = ',ut 
      write(76,*)'  s. speed = ',uti

      write(76,*)
      write(76,*)'Reflected values'
      write(76,*)'  pressure = ',p2
      write(76,*)'  density  = ',r2 
      write(76,*)'  velocity = ',u2 
      write(76,*)'  s. speed = ',uri
c
c
c     compute wave impedance ratio
c
      di = (gi+1.0d0)*p1+(gi-1.0d0)*p0+2.0d0*gi*p00_infty
      zi2 = (p0/2.0d0/v00)*di
      Zi = sqrt(zi2)
      dt = (gt+1.0d0)*p2+(gt-1.0d0)*p0+2.0d0*gt*p0_infty
      zt2 = (p0/2.0d0/v0)*dt
      Zt = sqrt(zt2)
      write(76,*)
      write(76,*)'Impedance Zi = ',Zi
      write(76,*)'Impedance Zt = ',Zt
      write(76,*)'Impedance Ratio = ',Zi/Zt
      write(6,*)'Impedance Zi = ',Zi
      write(6,*)'Impedance Zt = ',Zt
      write(6,*)'Impedance Ratio = ',Zi/Zt
      if (Zi/Zt.lt.1.0d0) write(76,*)'Reflected wave is a shock'
      if (Zi/Zt.gt.1.0d0) write(76,*)'Reflected wave is an expansion'
      if (Zi.eq.Zt) write(76,*)'No reflected wave'
      if (Zi/Zt.lt.1.0d0) write(6,*)'Reflected wave is a shock'
      if (Zi/Zt.gt.1.0d0) write(6,*)'Reflected wave is an expansion'
      if (Zi.eq.Zt) write(6,*)'No reflected wave'
      write(76,*)
c
c     compute shock speed ratio
c
      capU = uti/ui
      write(76,*)'Shock speed ratio uti/ui = ',capU
      write(6,*)'Shock speed ratio uti/ui = ',capU
      if (capU.lt.1.0d0) write(76,*)'Transmitted shock is concave'
      if (capU.gt.1.0d0) write(76,*)'Transmitted shock is convex'
      if (capU.lt.1.0d0) write(6,*)'Transmitted shock is concave'
      if (capU.gt.1.0d0) write(6,*)'Transmitted shock is convex'

  99  continue
      if (ict.gt.20) write(76,*)'Not converged' 
c
      stop
      end
c
c
c
c
c
      double precision function func(p2)
      implicit double precision (a-h,o-z)
      common /coeff1/ r00,c00,gt,v00
      common /coeff2/ r0,gi,v0
      common /coeff3/ p0,p1
      common /coeff4/ r1,v1,ui,u1
      common /coeff5/ p00_infty,p0_infty
      common /transmitted/ pt,rt,ut,uti
      common /refected/ r2,u2,uri
c
      v2 = v1*((gi-1.0d0)*p2+(gi+1.0d0)*p1+2.0d0*gi*p00_infty)/
     &      ((gi-1.0d0)*p1+(gi+1.0d0)*p2+2.0d0*gi*p00_infty)
      r2 = 1.0d0/v2

      vt = v0*((gt+1.0d0)*p0+(gt-1.0d0)*p2+2.0d0*gt*p0_infty)/
     &      ((gt+1.0d0)*p2+(gt-1.0d0)*p0+2.0d0*gt*p0_infty)
      rt = 1.0d0/vt

      uti = v0*sqrt((p2-p0)/(v0-vt))

      u2 = uti*(1.0d0-vt/v0)

      uri = (u1*v2-v1*u2)/(v1-v2)

c     func = (p2-p1)/(r1*(u2-u1)*(u1+uri))+1.0d0
      func = (r1*(u2-u1)*(u1+uri))/(p2-p1)+1.0d0

      pt = p2
      ut = u2
c
      return
      end
