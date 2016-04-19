c
c
      subroutine inputs(icase)
      implicit double precision (a-h,o-z)
      common /coeff1/ r00,c00,gt,v00
      common /coeff2/ r0,gi,v0
      common /coeff3/ p0,p1
      common /coeff4/ r1,v1,ui,u1
      common /coeff5/ p00_infty,p0_infty
      common /coeff6/ scale
      common /transmitted/ pt,rt,ut,uti
      common /refected/ r2,u2,uri
c
c
      if (icase.eq.1) then	! Hurricane and Miller, 1998 
c
c        material 2 properties (region 2 of H&M, SF6)
c          subscript 0
c   
         r0 = 1.31d-3
         v0 = 1.0d0/r0
         gt = 1.2d0
         p0_infty = 0.0d0
c
c        material 1 properties (region 0 of H&M, air)
c          subscript 00
c
         r00 = 2.66d-4
         v00 = 1.0d0/r00
         gi = 1.4d0
         p00_infty = 0.0d0
         c00 = 0.0348d0
         p0 = r00*c00*c00/gi
c
         p1 = 4.5d0*p0
         scale = 1.8
         open(unit=76,file='case01.dat')
         write(76,*)'*** Hurricane & Miller Example'
c
      endif
c
c
      if (icase.eq.2) then	! Case 15 of Polachek & Seeger, 1951
c
c        material 2 properties (Oxygen)
c          subscript 0
c   
         r0 = 1.4290d0	! kg/m^3
         v0 = 1.0d0/r0
         gt = 1.4d0
         p0_infty = 0.0d0
c
c        material 1 properties (Nitrogen)
c          subscript 00
c
         r00 = 1.2506d0	! kg/m^3
         v00 = 1.0d0/r00
         gi = 1.4d0
         p00_infty = 0.0d0
         c00 = 353.0d0	! m/s
         p0 = r00*c00*c00/gi
c
         p1 = 2.5d0*p0
         scale = 1.2
         open(unit=76,file='case02.dat')
         write(76,*)'Polachek & Seeger Case 15'
         write(76,*)'*** Nitrogen-Oxygen'
         write(6,*)'Polachek & Seeger Case 15'
         write(6,*)'*** Nitrogen-Oxygen'
c
      endif
c
      if (icase.eq.3) then	! Case 3 of Polachek & Seeger, 1951
c
c        material 2 properties (Nitrogen)
c          subscript 0
c   
         r0 = 1.2506
         v0 = 1.0d0/r0
         gt = 1.4d0
         p0_infty = 0.0d0
c
c        material 1 properties (Oxygen)
c          subscript 00
c
         r00 = 1.4290d0
         v00 = 1.0d0/r00
         gi = 1.4d0
         p00_infty = 0.0d0
         c00 = 330.0d0
         p0 = r00*c00*c00/gi
c
         p1 = 2.5d0*p0
         scale = 0.9
         open(unit=76,file='case03.dat')
         write(76,*)'Polachek & Seeger Case 3'
         write(76,*)'*** Oxygen-Nitrogen'
         write(6,*)'Polachek & Seeger Case 3'
         write(6,*)'*** Oxygen-Nitrogen'
c
      endif
c
      if (icase.eq.4) then	! Nitromethane and aluminum
c
c        material 2 properties (aluminum)
c          subscript 0
c   
         r0 = 2784.0d0
         v0 = 1.0d0/r0
         gt = 3.8d0
         p0_infty = 21.127d9
c
c        material 1 properties (nitromethane)
c          subscript 00
c
         r00 = 982.0d0
         v00 = 1.0d0/r00
         gi = 4.1d0
         p00_infty = 0.6496d9
         p0 = 1.0d5
         c00 = sqrt(gi*(p0+p00_infty)/r00)
c
         p1 = 1.0d9
         scale = 1.9
         open(unit=76,file='case04.dat')
         write(76,*)'Nitromethane & aluminum'
         write(6,*)'Nitromethane & aluminum'
c
      endif
c
      if (icase.eq.5) then	! HMX(1) and aluminum
c
c        material 2 properties (aluminum)
c          subscript 0
c   
         r0 = 2784.0d0
         v0 = 1.0d0/r0
         gt = 3.8d0
         p0_infty = 21.127d9
c
c        material 1 properties (HMX)
c          subscript 00
c
         r00 = 1900.0d0
         v00 = 1.0d0/r00
         gi = 7.2d0
         p00_infty = 1.9811d9
         p0 = 1.0d5
         c00 = sqrt(gi*(p0+p00_infty)/r00)
c
         p1 = 1.0d9
         scale = 1.9
         open(unit=76,file='case05.dat')
         write(76,*)'HMX(1) & aluminum'
         write(6,*)'HMX(1) & aluminum'
c
      endif
c
      if (icase.eq.6) then	! HMX(2) and aluminum
c
c        material 2 properties (aluminum)
c          subscript 0
c   
         r0 = 2784.0d0
         v0 = 1.0d0/r0
         gt = 3.8d0
         p0_infty = 21.127d9
c
c        material 1 properties (HMX)
c          subscript 00
c
         r00 = 1891.0d0
         v00 = 1.0d0/r00
         gi = 5.1d0
         p00_infty = 3.495d9
         p0 = 1.0d5
         c00 = sqrt(gi*(p0+p00_infty)/r00)
c
         p1 = 1.0d9
         scale = 1.9
         open(unit=76,file='case06.dat')
         write(76,*)'HMX(2) & aluminum'
         write(6,*)'HMX(2) & aluminum'
c
      endif
c
c
      if (icase.eq.7) then	! AP and aluminum
c
c        material 2 properties (aluminum)
c          subscript 0
c   
         r0 = 2784.0d0
         v0 = 1.0d0/r0
         gt = 3.8d0
         p0_infty = 21.127d9
c
c        material 1 properties (AP)
c          subscript 00
c
         r00 = 1950.0d0
         v00 = 1.0d0/r00
         gi = 5.3d0
         p00_infty = 2.967d9
         p0 = 1.0d5
         c00 = sqrt(gi*(p0+p00_infty)/r00)
c
         p1 = 1.0d9
         scale = 1.9
         open(unit=76,file='case07.dat')
         write(76,*)'AP & aluminum'
         write(6,*)'AP & aluminum'
c
      endif
c
      if (icase.eq.8) then	! Estane and aluminum
c
c        material 2 properties (aluminum)
c          subscript 0
c   
         r0 = 2784.0d0
         v0 = 1.0d0/r0
         gt = 3.8d0
         p0_infty = 21.127d9
c
c        material 1 properties (Estane)
c          subscript 00
c
         r00 = 1200.0d0
         v00 = 1.0d0/r00
         gi = 4.6d0
         p00_infty = 1.404d9
         p0 = 1.0d5
         c00 = sqrt(gi*(p0+p00_infty)/r00)
c
         p1 = 1.0d9
         scale = 1.9
         open(unit=76,file='case08.dat')
         write(76,*)'Estane & aluminum'
         write(6,*)'Estane & aluminum'
c
      endif
c
      if (icase.eq.9) then	! HTPB and aluminum
c
c        material 2 properties (aluminum)
c          subscript 0
c   
         r0 = 2784.0d0
         v0 = 1.0d0/r0
         gt = 3.8d0
         p0_infty = 21.127d9
c
c        material 1 properties (HTPB)
c          subscript 00
c
         r00 = 900.0d0
         v00 = 1.0d0/r00
         gi = 5.4d0
         p00_infty = 0.4537d9
         p0 = 1.0d5
         c00 = sqrt(gi*(p0+p00_infty)/r00)
c
         p1 = 1.0d9
         scale = 1.9
         open(unit=76,file='case09.dat')
         write(76,*)'HTPB & aluminum'
         write(6,*)'HTPB & aluminum'
c
      endif
c
      if (icase.eq.10) then	! PETN and aluminum
c
c        material 2 properties (aluminum)
c          subscript 0
c   
         r0 = 2784.0d0
         v0 = 1.0d0/r0
         gt = 3.8d0
         p0_infty = 21.127d9
c
c        material 1 properties (PETN)
c          subscript 00
c
         r00 = 1770.0d0
         v00 = 1.0d0/r00
         gi = 5.2d0
         p00_infty = 1.993d9
         p0 = 1.0d5
         c00 = sqrt(gi*(p0+p00_infty)/r00)
c
         p1 = 1.0d9
         scale = 1.9
         open(unit=76,file='case10.dat')
         write(76,*)'PETN & aluminum'
         write(6,*)'PETN & aluminum'
c
      endif
c
      if (icase.eq.11) then	! aluminum and Nitromethane
c
c
c        material 2 properties (nitromethane)
c          subscript 0
c
         r0 = 982.0d0
         v0 = 1.0d0/r0
         gt = 4.1d0
         p0_infty = 0.6496d9
         p0 = 1.0d5
c
c        material 1 properties (aluminum)
c          subscript 00
c   
         r00 = 2784.0d0
         v00 = 1.0d0/r00
         gi = 3.8d0
         p00_infty = 21.127d9
         c00 = sqrt(gi*(p0+p00_infty)/r00)
c
         p1 = 1.0d9
         scale = 0.6
         open(unit=76,file='case11.dat')
         write(76,*)'Aluminum & Nitromethane'
         write(6,*)'Aluminum & Nitromethane'
c
      endif
c
      if (icase.eq.12) then	! air and magnesium
c
c        material 2 properties (magnesium)
c          subscript 0
c   
         r0 = 1775.0d0
         v0 = 1.0d0/r0
         gt = 3.4d0
         p0_infty = 10.903d9
         p0 = 1.01325d5
         c0 = sqrt(gt*(p0+p0_infty)/r0)
c
c        material 1 properties (air)
c          subscript 0
c
         write(6,*)'Input Mach number'
         read(5,*) rmach
         R = 287.04
         T0 = 293.15
         r00 = p0/(R*T0)
         v00 = 1.0d0/r00
         gi = 1.4d0
         p00_infty = 0.0
         c00 = sqrt(gi*(p0+p00_infty)/r00)
c
         p2p1 = 1.0 + 2.0*gi*(rmach**2-1.0)/(gi+1.0)
         p1 = p0*p2p1
         write(6,*)'p1=', p1
         scale = 1.8
         open(unit=76,file='case12.dat')
         write(76,*)'Air & Magnesium'
         write(6,*)'Air & Magnesium'
c
      endif
c
      if (icase.eq.13) then	! air and aluminum
c
c        material 2 properties (aluminum)
c          subscript 0
c   
         r0 = 2784.0d0
         v0 = 1.0d0/r0
         gt = 3.8d0
         p0_infty = 21.127d9
         p0 = 1.01325d5
         c0 = sqrt(gt*(p0+p0_infty)/r0)
c
c        material 1 properties (air)
c          subscript 0
c
         write(6,*)'Input Mach number'
         read(5,*) rmach
         R = 287.04
         T0 = 293.15
         r00 = p0/(R*T0)
         v00 = 1.0d0/r00
         gi = 1.4d0
         p00_infty = 0.0
         c00 = sqrt(gi*(p0+p00_infty)/r00)
c
         p2p1 = 1.0 + 2.0*gi*(rmach**2-1.0)/(gi+1.0)
         p1 = p0*p2p1
         write(6,*)'p1=', p1
         scale = 1.8
         open(unit=76,file='case13.dat')
         write(76,*)'Air & Aluminum'
         write(6,*)'Air & Aluminum'
c
      endif
c
      if (icase.eq.14) then	! air and iron
c
c        material 2 properties (iron)
c          subscript 0
c   
         r0 = 3368.0d0
         v0 = 1.0d0/r0
         gt = 2.8d0
         p0_infty = 0.0202d9
         p0 = 1.01325d5
         c0 = sqrt(gt*(p0+p0_infty)/r0)
c
c        material 1 properties (air)
c          subscript 0
c
         write(6,*)'Input Mach number'
         read(5,*) rmach
         R = 287.04
         T0 = 293.15
         r00 = p0/(R*T0)
         v00 = 1.0d0/r00
         gi = 1.4d0
         p00_infty = 0.0
         c00 = sqrt(gi*(p0+p00_infty)/r00)
c
         p2p1 = 1.0 + 2.0*gi*(rmach**2-1.0)/(gi+1.0)
         p1 = p0*p2p1
         write(6,*)'p1=', p1
         scale = 1.8
         open(unit=76,file='case14.dat')
         write(76,*)'Air & Iron'
         write(6,*)'Air & Iron'
c
      endif
c
      if (icase.eq.15) then	! air and nickel
c
c        material 2 properties (nickel)
c          subscript 0
c   
         r0 = 8875.0d0
         v0 = 1.0d0/r0
         gt = 4.2d0
         p0_infty = 44.519d9
         p0 = 1.01325d5
         c0 = sqrt(gt*(p0+p0_infty)/r0)
c
c        material 1 properties (air)
c          subscript 0
c
         write(6,*)'Input Mach number'
         read(5,*) rmach
         R = 287.04
         T0 = 293.15
         r00 = p0/(R*T0)
         v00 = 1.0d0/r00
         gi = 1.4d0
         p00_infty = 0.0
         c00 = sqrt(gi*(p0+p00_infty)/r00)
c
         p2p1 = 1.0 + 2.0*gi*(rmach**2-1.0)/(gi+1.0)
         p1 = p0*p2p1
         write(6,*)'p1=', p1
         scale = 1.8
         open(unit=76,file='case15.dat')
         write(76,*)'Air & Nickel'
         write(6,*)'Air & Nickel'
c
      endif
c
c
      if (icase.eq.16) then	! aluminum and air
c
c        material 1 properties (aluminum)
c          subscript 0
c   
         r00 = 2784.0d0
         v00 = 1.0d0/r00
         gt = 3.8d0
         p00_infty = 21.127d9
         p00 = 1.01325d5
         c00 = sqrt(gt*(p00+p00_infty)/r00)
         p0=p00
c
c        material 2 properties (air)
c          subscript 0
c
         write(6,*)'Input Mach number'
         read(5,*) rmach
         R = 287.04
         T0 = 293.15
         r0 = p0/(R*T0)
         v0 = 1.0d0/r0
         gi = 1.4d0
         p0_infty = 0.0
         c0 = sqrt(gi*(p+p0_infty)/r0)
c
         p2p1 = 1.0 + 2.0*gi*(rmach**2-1.0)/(gi+1.0)
         p1 = p0*p2p1
         write(6,*)'p1=', p1
         scale = 0.8
         open(unit=76,file='case16.dat')
         write(76,*)'Aluminum & Air'
         write(6,*)'Aluminum & Air'
c
      endif
c
c
      if (icase.eq.17) then	! Nitromethane and air
c
c
c        material 2 properties (nitromethane)
c          subscript 0
c
         r0 = 982.0d0
         v0 = 1.0d0/r0
         gt = 4.1d0
         p0_infty = 0.6496d9
         p0 = 1.0d5
c
c        material 1 properties (air)
c          subscript 00
c
         write(6,*)'Input Mach number'
         read(5,*) rmach
         R = 287.04
         T0 = 293.15
         r00 = p0/(R*T0)
         v00 = 1.0d0/r00
         gi = 1.4d0
         p00_infty = 0.0
         c00 = sqrt(gi*(p0+p00_infty)/r00)
c
         p2p1 = 1.0 + 2.0*gi*(rmach**2-1.0)/(gi+1.0)
         p1 = p0*p2p1
         write(6,*)'p1=', p1
         scale = 1.8
c
         open(unit=76,file='case17.dat')
         write(76,*)'Nitromethane & Air'
         write(6,*)'Nitromethane & Air'
c
      endif
c
c
c
c
      return
      end
