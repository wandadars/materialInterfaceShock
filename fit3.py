"""
     Solves normal shock-material interface problem
     with ideal or stiffened gas

     Thomas L. Jackson
     June 2014

     The material has subscript 00
     The gas upstream of the material interface has subscripts
       0 for unshocked gas
       1 for shocked gas

     After the shock has collided with the material interface,
     the incident shock creates a transmitted shock that travels
     through the material with speed u_ti, the material interface
     moves with speed u2=ut, and there is a reflected wave
     that can either be a shock or an expansion wave that
     travels with speed u_ri. In the material the shocked state
     has subscript t, while in the incident gas the region
     between the reflected wave and the interface has subscript 2.
     See notes for more information.

"""



def determine_incident_wave_values():
    """
    Subscript '1' denotes values related to the incident wave
    """
    v_1 = v00*((gi+1.0)*p0+(gi-1.0)*p1 + 2.0 * gi * p00_infty) / ((gi+1.0)*p1 + (gi-1.0)*p0 + 2.0 * gi * p00_infty)
    r_1 = 1.0/ v_1

    ui = v00 * np.sqrt((p1-p0)/(v00-v_1))   ! incident shock speed

    u1 = ui * (v00-v_1) / v00      ! piston speed

    c00 = np.sqrt(gi*(p0 + p00_infty) / r00) ! sound speed in unshocked gas
    c11 = np.sqrt(gt * (p0 + p0_infty) / r0)   ! sound speed in unshocked material
    rm0 = ui / c00


def compute_state_after_shock_hits_boundary():
    """
    Compute states after incident shock
    has hit the material boundary
    Use Secant method to find solution
    """
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


def compute_impedance_ratio():
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


def compute_shock_speed_ratio():
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


def func(p2):
     v2 = v1*((gi-1.0d0)*p2+(gi+1.0d0)*p1+2.0d0*gi*p00_infty) / ((gi-1.0d0)*p1+(gi+1.0d0)*p2+2.0d0*gi*p00_infty)
     r2 = 1.0d0/v2

     vt = v0*((gt+1.0d0)*p0+(gt-1.0d0)*p2+2.0d0*gt*p0_infty) / ((gt+1.0d0)*p2+(gt-1.0d0)*p0+2.0d0*gt*p0_infty)
     rt = 1.0d0/vt

     uti = v0*sqrt((p2-p0)/(v0-vt))

     u2 = uti*(1.0d0-vt/v0)

     uri = (u1*v2-v1*u2)/(v1-v2)

     pt = p2

     return (r1*(u2-u1)*(u1+uri))/(p2-p1)+1.0d0



def print_input_parameters():
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

