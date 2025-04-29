      program comp
      implicit real*8 (a-h,o-z)
      integer i4char(20)
      parameter(maxp=100000)
      parameter(eps =1.0d-10)
      character*19 tex(maxp),texs
      real*8 q(maxp),erq(maxp),tp(maxp),qt(maxp),erqt(maxp)
      real*8 qs,erqs,tps,rs,qa,erqa,s,sw,w,er,cs
      integer*4 sp(maxp),sps
      real*8 sm(maxp)
      integer*4 l(maxp),ls,np(maxp)
      ls=0
      qs=0.0d0
      erqs=0.0d0
      tps=0.0d0
      sps=0
      rs=0.0d0
      texs='                  '
      ig=0
      i=0
  100 i=i+1
      read(5,101,end=102) l(i),q(i),erq(i),tp(i),sp(i),sm(i),tex(i)
  101 format(i8,2f20.8,f12.8,i12,f6.1,a13)
      if(l(i).eq.ls.and.tp(i).eq.tps.and.sp(i).eq.sps) then
         np(ig)=np(ig)+1
      else
         ig=ig+1
         if (ig.gt.2000) then
            write(6,200) ig
  200       format(' ig too large',i4)
            stop
         endif
         np(ig)=1
         ls=l(i)
         tps=tp(i)
         sps=sp(i)
      endif
      goto 100

  102 continue
      ntp=i-1
      ng =ig
      i  =0
      do 103 ig=1,ng
        s  =0.0d0
        sw =0.0d0
        sms=0
        do 104 k=1,np(ig)
           i  =i+1
           sms=sms+sm(i)
           w  =erq(i)
           if(dabs(w).gt.eps) w = 1.d0/(w*w)
           sw =sw+w
           s  =s+q(i)*w
           qt(k)=q(i)
           erqt(k)=erq(i)
  104   continue

        if(dabs(sw).gt.eps) then
          qa=s/sw
          erqa=1.0d0/dsqrt(sw)
          s =0.0d0
          do k=1,np(ig)
            er=(qa-qt(k))/erqt(k)
            s =s+er*er
          enddo
        else
          qa   = qt(1)
          erqa = 0.d0
          sw   = 0.d0
          s    = 0.d0
          do k=2,np(ig)
            if(dabs(qa-qt(k)).gt.eps) s = np(ig)*10
          enddo
        endif

        sp(i) = (2*sp(i)+1)**2-1
        write(6,106) l(i),qa,erqa,tp(i),sp(i),sms,(np(ig)-1),s
  106   format(i8,2f20.8,f12.8,i12,f8.1,i5,f8.2)
  103 continue
      stop
      end
