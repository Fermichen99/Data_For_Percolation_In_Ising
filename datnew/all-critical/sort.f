      program sort
      integer i4char(20)
      parameter(maxp=100000)
      character*10 tex(maxp),texs
      real*8 q(maxp),erq(maxp),tp(maxp),qs,erqs,tps
      integer*8 sp(maxp),sps
      integer*4 l(maxp),ls,nrs,nr(maxp)
      i=0
  100 i=i+1
      read(5,101,end=102) l(i),q(i),erq(i),sp(i),tp(i),tex(i),nr(i)
  101 format(i8,2f20.8,i12,f12.8,a9,i8)
      if (l(i).eq.0) i=i-1
      goto 100
  102 continue
      np=i-1

  104 nin=0
      do 103 i=2,np

      if (sp(i).lt.sp(i-1)) then
        ls=l(i)
        qs=q(i)
        erqs=erq(i)
        tps=tp(i)
        sps=sp(i)
        texs=tex(i)
        nrs=nr(i)
        l(i)=l(i-1)
        q(i)=q(i-1)
        erq(i)=erq(i-1)
        tp(i)=tp(i-1)
        sp(i)=sp(i-1)
        tex(i)=tex(i-1)
        nr(i)=nr(i-1)
        l(i-1)=ls
        q(i-1)=qs
        erq(i-1)=erqs
        tp(i-1)=tps
        sp(i-1)=sps
        tex(i-1)=texs
        nr(i-1)=nrs
        nin=nin+1
      endif

      if(sp(i).eq.sp(i-1).and.l(i).lt.l(i-1)) then
        ls=l(i)
        qs=q(i)
        erqs=erq(i)
        tps=tp(i)
        sps=sp(i)
        texs=tex(i)
        nrs=nr(i)
        l(i)=l(i-1)
        q(i)=q(i-1)
        erq(i)=erq(i-1)
        tp(i)=tp(i-1)
        sp(i)=sp(i-1)
        tex(i)=tex(i-1)
        nr(i)=nr(i-1)
        l(i-1)=ls
        q(i-1)=qs
        erq(i-1)=erqs
        tp(i-1)=tps
        sp(i-1)=sps
        tex(i-1)=texs
        nr(i-1)=nrs
        nin=nin+1
      endif

      if(sp(i).eq.sp(i-1).and.l(i).eq.l(i-1).and.tp(i).lt.tp(i-1)) then
        ls=l(i)
        qs=q(i)
        erqs=erq(i)
        tps=tp(i)
        sps=sp(i)
        texs=tex(i)
        nrs=nr(i)
        l(i)=l(i-1)
        q(i)=q(i-1)
        erq(i)=erq(i-1)
        tp(i)=tp(i-1)
        sp(i)=sp(i-1)
        tex(i)=tex(i-1)
        nr(i)=nr(i-1)
        l(i-1)=ls
        q(i-1)=qs
        erq(i-1)=erqs
        tp(i-1)=tps
        sp(i-1)=sps
        tex(i-1)=texs
        nr(i-1)=nrs
        nin=nin+1
      endif


  103 continue
      if (nin.gt.0) goto 104
      do 105 i=1,np
      if(erq(i)<1.d-10) erq(i) = 1.d-10
      write(6,106) l(i),q(i),erq(i),tp(i),sp(i),tex(i),nr(i)
  106 format(i8,2f20.8,f12.8,i12,a9,i8)
  105 continue
      stop
      end
