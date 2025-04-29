      program extchiv 
      parameter (mxl=50000) 
      implicit real*8 (a-h,o-z)
      character*2 id1,id3 
      character*6 id2 
      integer  kf, nnb, zL
      integer lab(100)
      real*8  res(100),err(100),cor(100)
      real*8  ki, kp
      logical leven,corrh,nogh,prt
      logical prt1,prt2,prt3,prt4,prt5,prt6,prt7

      open(11,file='ext.C11', access='append') 
      open(13,file='ext.C12', access='append')   
      open(15,file='ext.S21', access='append') 
      open(16,file='ext.Rx1', access='append') 
      open(17,file='ext.R21', access='append') 
      open(18,file='ext.R01', access='append') 
      open(19,file='ext.Pbh', access='append')  

      open(110,file='ext.C21', access='append') 
      open(112,file='ext.C22', access='append')  
      open(114,file='ext.S22', access='append') 
      open(115,file='ext.Rx2', access='append') 
      open(116,file='ext.R22', access='append') 
      open(117,file='ext.R02', access='append') 
      open(118,file='ext.Pb2', access='append')  

      open(119,file='ext.C1t', access='append') 
      open(121,file='ext.C2t', access='append')   
      open(123,file='ext.S2t', access='append') 
      open(124,file='ext.Rxt', access='append') 
      open(125,file='ext.R2t', access='append') 
      open(126,file='ext.R0t', access='append') 
      open(127,file='ext.Pbt', access='append')  

!     dk = 0.0000002d0
      nop=0
 140  read(5,141,end=20) id1,id2
 141  format(a2,a6)
      if ((id1.eq.'Pe')) then
         rewind(5)
         goto 142
      endif
      nop=nop+1
      goto 140
 142  do 143 i=1,nop
      read(5,141,end=20) id1
 143  continue
 100  continue
      read(5,130,end=20) id1,id2,li,numint,zL,nnb,kp,ki,nr
 130  format(a2,a6,i6,3i8,2f14.8,i8)
      if (id2.eq.'      ') goto 100
      prt=.true.
      imax=49
      do i = 1, imax
      read(5,123) lab(i),res(i),err(i),cor(i)
 123  format(i5,2e22.12,f12.5)
      enddo
      swm=numint*0.001d0
      

      write(11,101) li,res(9),err(9),zL,kp,swm,id2,nr
      write(13,101) li,res(11),err(11),zL,kp,swm,id2,nr
      write(15,101) li,res(13),err(13),zL,kp,swm,id2,nr
      write(16,101) li,res(15),err(15),zL,kp,swm,id2,nr
      write(17,101) li,res(16),err(16),zL,kp,swm,id2,nr
      write(18,101) li,res(17),err(17),zL,kp,swm,id2,nr
      write(19,101) li,res(16)-res(27),err(16),zL,kp,swm,id2,nr

      write(110,101) li,res(19),err(19),zL,kp,swm,id2,nr
      write(112,101) li,res(21),err(21),zL,kp,swm,id2,nr
      write(114,101) li,res(23),err(23),zL,kp,swm,id2,nr
      write(115,101) li,res(25),err(25),zL,kp,swm,id2,nr
      write(116,101) li,res(26),err(26),zL,kp,swm,id2,nr
      write(117,101) li,res(27),err(27),zL,kp,swm,id2,nr
      write(118,101) li,res(28),err(28),zL,kp,swm,id2,nr

      write(119,101) li,res(29),err(29),zL,kp,swm,id2,nr
      write(121,101) li,res(31),err(31),zL,kp,swm,id2,nr
      write(123,101) li,res(33),err(33),zL,kp,swm,id2,nr
      write(124,101) li,res(35),err(35),zL,kp,swm,id2,nr
      write(125,101) li,res(36),err(36),zL,kp,swm,id2,nr
      write(126,101) li,res(37),err(37),zL,kp,swm,id2,nr
      write(127,101) li,res(38),err(38),zL,kp,swm,id2,nr
 101  format(i8,2f20.12,i12,f12.8,f6.2,1x,a2,i8)
      goto 100     
  20  continue                                                          
      stop                                                            
      end  
