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

      open(11,file='ext.C1i', access='append')
      open(51,file='ext.Rxi', access='append') 
      open(61,file='ext.R2i', access='append') 
      open(71,file='ext.Pbi', access='append')  
      open(29,file='ext.Q1i', access='append')  
      open(30,file='ext.Qsi', access='append')  

      open(8,file='ext.C11', access='append')
      open(12,file='ext.Rx1', access='append') 
      open(13,file='ext.R21', access='append') 
      open(14,file='ext.Pb1', access='append')  
      open(31,file='ext.Q11', access='append')  
      open(32,file='ext.Qs1', access='append')

      open(15,file='ext.C12', access='append')
      open(19,file='ext.Rx2', access='append') 
      open(20,file='ext.R22', access='append') 
      open(21,file='ext.Pb2', access='append')  
      open(33,file='ext.Q12', access='append')  
      open(34,file='ext.Qs2', access='append')  

      open(22,file='ext.C1t', access='append')
      open(26,file='ext.Rxt', access='append') 
      open(27,file='ext.R2t', access='append') 
      open(28,file='ext.Pbt', access='append')  
      open(35,file='ext.Q1t', access='append')  
      open(36,file='ext.Qst', access='append') 

      
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
      read(5,130,end=20) id1,id2,li,numint,zL,nnb,ki,kp,nr
 130  format(a2,a6,i6,3i8,2f14.8,i8)
      if (id2.eq.'      ') goto 100
      prt=.true.
      imax=36
      do i = 1, imax
      read(5,123) lab(i),res(i),err(i),cor(i)
 123  format(i5,2e22.12,f12.5)
      enddo
      swm=numint*0.001d0

      write(11,101) li,res( 1),err( 1),zL,ki,swm,id2,nr
      write(51,101) li,res( 5),err( 5),zL,ki,swm,id2,nr
      write(61,101) li,res( 6),err( 6),zL,ki,swm,id2,nr
      write(71,101) li,res( 7),err( 7),zL,ki,swm,id2,nr
      write(29,101) li,res(29),err(29),zL,ki,swm,id2,nr
      write(30,101) li,res(30),err(30),zL,ki,swm,id2,nr

      write(8,101) li,res(8),err(8),zL,kp,swm,id2,nr
      write(12,101) li,res(12),err(12),zL,kp,swm,id2,nr
      write(13,101) li,res(13),err(13),zL,kp,swm,id2,nr
      write(14,101) li,res(14),err(14),zL,kp,swm,id2,nr
      write(31,101) li,res(31),err(31),zL,kp,swm,id2,nr
      write(32,101) li,res(32),err(32),zL,kp,swm,id2,nr

      write(15,101) li,res(15),err(15),zL,kp,swm,id2,nr
      write(19,101) li,res(19),err(19),zL,kp,swm,id2,nr
      write(20,101) li,res(20),err(20),zL,kp,swm,id2,nr
      write(21,101) li,res(21),err(21),zL,kp,swm,id2,nr
      write(33,101) li,res(33),err(33),zL,kp,swm,id2,nr
      write(34,101) li,res(34),err(34),zL,kp,swm,id2,nr

      write(22,101) li,res(22),err(22),zL,kp,swm,id2,nr
      write(26,101) li,res(26),err(26),zL,kp,swm,id2,nr
      write(27,101) li,res(27),err(27),zL,kp,swm,id2,nr
      write(28,101) li,res(28),err(28),zL,kp,swm,id2,nr
      write(35,101) li,res(35),err(35),zL,kp,swm,id2,nr
      write(36,101) li,res(36),err(36),zL,kp,swm,id2,nr


 101  format(i8,2f20.12,i12,f12.8,f6.2,1x,a2,i8)
      goto 100     
  20  continue                                                          
      stop                                                            
      end  
