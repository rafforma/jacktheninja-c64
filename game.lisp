;;;;;;; 

(defmacro (rts) (begin
	(.byte #$60)    
    #0 ))

(defmacro (dex) (begin
	(.byte #$ca)    
    #0 ))

(defmacro (dey) (begin
	(.byte #$88)    
    #0 ))

(defmacro (inx) (begin
	(.byte #$e8)    
    #0 ))

(defmacro (tsx) (begin
	(.byte #$ba)    
    #0 ))

(defmacro (pha) (begin
	(.byte #$48)    
    #0 ))

(defmacro (pla) (begin
	(.byte #$68)    
    #0 ))

(defmacro (adc.zp v) (begin
	(.byte #$65)
    (.byte v)
    v ))

	
(defmacro (adc.a v) (begin
	(.byte #$6d)
    (.word v)
    v ))

(defmacro (adc.im v) (begin
	(.byte #$69)
    (.byte v)
    v ))

(defmacro (sbc.a v) (begin
	(.byte #$ed)
    (.word v)
    v ))


(defmacro (inc.zp v) (begin
	(.byte #$E6)
    (.byte v)
    v ))

(defmacro (lda.a+y a) (begin
	(.byte #$b9)
	(.if (.addref? a)
		(.word a)
		(.word a))
    a ))

(defmacro (lda.a+x v) (begin
	(.byte #$BD)
    (.word v)
    v ))



(defmacro (and.im v) (begin
	(.byte #$29 v)
	v))


(defmacro (ora.im v) (begin
	(.byte #$09 v)
	v))

(defmacro (ora.a v) (begin
	(.byte #$0d)
	(.word v)
	v))


(defmacro (eor.im v) (begin
	(.byte #$49 v)
	v))


(defmacro (lda.im v) (begin
	(.byte #$a9 v)
	v))

(defmacro (lda.a v) (begin
	(.byte #$ad)
        (.word v)
	v))

(defmacro (ldy.im v) (begin
	(.byte #$a0 v)
	v))

(defmacro (ldy.a v) (begin
	(.byte #$ac)
    	(.word v)
	v))

(defmacro (ldx.im v) (begin
	(.byte #$a2 v)
	v))

(defmacro (ldx.a v) (begin
	(.byte #$ae)
    (.word v)
	v))


(defmacro (sta.zp v) (begin
	(.byte #$85)
    (.byte v)
	v))
	
(defmacro (sta.zp+x v) (begin
	(.byte #$81)
    (.byte v)
	v))


(defmacro (sta.zp+y v) (begin
	(.byte #$91)
    (.byte v)
	v))

(defmacro (lda.zp+x v) (begin
	(.byte #$a1)
    (.byte v)
	v))

(defmacro (lda.zp+y v) (begin
	(.byte #$b1)
    (.byte v)
	v))

	
(defmacro (lda.zp v) (begin
	(.byte #$A5)
    (.byte v)
	v))

(defmacro (sta.a v) (begin
	(.byte #$8d)
    (.word v)
	v))

(defmacro (sta.ax v) (begin
	(.byte #$9d)
    (.word v)
	v))
    
(defmacro (sta.ay v) (begin
	(.byte #$99)
    	(.word v)
	v))

(defmacro (stx.a v) (begin
	(.byte #$8e)
    	(.word v)
	v))

(defmacro (sty.a v) (begin
	(.byte #$8c)
    	(.word v)
	v))

(defmacro (iny) (begin
	(.byte #$c8)
	#0))

(defmacro (clc) (begin
	(.byte #$18)
	#0))

(defmacro (sec) (begin
	(.byte #$38)
	#0))

(defmacro (inc.a v) (begin
	(.byte #$ee)
        (.word v)
	v))
    
(defmacro (dec.a v) (begin
	(.byte #$ce)
        (.word v)
	v))

(defmacro (inc.ax v) (begin
	(.byte #$fe)
    (.word v)
	v))
    
(defmacro (cmp.im v)  (begin 
	(.byte #$c9 v)
	v))

(defmacro (cmp.a v)  (begin 
	(.byte #$cd)
	(.word v)
	v))

(defmacro (cpx.a v)  (begin 
	(.byte #$ec)
	(.word v)
	v))

(defmacro (cpy.a v)  (begin 
	(.byte #$cc)
	(.word v)
	v))

(defmacro (cpy.im v)  (begin 
	(.byte #$c0)
	(.byte v)
	v))

(defmacro (peek a) (begin
  (lda.a a)
	a))

(defmacro (tya) (begin
  (.byte #$98)
	#0))

(defmacro (txa) (begin
  (.byte #$8A)
	#0))

(defmacro (tay) (begin
  (.byte #$A8)
	#0))

(defmacro (tax) (begin
  (.byte #$AA)
	#0))

(defmacro (lsr ) (begin
  (.byte #$4a)
	#0))

(defmacro (asl) (begin
  (.byte #$0a)
	#0))

(defmacro (memset address val size) (
	(lda.im val)
	(dotimes size (		
	(sta.ax address)))))
		
(defmacro (color back fore) (
	  (poke back 53281)
	  (poke fore 53280)))

(defmacro (++ x) (begin 
	(inc.a x) 
	x))
		
(defmacro (-- x) (begin 
	(dec.a x) 
	x))

(defmacro (when code) (begin 
	code
	))

(defmacro (x=sp) (begin 
	(tsx)                   ;; prende il primo parametro	
	(lda.a+x #$103)
	))

(defmacro (x=sp+1) (begin 
	(tsx)                   ;; prende il secondo parametro	
	(lda.a+x #$104)
	))

(defmacro (x=sp+2) (begin 
	(tsx)                   ;; prende il terzo parametro	
	(lda.a+x #$105)
	))

(defmacro (x=sp+3) (begin 
	(tsx)                   ;; prende il terzo parametro	
	(lda.a+x #$106)
	))

(defmacro (x=sp+4) (begin 
	(tsx)                   ;; prende il terzo parametro	
	(lda.a+x #$107)
	))

(defmacro (bit-and a b) (begin
	(.if (.gt a 255) 
		(lda.a a)
		(lda.im a))

	(and.im b)
	))

(defmacro (== a b) (begin
	(.if (.addref? a)
		(lda.a a)
		(.if (.gt a 255) 
			(lda.a a)
			(lda.im a)))

	(.if (.addref? b)
		(cmp.a  b)
		(.if (.gt b 255) 
			(cmp.a b)
			(cmp.im b)))	
	))

(defmacro (= b a) (begin
	(.if (.addref? a)
		(lda.a a)
		(.if (.gt a 255) 
			(lda.a a)
			(lda.im a)))

	(.if (.addref? b)
		(sta.a b)
		(.if (.gt b 255) 
			(sta.a b)
			(sta.im b)))	
	))

(defmacro (test a b) (begin
	(.if (.gt a 255) 
		(lda.a a)
		(lda.im a))

	(cmp.im b)
	))

(defmacro (set address value) (begin
	(lda.im value)
	(sta.a address)
	0
	))

(defmacro (wait-retrace v) (begin 
	(repeat-until :zero (begin
		(lda.a #$d012)
		(cmp.im v)
	))
))

(defmacro (put-char-at x y c) (begin (
		(get-cursor-at x y)
		(.if (.addref? c) 
			(lda.a c)
			(lda.im c))
		
		(ldx.im 0)
	    (sta.zp+x #$02)

	)))

(defmacro (get-char-at x y c) (begin (
		(get-cursor-at x y)
		(ldx.im #0)
	    (lda.zp+x #$02)
		(.if (.addref? c) 
			(sta.a c)
			(sta.a c))
)))

(defmacro (scroll-h offset) (begin 
	(lda.a #$d016)
	(and.im #$f8)	
	(.if (.addref? offset) 
			(ora.a offset)
			(ora.im offset))

	(sta.a #$d016)
))



(defun main-loop (begin	

	;(get-char-at (addr :x) (addr :y) (addr :char))
	(label-at :map #$5000)
    (= #53280 #10)
    (= #53281 #11)
	
	
	(lda.a #$d016)
	(and.im #b11110111)
	(sta.a #$d016)
	
	(level1)
	(scroll-h (.addr :scroll))
	(copy-screen)

	(= #$d015 #$ff)
	(= #$d01c #$ff)


	(= (.addr :rom-charset) #$d018)
	(= #$d018 #$1c)
	(lda.a #$d016)
	(and.im #b11110111)
	(sta.a #$d016)

	(forever 
		
		(wait-retrace #200)
		(read-joystick)
		(sta.a (.addr :joy))		
		;;;;(move-player)		
		(move-soft-sprite)
		(draw-software-sprite)
	;;	(= #$d000 (.addr  :playerx))
	;;	(= #$d001 (.addr  :playery))
	)

	(rts)


	(with-label :prevpy (.byte #0))
	(with-label :prevpx (.byte #0))
	(with-label :playerx (.byte #150))
	(with-label :playery (.byte #65))


	(with-label :rom-charset (.byte #0))
	(with-label :need-scroll (.byte #1))
	(with-label :scroll (.byte #00))
	(with-label :scroll-left (.byte #00))
	(with-label :scroll-right (.byte #39))
	(with-label :prevy (.byte #0))
	(with-label :prevx (.byte #0))
	(with-label :char (.byte #0))
	(with-label :joy  (.byte #0))
	(with-label :x    (.byte #10))
	(with-label :col-num    (.byte #20))
	(with-label :y    (.byte #5))))
	
(defun shift-screen-left (begin 


	(lda.a (.addr :scroll-right))
	(sta.zp #$02)
	
	(lda.im #$50)
	(sta.zp #$03)

	(lda.im #$27)
	(sta.zp #$04)
	
	(lda.im #$04)
	(sta.zp #$05)
	
	(= (.addr :col-num) 20)

	
	(repeat-until :zero (begin 

		(lda.zp+x #$02)
		(sta.zp+x #$04)					
		
		(clc)
		(lda.im #80)
		(adc.zp #$02)
		(sta.zp #$02)
		(if :carry-set 
			(inc.zp #$03))		
			
		(clc)
		(lda.im #40)
		(adc.zp #$04)
		(sta.zp #$04)
		(if :carry-set 
			(inc.zp #$05))		

		(dec.a (.addr :col-num))
	))

	(ldx.im #0)

	(repeat-until :zero (begin
		(lda.a+x #$0401) 
		(sta.ax #$0400) 

		(lda.a+x #$0429) 
		(sta.ax #$0428) 

		(lda.a+x #$0451) 
		(sta.ax #$0450) 

		(lda.a+x #$0479) 
		(sta.ax #$0478) 

		(lda.a+x #$04a1) 
		(sta.ax #$04a0) 

		(lda.a+x #$04c9) 
		(sta.ax #$04c8) 

		(lda.a+x #$04f1) 
		(sta.ax #$04f0) 

		(lda.a+x #$0519) 
		(sta.ax #$0518) 

		(lda.a+x #$0541) 
		(sta.ax #$0540) 

		(lda.a+x #$0569) 
		(sta.ax #$0568) 

		(lda.a+x #$0591) 
		(sta.ax #$0590) 

		(lda.a+x #$05b9) 
		(sta.ax #$05b8) 

		(lda.a+x #$05e1) 
		(sta.ax #$05e0) 

		(lda.a+x #$0609) 
		(sta.ax #$0608) 

		(lda.a+x #$0631) 
		(sta.ax #$0630) 

		(lda.a+x #$0659) 
		(sta.ax #$0660) 

		(lda.a+x #$0681) 
		(sta.ax #$0680) 

		(inx)		
		(txa)
		(cmp.im #40)
	))

	

	

	(rts)
))


(defun shift-screen-right (begin 


	

	(ldx.im #38)
	(repeat-until :zero (begin
		(lda.a+x #$0400) 
		(sta.ax #$0401) 

		(lda.a+x #$0428) 
		(sta.ax #$0429) 

		(lda.a+x #$0450) 
		(sta.ax #$0451) 

		(lda.a+x #$0478) 
		(sta.ax #$0479) 

		(lda.a+x #$04a0) 
		(sta.ax #$04a1) 

		(lda.a+x #$04c8) 
		(sta.ax #$04c9) 

		(lda.a+x #$04f0) 
		(sta.ax #$04f1) 

		(lda.a+x #$0518) 
		(sta.ax #$0519) 

		(lda.a+x #$0540) 
		(sta.ax #$0541) 

		(lda.a+x #$0568) 
		(sta.ax #$0569) 

		(lda.a+x #$0590) 
		(sta.ax #$0591) 

		(lda.a+x #$05b8) 
		(sta.ax #$05b9) 

		(lda.a+x #$05e0) 
		(sta.ax #$05e1) 

		(lda.a+x #$0608) 
		(sta.ax #$0609) 

		(lda.a+x #$0630) 
		(sta.ax #$0631) 

		(lda.a+x #$0658) 
		(sta.ax #$0659) 

		(lda.a+x #$0680) 
		(sta.ax #$0681) 

		(dex)		
	))
	
		(lda.a #$0400) 
		(sta.a #$0401) 

		(lda.a #$0428) 
		(sta.a #$0429) 

		(lda.a #$0450) 
		(sta.a #$0451) 

		(lda.a #$0478) 
		(sta.a #$0479) 

		(lda.a #$04a0) 
		(sta.a #$04a1) 

		(lda.a #$04c8) 
		(sta.a #$04c9) 

		(lda.a #$04f0) 
		(sta.a #$04f1) 

		(lda.a #$0518) 
		(sta.a #$0519) 

		(lda.a #$0540) 
		(sta.a #$0541) 

		(lda.a #$0568) 
		(sta.a #$0569) 

		(lda.a #$0590) 
		(sta.a #$0591) 

		(lda.a #$05b8) 
		(sta.a #$05b9) 

		(lda.a #$05e0) 
		(sta.a #$05e1) 

		(lda.a #$0608) 
		(sta.a #$0609) 

		(lda.a #$0630) 
		(sta.a #$0631) 

		(lda.a #$0658) 
		(sta.a #$0659) 

		(lda.a #$0680) 	
		(sta.a #$0681) 

	(lda.a (.addr :scroll-left))
	(sta.zp #$02)
	
	(lda.im #$50)
	(sta.zp #$03)

	(lda.im #$00)
	(sta.zp #$04)
	
	(lda.im #$04)
	(sta.zp #$05)
	
	(= (.addr :col-num) 20)

	
	(repeat-until :not-minus (begin 

		(lda.zp+x #$02)
		(sta.zp+x #$04)					
		
		(clc)
		(lda.im #80)
		(adc.zp #$02)
		(sta.zp #$02)
		(if :carry-set 
			(inc.zp #$03))		
			
		(clc)
		(lda.im #40)
		(adc.zp #$04)
		(sta.zp #$04)
		(if :carry-set 
			(inc.zp #$05))		

		(dec.a (.addr :col-num))
	))	

	(rts)
))
(defun copy-screen (begin
	(lda.a (.addr :scroll-left))
	(sta.zp #$02)
	
	(lda.im #$50)
	(sta.zp #$03)

	(lda.im #$00)
	(sta.zp #$04)
	
	(lda.im #$04)
	(sta.zp #$05)	

	(= (.addr :col-num) 20)
	
	(repeat-until :zero (begin 
		(ldy.im #40)
		(repeat-until :not-minus (begin 
			(lda.zp+y #$02)
			(sta.zp+y #$04)			
			(dey)
		))
		
		(clc)
		(lda.im #80)
		(adc.zp #$02)
		(sta.zp #$02)
		(if :carry-set 
			(inc.zp #$03))		
			
		(clc)
		(lda.im #40)
		(adc.zp #$04)
		(sta.zp #$04)
		(if :carry-set 
			(inc.zp #$05))		

		(dec.a (.addr :col-num))
	))
	(rts)
))

(defun move-player (begin	
		
		(put-char-at (.addr :x) (.addr :y) #32)

		(= (.addr :prevx) (.addr :x))	
		(= (.addr :prevy) (.addr :y))	


		(= (.addr :prevpx) (.addr :playerx))	
		(= (.addr :prevpy) (.addr :playery))	


		(if (bit-and (mem :joy) #$2) (begin			
			(++ (.addr :playery))
			))
		
		(if (bit-and (mem :joy) #$1) (begin
			(-- (.addr :playery))
			))

						
		(lda.a (.addr :playery))
		(lsr)
		(lsr)
		(lsr)
		(sta.a (.addr :y))

		(get-char-at (.addr :x) (.addr :y) (.addr :char))
		
		
		(if (== (.addr :char) #32)  (begin
			(= (.addr :y) (.addr :prevy))
			(= (.addr :playery) (.addr :prevpy))
		))

		(if (bit-and (mem :joy) #$8) (begin
			(++ (.addr :playerx))
			))

		(if (bit-and (mem :joy) #$4) (begin
			(-- (.addr :playerx))
			))

		(lda.a (.addr :playerx))
		(sec)
		(sbc.a (.addr :scroll))
		(lsr)
		(lsr)
		(lsr)
		(sta.a (.addr :x))

		(get-char-at (.addr :x) (.addr :y) (.addr :char))

		(if (== (.addr :char) #32)  (begin
			(= (.addr :x) (.addr :prevx))
			(= (.addr :playerx) (.addr :prevpx))
		))
		
		

		(= (.addr :need-scroll) 0)

		(test (mem :x) #30)
		(if :carry-set  (begin
			
			
			;;(dec.a (.addr :scroll))
			(dec.a (.addr :scroll))

			(if :minus (begin 
				(lda.im #7)
				(sta.a (.addr :scroll))
				(++ (.addr :scroll-left))
				(++ (.addr :scroll-right))				
				(shift-screen-left)
			))
			(lda.a (.addr :scroll))
			(and.im #7)
			(sta.a (.addr :scroll))
			(setb :x #29)
			(= (.addr :playerx) (.addr :prevpx))
			(scroll-h (.addr :scroll))
			(lda.a (.addr :need-scroll))
			
		))
					

		(test (mem :x) #10)
		(if :minus  (begin 
			(inc.a (.addr :scroll))
		;;	(inc.a (.addr :scroll))
			
			(test (mem :scroll) #7)
			(if :carry-set  (begin
				(lda.im #0)
				(sta.a (.addr :scroll))
				(-- (.addr :scroll-left))
				(-- (.addr :scroll-right))				
				(shift-screen-right)
				
			))

			(lda.a (.addr :scroll))
			(and.im #7)
			(sta.a (.addr :scroll))
			(setb :x #10)
			(= (.addr :playerx) (.addr :prevpx))
			(scroll-h (.addr :scroll))
			
			(lda.a (.addr :need-scroll))
			
		))
			
		(lda.a (.addr :playerx))
		(sec)
		(sbc.a (.addr :scroll))
		(lsr)
		(lsr)
		(lsr)
		(sta.a (.addr :x))
        
		(put-char-at (.addr :x) (.addr :y) 0)

        (rts)
))


(defun move-soft-sprite (begin	
	
		(get-cursor-at (.addr :x) (.addr :y))
		(ldy.im #0)
		(lda.im #32)
		(sta.zp+y #$02)
		(iny)		
		(sta.zp+y #$02)
		(ldy.im #40)
		(sta.zp+y #$02)
		(iny)		
		(sta.zp+y #$02)

		(if (bit-and (mem :joy) #$2) (begin			
			(++ (.addr :playery))
			))
		
		(if (bit-and (mem :joy) #$1) (begin
			(-- (.addr :playery))
			))

						
		(lda.a (.addr :playery))
		(lsr)
		(lsr)
		(lsr)
		(sta.a (.addr :y))		

		(if (bit-and (mem :joy) #$8) (begin
			(++ (.addr :playerx))
			))

		(if (bit-and (mem :joy) #$4) (begin
			(-- (.addr :playerx))
			))

		(lda.a (.addr :playerx))
;;		(sec)
;;		(sbc.a (.addr :scroll))
		(lsr)
		(lsr)
		(lsr)
		(sta.a (.addr :x))

        (rts)
))

(defun draw-box (begin

		(lda.a (.addr :lo-addr))
		(sta.zp #$02)
		(lda.a (.addr :hi-addr))
		(sta.zp #$03)
		
	    (repeat-until :zero (begin 		
			(ldy.im 0)
			(repeat-until :zero (begin
				(lda.a (.addr :ch))
				(sta.zp+y #$02)			
				(iny)
				(cpy.a (.addr :col-len))			
			))
			(clc)
			(lda.im #80)
			(adc.zp #$02)
			(sta.zp #$02)
			(if :carry-set 
				(inc.zp #$03))
				
			(dec.a (.addr :row-len))					
		))
		(rts)

			(with-label :box-param 
			(with-label :lo-addr (.byte  #$01))		
			(with-label :hi-addr (.byte  #$01))
			(with-label :col-len (.byte  #$01))
			(with-label :row-len (.byte  #$01))
			(with-label :ch      (.byte  #$01))
			(with-label :box-cmd (.byte  #$01)) 
		)
))

(defun draw-box-pattern (begin

		(lda.a (.addr :lo-addr))
		(sta.zp #$02)
		(lda.a (.addr :hi-addr))
		(sta.zp #$03)
		
		(lda.im #0)
		(sta.a (.addr :pattern-count))
		
		(lda.a (.addr :pattern-width))
		(sta.a (.addr :pattern-tmp-width))
		
		(lda.a (.addr :pattern-height) )
		(sta.a (.addr :pattern-tmp-height))
		
	    (repeat-until :zero (begin 		
			(ldx.a (.addr :pattern-count))
			(ldy.im #0)
			(repeat-until :zero (begin
				(lda.a+x (.addr :pattern-data))
				(sta.zp+y #$02)			
				(inx)
				(txa)
				(cmp.a (.addr :pattern-tmp-width))
				(if :not-zero 
					(ldx.a (.addr :pattern-count)))					
				(iny)
				(cpy.a (.addr :col-len))			
			))
			(clc)
			(lda.im #80)
			(adc.zp #$02)
			(sta.zp #$02)
			(if :carry-set 
				(inc.zp #$03))
			
			
			(lda.a (.addr :pattern-tmp-width))
			(sta.a (.addr :pattern-count))
			
			
			(clc)
			(lda.a (.addr :pattern-tmp-width))			
			(adc.a (.addr :pattern-width))			
			(sta.a (.addr :pattern-tmp-width))

			(dec.a (.addr :pattern-tmp-height))
			
			(if :not-zero (begin 
			
					(lda.a (.addr :pattern-height) )
					(sta.a (.addr :pattern-tmp-height))
					
					(lda.im #0 )
					(sta.a (.addr :pattern-count))		
					
					(lda.a (.addr :pattern-width))
					(sta.a (.addr :pattern-tmp-width))))
			
			(dec.a (.addr :row-len))
		))
		(rts)


		(with-label :pattern
			(with-label :pattern-count      (.byte  #$00))
			(with-label :pattern-tmp-width  (.byte  #$00))
			(with-label :pattern-tmp-height (.byte  #$00))
			
			(with-label :pattern-width      (.byte  #$03))		
			(with-label :pattern-height     (.byte  #$02))
			(with-label :pattern-data       (.byte  #$1 #$2 #$3)
										    (.byte  #$4 #$5 #$6))
		)
))
(defmacro (add-box pos w h c) (begin
    (.byte 6)
	(.word pos)
	(.byte w h c)
	(.byte 1)
))


(defmacro (add-box-pattern pos w h c) (begin
    (.byte 6)
	(.word pos)
	(.byte w h c)
	(.byte 2)
))

(defun level1 (begin 

	(ldx.im 0)	
	(repeat-until :zero (begin 
		(lda.a+x (.addr :level1-data))
		(sta.a (.addr :cmd-size))		
		(if :zero (begin 
			(ldy.im 0)
			(inx)
			(repeat-until :zero (begin				
				(lda.a+x (.addr :level1-data))
				(sta.ay  (.addr :box-param))
				(inx)	
				(iny)
				(dec.a (.addr :cmd-size))			
			))
			(txa)
			(pha)
			(tya)
			(pha)

			(test (mem :box-cmd) #1)
			(if :not-zero 			;;;;;;; รจ sbagliato il principo dell'if
				(draw-box))

			(test (mem :box-cmd) #2)
			(if :not-zero 			
				(draw-box-pattern))
			
			;;(draw-box)	
			(pla)
			(tay)
			
			(pla)
			(tax)
			
		))
			
		(lda.a+x (.addr :level1-data))
		(sta.a (.addr :cmd-size))		
	
	))
	
	(rts)
	(with-label :cmd-size (.byte 0)) 
    (with-label :level1-data) 
		(add-box #$5000 80 24 32)  ;; clear screen
		;(add-box-pattern #$5000 5 6 1)
		;(add-box #$5030 3 24 4)
		;(add-box #$5103 5 8 2)
		;(add-box-pattern #$5203 3 7 44)
		;(add-box #$504f 1 8 0)

		(.byte 0)
	))
(defun read-joystick (begin 
	(lda.im #$ff)
	(sta.a #$dc00)
	(lda.a #$dc00)
	(eor.im #$ff)
	(rts)
))

(defun get-cursor-at (begin 
	;;(screen-lookup)
	;; reg.x coordinata x
	;; reg.y coordinata y 
	;; il risultato sta in $2 $3
	(x=sp)                   ;; carica y
	(asl)   ;; moltiplica per 2
	(tay)
	(lda.a+y (.addr screen-lookup-table))
	(sta.zp #$02)
	(iny)
	(lda.a+y (.addr screen-lookup-table))
	(sta.zp #$03)	
	
	(x=sp+1) ;; carica x
	(clc)
	(adc.zp #$02)
	(sta.zp #$02)
	(if :carry-set 
		(inc.zp #$03))
	(rts)

	(with-label screen-lookup-table ;;;tabella accesso schermo
		(.word  #$0400) ;;0
		(.word  #$0428) ;;0
		(.word  #$0450) ;;0
		(.word  #$0478) ;;0
		(.word  #$04a0) ;;0
		(.word  #$04c8) ;;0
		(.word  #$04f0) ;;0
		(.word  #$0518) ;;0
		(.word  #$0540) ;;0
		(.word  #$0568) ;;0
		(.word  #$0590) ;;0
		(.word  #$05b8) ;;0
		(.word  #$05e0) ;;0
		(.word  #$0608) ;;0
		(.word  #$0630) ;;0
		(.word  #$0658) ;;0
		(.word  #$0680) ;;0
		(.word  #$06a8) ;;0
		(.word  #$06d0) ;;0
		(.word  #$06f8) ;;0
		(.word  #$0720) ;;0
		(.word  #$0748) ;;0
		(.word  #$0770) ;;0
		(.word  #$0798)
		(.word  #$07c0)))) ;;0


(defmacro (copy-byte-mychar char) (begin 			
	
	;;(lda.a+y char)
	(lda.zp+y #$02)
	(ldx.a (.addr :extend))	
	(while :not-zero (begin 		
		(lsr)
		(dex)
	))	
	;;(sta.ay #$3000)
	(sta.zp+y #$04)	
))

(defmacro (copy-byte-next char) (begin 	
    ;;(lda.a+y char)			
	(lda.zp+y #$02)
	(ldx.a (.addr :extend-neg))			
	(inx)  ;;; bho non lo so ma cosi funziona
	(while :not-zero (begin 		
		(asl)
		(dex)
	))
	(sta.zp+y #$06)	
	;;(sta.ay #$3008)	
	

))

(defun draw-software-sprite (begin 

	(ldy.im #0)
	(lda.im #0)
	(repeat-until :zero (begin 
	   (sta.ay (.addr :bitmap))
	   (iny)
	   (cpy.im #32)
	))

	(lda.a (.addr :playerx))	
	(and.im #7)
	(sta.a (.addr :extend))
	
	(eor.im #7)
	(sta.a (.addr :extend-neg))
	   
	(lda.a (.addr :char-ptr))
	(sta.zp #$02)

	(lda.a (.addr :char-ptr 1))
	(sta.zp #$03)

	(lda.a (.addr :set-ptr))
	(sta.zp #$04)
	(sta.zp #$06)

	(lda.a (.addr :set-ptr 1))
	(sta.zp #$05)
	(sta.zp #$07)
		
	
	(lda.im #$08)
	(clc)
	(adc.zp #$06)
	(sta.zp #$06)
	(if :carry-set 
		(inc.zp #$07))	

	(lda.a (.addr :playery))	
	(and.im #7)
	(sta.a (.addr :max-h))
	(eor.im #7)
	(sta.a (.addr :max-h-neg))


	(lda.a (.addr :max-h))
	(clc)
	(adc.zp #$04)
	(sta.zp #$04)	
	(if :carry-set 
		(inc.zp #$05))	


	(lda.a (.addr :max-h))
	(clc)
	(adc.zp #$06)
	(sta.zp #$06)
	(if :carry-set 
		(inc.zp #$07))	

	(ldy.im #0)
	(repeat-until :minus (begin 
		(copy-byte-mychar (.addr :mychar))
		(copy-byte-next   (.addr :mychar))
		(iny)
		(cpy.a (.addr :max-h-neg))
	))
	
	
		
	(lda.a (.addr :set-ptr))
	(sta.zp #$04)
	(sta.zp #$06)

	(lda.a (.addr :set-ptr 1))
	(sta.zp #$05)
	(sta.zp #$07)

	(lda.im #24)
	(clc)
	(adc.zp #$06)
	(sta.zp #$06)
	(if :carry-set 
		(inc.zp #$07))	
	
	(lda.im #16)
	(clc)
	(adc.zp #$04)
	(sta.zp #$04)
	(if :carry-set 
		(inc.zp #$05))	


	(lda.a (.addr :char-ptr))
	(sta.zp #$02)

	(lda.a (.addr :char-ptr 1))
	(sta.zp #$03)

	(lda.a (.addr :max-h-neg))
	(clc)
	(adc.zp #$02)
	(sta.zp #$02)
	(if :carry-set 
		(inc.zp #$03))	

	(ldy.im #0)
	(repeat-until :minus (begin 
		(copy-byte-mychar (.addr :mychar))
		(copy-byte-next   (.addr :mychar))
		(iny)
		((cpy.a (.addr :max-h)))
;;		(cpy.im #8)
	))


	(get-cursor-at (.addr :x) (.addr :y))
	(ldy.im #0)
	(lda.im #0)
	(sta.zp+y #$02)
	(iny)		
	(lda.im #1)
	(sta.zp+y #$02)
	(ldy.im #40)
	(lda.im #2)
	(sta.zp+y #$02)
	(iny)		
	(lda.im #3)
	(sta.zp+y #$02)


	(rts)

	(with-label :extend (.byte 0))
	(with-label :extend-neg (.byte 0))
	(with-label :extend-h (.byte 0))
	(with-label :max-h (.byte 0))
	(with-label :max-h-neg (.byte 0))


	(with-label :mychar 
		(.byte #b00111100 
		       #b11000011
			   #b11000011
			   #b11111111
			   #b11000011
			   #b11000011
			   #b11000011
			   #b11111111))
	(with-label :char-ptr (.word (.addr :mychar)))
	(with-label :set-ptr (.word (.addr :bitmap)))
	
))
(defun global-data (begin 	

	(label-at :bitmap #$2000 
		(.byte #$00 #$00 #$00 #$00 #$00 #$00 #$00 #$00 #$00 )) 

))
