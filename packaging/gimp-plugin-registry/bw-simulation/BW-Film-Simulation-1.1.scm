;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                 Serge Mankovski B/W Film Simulation
;
; 		Copyright Serge Mankovski (sevato@mankovski.com)
;                    Toronto, Ontario, 2007
;
;           Version 1.1  March 30,  2007
;           
; Converts selected layer into Black and White using channel mixer
; Uses channel presets found on Internet. I am not sure about the origin of these
; settings and I do not know if they really produce result resembling tonal qualities of
; the film, but it produces nice looking B/W and it is a useful way to convert to black and white
; 
; Change Log:
; Version 1.1
;		- added IR film simulation
;		- added color filters applied before b/w conversion
; 		- added saturation option
;		- Gimp 2.3.15+ support
;
; Version 1.1.1 Various fixes by Ari Pollak - thanks!
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(define (create-new-layer img drawable) 
  (let ((layer drawable)) 
	(set! layer (car (gimp-layer-copy drawable TRUE)))
	(gimp-image-add-layer img layer -1)
	layer))

      
(define (script-fu-bw-film  img drawable film filter rename new-layer increase-local-contrast auto-levels drop-gamma saturate )
	(let 
	  ((bw-layer nil)
	   (chan-name ""))

	  (gimp-image-undo-group-start img) ; Start an undo group.  



	  (if (equal? new-layer TRUE) 
		(set! bw-layer (create-new-layer img drawable))
		(set! bw-layer drawable)
		)

	  ;   (if (equal? auto-levels TRUE) (gimp-levels-stretch bw-layer) ())

	  (if (equal? saturate TRUE) 
		(plug-in-colors-channel-mixer 1 img bw-layer FALSE 1.3 -0.15 -0.15 -0.15 1.3 -0.15 -0.15 -0.15 1.3)
		())

	  (if (equal? drop-gamma TRUE) (gimp-levels bw-layer 0 0 255 0.9 0 255) ())    



	  (cond
		; Yellow Filter	
		((= filter 1)
		 (gimp-hue-saturation bw-layer 0 -5 0 33)    	
		 (set! chan-name (string-append chan-name " Yellow Filter")))	
		; Orange Filter	
		((= filter 2)
		 (gimp-hue-saturation bw-layer 0 -20 0 25)    	
		 (set! chan-name (string-append chan-name " Orange Filter")))	
		; Red Filter	
		((= filter 3)
		 (gimp-hue-saturation bw-layer 0 -41 0 25)    	
		 (set! chan-name (string-append chan-name " Red Filter")))	
		; Green Filter
		((= filter 4)
		 (gimp-hue-saturation bw-layer 0 90 0 33)    	
		 (set! chan-name (string-append chan-name " Green Filter")))	
		; Blue Filter
		((= filter 5)
		 (gimp-hue-saturation bw-layer 0 -145 0 25)    	
		 (set! chan-name (string-append chan-name " Blue Filter")))	
		)


	  (cond
		; Agfa 200X
		((= film 0)
		 (set! chan-name (string-append chan-name " Agfa 200X"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.18 0.41 0.41 0.18 0.41 0.41 0.18 0.41 0.41 ))
		; Agfapan 25
		((= film 1)  
		 (set! chan-name (string-append chan-name " Agfapan 25"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.25 0.39 0.36 0.25 0.39 0.36 0.25 0.39 0.36 ))
		; Agfapan 100
		((= film 2)  
		 (set! chan-name (string-append chan-name " Agfapan 100"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.21 0.40 0.39 0.21 0.40 0.39 0.21 0.40 0.39 ))
		; Agfapan 400
		((= film 3)  
		 (set! chan-name (string-append chan-name " Agfapan 400"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.20 0.41 0.39 0.20 0.41 0.39 0.20 0.41 0.39 ))
		; Ilford Delta 100
		((= film 4)  
		 (set! chan-name (string-append chan-name " Ilford Delta 100"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.21 0.42 0.37 0.21 0.42 0.37 0.21 0.42 0.37 ))
		; Ilford Delta 400
		((= film 5)  
		 (set! chan-name (string-append chan-name " Ilford Delta 400"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.22 0.42 0.36 0.22 0.42 0.36 0.22 0.42 0.36 ))
		; Ilford Delta 400 Pro & 3200
		((= film 6)  
		 (set! chan-name (string-append chan-name " Ilford Delta 400 Pro & 3200"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.31 0.36 0.33 0.31 0.36 0.33 0.31 0.36 0.33 ))
		; Ilford FP4
		((= film 7)  
		 (set! chan-name (string-append chan-name " Ilford FP4"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.28 0.41 0.31 0.28 0.41 0.31 0.28 0.41 0.31 ))
		; Ilford HP5
		((= film 8)  
		 (set! chan-name (string-append chan-name " Ilford HP5"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.23 0.37 0.40 0.23 0.37 0.40 0.23 0.37 0.40 ))
		; Ilford Pan F
		((= film 9)  
		 (set! chan-name (string-append chan-name " Ilford Pan F"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.33 0.36 0.31 0.33 0.36 0.31 0.33 0.36 0.31 ))
		; Ilford SFX
		((= film 10)  
		 (set! chan-name (string-append chan-name " Ilford SFX"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.36 0.31 0.33 0.36 0.31 0.33 0.36 0.31 0.33 ))
		; Ilford XP2 Super
		((= film 11)  
		 (set! chan-name (string-append chan-name " Ilford XP2 Super"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.21 0.42 0.37 0.21 0.42 0.37 0.21 0.42 0.37 ))
		; Kodak Tmax 100
		((= film 12)  
		 (set! chan-name (string-append chan-name " Kodak Tmax 100"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.24 0.37 0.39 0.24 0.37 0.39 0.24 0.37 0.39 ))
		; Kodak Tmax 400
		((= film 13)  
		 (set! chan-name (string-append chan-name " Kodak Tmax 400"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.27 0.36 0.37 0.27 0.36 0.37 0.27 0.36 0.37 ))
		; Kodak Tri-X
		((= film 14)  
		 (set! chan-name (string-append chan-name " Kodak Tri-X"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.25 0.35 0.40 0.25 0.35 0.40 0.25 0.35 0.40 ))
		; Kodak HIE
		((= film 15)  
		 (set! chan-name (string-append chan-name " Kodak HIE"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 1.0 1.0 -1.0 0.0 1.0 1.0 -1.0 0.0 1.0 1.0 -1.0 0.0 ))
		; Normal Contrast
		((= film 16)  
		 (set! chan-name (string-append chan-name " Normal Contrast"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.43 0.33 0.30 0.43 0.33 0.30 0.43 0.33 0.30 ))
		; High Contrast
		((= film 17)  
		 (set! chan-name (string-append chan-name " High Contrast"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.40 0.34 0.60 0.40 0.34 0.60 0.40 0.34 0.60 ))
		; Generic BW
		((= film 18)  
		 (set! chan-name (string-append chan-name " Generic BW"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.24 0.68 0.08 0.24 0.68 0.08 0.24 0.68 0.08 ))
		; 50-50
		((= film 19)  
		 (set! chan-name (string-append chan-name " 50-50"))
		 (plug-in-colors-channel-mixer 1 img bw-layer TRUE 0.5 0.5 0.00 0.0 0.5 0.5 0.00 0.0 0.5 0.5 0.00 0.0 ))
		)    


	  (if (equal? rename TRUE)  (gimp-drawable-set-name bw-layer chan-name) () )

	  (if (equal? increase-local-contrast TRUE) (plug-in-unsharp-mask 1 img bw-layer 30.0 0.25 9) ())

	  ; Complete the undo group

	  (gimp-image-undo-group-end img)
	
	; Flush the display 

	(gimp-displays-flush)))
 
 
(script-fu-register 
      
      "script-fu-bw-film"
 
      "<Image>/Script-Fu/BW Film Simulation" 
      "Black and White Film Simulation"
      "Serge Mankovski (sevato@mankvoski.com)"
      "2007, Serge Mankovski, Toronto, ON, Canada"
      "05.01.2007"
      "RGB*" 
      SF-IMAGE "Image" 0
      SF-DRAWABLE "Current Layer" 0
      SF-OPTION   "Film" 
			'( 		
					"Agfa 200X" 
					"Agfapan 25"
					"Agfapan 100"
					"Agfapan 400"
					"Ilford Delta 100"
					"Ilford Delta 400"
					"Ilford Delta 400 Pro & 3200"
					"Ilford FP4"
					"Ilford HP5"
					"Ilford Pan F"
					"Ilford SFX"
					"Ilford XP2 Super"
					"Kodak Tmax 100"
					"Kodak Tmax 400"
					"Kodak Tri-X"
					"Kodak HIE"
					"Normal Contrast"
					"High Contrast"
					"Generic BW"
					"50/50"
			)
    SF-OPTION   "Filter" 
			'( 		
					"Select"
					"Yellow"
					"Orange" 
					"Red"
					"Green"
					"Blue"
			)
									
	SF-TOGGLE "Rename Layer?" TRUE 		
	SF-TOGGLE "New Layer?" FALSE 		
	SF-TOGGLE "Increase Local Contrast" FALSE
	SF-TOGGLE "Auto Levels" FALSE	
	SF-TOGGLE "Drop Gamma 10%" FALSE
	SF-TOGGLE "Saturate" FALSE

)

