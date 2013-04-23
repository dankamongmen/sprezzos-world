;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
; 
; Match Points script  for GIMP 2.6
; Created by Mark Lowry
;
; Tags: align rotate transform
;
; Author statement:
;
; A GIMP script-fu to take two points on one layer and
; align them with two points on another layer by scaling,
; translating, and rotating the layer.
;
; Instructions:
; 1.  Select the layer you want to rotate.
; 2.  Create a path consisting of the four control points.
;     The first point selected will end up on top of the second
;     point, and the third point will end up on the fourth point,
;     i.e. when finished, x1,y1 will lie on x2,y2 and x3,y3 will
;     lie on x4,y4.
; 3.  IMPORTANT:  Go to the paths tab and give the path a name.
; 4.  Run the Match Points script.
; 5.  Enter the name of the path you created.
; 6.  Select clockwise or counter-clockwise to indicate the direction
;     of rotation that required to align the points.
; 7.  Select OK.
;
; Tested on 2.6.3
; 5/21/2009
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;   5/21/2009 - Corrected reference to x3-translated in counter-clockwise conditional
; --------------------------------------------------------------------
; 
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (script-fu-matchpoints img drawable path-name cw-ccw)

  ; DEFINE VARIABLES


 (let*
   (
         (pi 3.14159265359)
         (x1 0)
         (y1 0)
         (x2 0)
         (y2 0)
         (x3 0)
         (y3 0)
         (x4 0)
         (y4 0)
         (L24 0)
         (L13 0)
         (scale-factor 0)
         (width-new 0)
         (height-new 0)
         (x1-new 0)
         (y1-new 0)
         (x3-new 0)
         (y3-new 0)
         (x3-translated 0)
         (y3-translated 0)
         (angle-between 0)
   )

  ; Retrieve the control points from the path

  (set! x1 (aref (car (cdddr (gimp-path-get-points img path-name))) 0 ))

  (set! y1 (aref (car (cdddr (gimp-path-get-points img path-name))) 1 ))

  (set! x2 (aref (car (cdddr (gimp-path-get-points img path-name))) 6 ))

  (set! y2 (aref (car (cdddr (gimp-path-get-points img path-name))) 7 ))

  (set! x3 (aref (car (cdddr (gimp-path-get-points img path-name))) 15 ))

  (set! y3 (aref (car (cdddr (gimp-path-get-points img path-name))) 16 ))

  (set! x4 (aref (car (cdddr (gimp-path-get-points img path-name))) 24 ))

  (set! y4 (aref (car (cdddr (gimp-path-get-points img path-name))) 25 ))

  ; Calculate scale factor as the ratio of
  ; the length of the line from point 2 to point 4 divided by
  ; the length of the line from point 1 to point 3.

  (set! L24 (sqrt ( + (* (- x4 x2) (- x4 x2)) (* (- y4 y2) (- y4 y2) )  ) ) )

  (set! L13 (sqrt ( + (* (- x3 x1) (- x3 x1)) (* (- y3 y1) (- y3 y1) )  ) ) )

  (set! scale-factor (/ L24 L13) )

  ; Calculate new layer width and height

  (set! width-new (* scale-factor (car (gimp-drawable-width drawable)) ) )

  (set! height-new (* scale-factor (car (gimp-drawable-height drawable)) ) )

  ; Calculate coordinates of x1,y1 after scaling

  (set! x1-new (* scale-factor x1) )

  (set! y1-new (* scale-factor y1) )

  ; Calculate coordinates of x3,y3 after scaling

  (set! x3-new (* scale-factor x3) )

  (set! y3-new (* scale-factor y3) )

  ; Calculate coordinates of x3,y3 after the translation of x1,y1 to x2,y2 has taken place

  (set! x3-translated (+ x3-new (- x2 x1-new)))

  (set! y3-translated (+ y3-new (- y2 y1-new)))

; Calculate rotation angle, angle-between based on acos of the dot product of
; the two unit vectors defining the lines from x2,y2 to x4,y4 and from x2,y2 to x3-translated,y3-translated.
; This angle may need to be positive or negative, depending upon how the translated x1,y1 to x3,y3 vector
; needs to be rotated.

  (if (= cw-ccw 0)

      (set! angle-between (acos (dot-product (- x3-translated x2) (- y3-translated y2) 0 (- x4 x2) (- y4 y2) 0)))

  )

  (if (= cw-ccw 1)

      (set! angle-between (* -1 (acos (dot-product (- x3-translated x2) (- y3-translated y2) 0 (- x4 x2) (- y4 y2) 0))))

  )


  ; START AN UNDO GROUP.
  ; Everything between the start and the end will be carried out if an undo command is issued.

  (gimp-image-undo-group-start img)


  ; SCALE THE LAYER

  (gimp-drawable-transform-scale drawable 0 0 width-new height-new TRANSFORM-FORWARD INTERPOLATION-CUBIC 1 3 0)


  ; TRANSLATE THE LAYER

  (gimp-layer-translate drawable (- x2 x1-new) (- y2 y1-new) )


  ; ROTATE THE LAYER

  (gimp-drawable-transform-rotate drawable angle-between 0 x2 y2 TRANSFORM-FORWARD INTERPOLATION-CUBIC 1 3 0)


  ; Complete the undo group

  (gimp-image-undo-group-end img)


  ; Flush the display

  (gimp-displays-flush)

))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                      ;
;                      dot-product                     ;
;                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This function calculates the dot product of the UNIT VECTORS
; corresponding to the vectors v1=ai+bj+ck and v2=xi+yj+zk.

(define (dot-product a b c x y z)

 (let*
   (
         (Labc 0)
         (Lxyz 0)
         (a1 0)
         (b1 0)
         (c1 0)
         (x1 0)
         (y1 0)
         (z1 0)
   )


  ; Calculate the magnitudes of the two vectors.

  (set! Labc (sqrt (+ (pow a 2) (pow b 2) (pow c 2))))

  (set! Lxyz (sqrt (+ (pow x 2) (pow y 2) (pow z 2))))

  ; Determine the coefficients of the two unit vectors.

  (set! a1 (/ a Labc))
  (set! b1 (/ b Labc))
  (set! c1 (/ c Labc))
  (set! x1 (/ x Lxyz))
  (set! y1 (/ y Lxyz))
  (set! z1 (/ z Lxyz))


  ; Calculate the dot product of the two unit vectors.

  (+ (* a1 x1) (* b1 y1) (* c1 z1))

))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(script-fu-register "script-fu-matchpoints"
     _"Match Points"
     _"Aligns two points on the current layer with two
points on any other layer(s) by scaling the layer, moving
point 1 onto point 2, and rotating point 3 onto point 4.
You must create a path containing these four points in the
order 1, 2, 3, 4 (where 1 moves to 2, 3 moves to 4) prior
to executing this script."
     "Script by Mark Lowry"
     "Script by Mark Lowry"
     "2009"
     "RGB*, GRAY*"
     SF-IMAGE "Image" 0
     SF-DRAWABLE "Current Layer" 0
     SF-STRING _"Path Name:" "1234"
     SF-OPTION _"Rotation"   '(_"Clockwise" _"Counter-clockwise" )
 )

(script-fu-menu-register "script-fu-matchpoints"
                         "<Image>/FX-Foundry/Toolbox")
