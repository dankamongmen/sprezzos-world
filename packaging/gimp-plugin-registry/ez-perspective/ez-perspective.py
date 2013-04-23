#!/usr/bin/python
# -*- coding: utf-8 -*-

"""
EZ Perspective

A Gimp plug-in to easily change the perspective in an image,
just like it says on the tin.

Particularly useful for correcting photographs,
but can also be used to add a Dutch angle to any image.

Provides 3 parameters to change up/down (tilt, pitch),
left/right (swing, yaw), and rotation (twist, roll).

Please use:
* *after* correcting lens distortion
  (barrel distortion, vignetting, lateral chromatic aberration),
  as these depend on the lens, on the shot as taken

* *before* cropping,
  as this changes the shape of the image,
  and will likely warrant cropping afterwards


By Nils R. Barth, 2010 June 28, Portland, OR, USA

I dedicate this program to the Public Domain, and claim no rights to it.
Formally, under the Creative Commons CC0 license:

To the extent possible under law, I, Nils Roland Barth have waived all copyright and related or neighboring rights to this program.
This work is published from the United States of America.

"""

############################################################
# Libraries
############################################################

from gimpfu import *
from math import pi, sin, cos, sqrt

# Localization (l10n)
#
# use with _("foo") around all strings, to indicate “translatable”

import gettext
locale_directory = gimp.locale_directory
gettext.install("gimp20", locale_directory, unicode=True)


############################################################
# Main functions & content
############################################################

##############################
# Main function (via GUI)
##############################
def python_fu_ez_perspective_correction(img, drawable,
                                        uds, lrs, rots, # as sliders
                                        ud,   lr,  rot, # as explicit values
                                        efl,            # Effective Focal Length
                                        quality, crop,
                                       ):
    """
    Change the perspective of the given image+drawable as specified
    by the angles, (effective) focal length, and quality/crop settings.
    
    Specifically, parse the UI values and pass on to the underlying function
    which actually does the transform.
    """
    # Parse parameters
    # Take values from sliders
    ud_deg  = uds
    lr_deg  = lrs
    rot_deg = rots
    
    # …but override if set explicitly
    # (Note that these are in tenths, b/c of limits of the spinner box)
    if ud:  ud_deg  = ud/10. # 10. so makes floating
    if lr:  lr_deg  = lr/10.
    if rot: rot_deg = rot/10.

    angles = (ud_deg, lr_deg, rot_deg) # package up
    
    # Interpolation / Crop options
    # Translate Radio buttons to parameters
    qual_to_interp = {
        "fast": INTERPOLATION_NONE,    # fast
        "good": INTERPOLATION_LANCZOS, # High-quality interpolation
    }
    crop_to_opt = {
        "adjust":           TRANSFORM_RESIZE_ADJUST,           # Don’t clip, don’t crop
        "clip":             TRANSFORM_RESIZE_CLIP,             # clip only
        "crop to result":   TRANSFORM_RESIZE_CROP,             # crop to result
        "crop with aspect": TRANSFORM_RESIZE_CROP_WITH_ASPECT, # crop with aspect
    }
    
    interp_opt = qual_to_interp[quality]
    crop_opt = crop_to_opt[crop]
    opts = (interp_opt, crop_opt)

    # Action!
    proj_trans_image(img, drawable, angles, efl, opts)


##############################
# Main function (scriptable)
##############################
def proj_trans_image(img, drawable,
                     angles, efl, opts,
                    ):
    """
    Change the perspective of the given image+drawable as specified
    by the angles, (effective) focal length, and quality/crop settings.
    
    angles = (ud, lr, rot), in degrees,
    efl = effective focal length, in millimeters, based on diagonal angle of view
    opts = (interpolation, crop) – Gimp enums
    
    """
    # Start
    img.undo_group_start()
    # drawable probably equals img.active_layer, but don’t assume this
    
    # Get image dimensions
    width = img.width
    height = img.height
    dimensions = (width, height)

    # Compute transform coordinates
    frame = proj_trans_frame(angles, dimensions, efl)

    interp_opt, crop_opt = opts # unpack options
    # Do the transform
    pdb.gimp_drawable_transform_perspective(
        drawable,
        frame[0][0], frame[0][1], # (x,y) coords, as ul, ur, ll, lr
        frame[1][0], frame[1][1],
        frame[2][0], frame[2][1],
        frame[3][0], frame[3][1],
        TRANSFORM_FORWARD, # Technical parameter to use the forward transform (as specified), not the inverse
        interp_opt, # Interpolation
        True, # Not used (Supersampling determined by interpolation type), but specify True any, as that’s what we want
        3, # Recursion level; 3 is recommend, and should be enough (makes no diference when not interpolating)
        crop_opt,
    )
    img.resize_to_layers() # Fit canvas to new layer size (whether stretched or cropped)

    # Finish
    gimp.displays_flush()
    img.undo_group_end()

##############################
# Non-interactive
# (for batch processing)
##############################
def python_fu_ez_perspective_correction_non_interactive(
    in_file_name, out_file_name,
    ud,   lr,  rot, # as explicit values (angles)
    efl,            # Effective Focal Length
    quality, crop,
    ):
    """
    Change the perspective of the given file as specified
    by the angles, (effective) focal length, and quality/crop settings.
    
    angles are: ud, lr, rot – in degrees,
    efl = effective focal length, in millimeters, based on diagonal angle of view
    quality and crop are text fields, as per UI in 
    python_fu_ez_perspective_correction
    """
    # Load file
    image = pdb.gimp_file_load(in_file_name, in_file_name,
                               run_mode=RUN_NONINTERACTIVE)
    drawable = pdb.gimp_image_get_active_layer(image)
    
    # Apply transform
    # Note the 0 pads, and the *10 because sliders are in units
    # of 1/10, but these arguments are the actual angles
    python_fu_ez_perspective_correction(image, drawable,
                                        0, 0, 0,
                                        ud*10, lr*10, rot*10,
                                        efl,
                                        quality, crop)
    # Save file
    pdb.gimp_image_flatten(image) # Flatten first
    drawable = pdb.gimp_image_get_active_layer(image)
    pdb.gimp_file_save(image, drawable, out_file_name, out_file_name, run_mode=RUN_NONINTERACTIVE)
    pdb.gimp_image_delete(image) # Cleanup


##############################
# Math functions
# 
# Straight-forward projective
# transforms, with a few
# subtle corrections.
##############################

def proj_trans_frame(angles, dimensions, efl):
    """
    Compute corners of transformed frame,
    to feed into pdb.gimp_drawable_transform_perspective()
    
    angles = (ud, lr, rot), in degrees
    dimensions = (width, height), in pixels
    efl = effective focal length, in millimeters, based on diagonal angle of view
    
    Transform is done in 3 steps, to scale x & y directions separately.
    Order is as zyx Euler angles, or rather Tait–Bryan angles,
    which corresponds to rotation by the extrinsic (scene) axes in the
    order x, y, z (up/down, left/right, rotation),
    or alterantively to rotation by the intrinsic (camera) axes in the
    order z, y, x;
    this seems the most natural order for photography.
    """
    # Unpack parameters
    ud_deg, lr_deg, rot_deg = angles
    width, height = dimensions
    # Hard-code center for now at the middle (50%, 50%)
    center_x_pc = 50./100
    center_y_pc = 50./100
    center_x_px = center_x_pc * width
    center_y_px = center_y_pc * width

    # Compute depth (in pixels)
    # We scale by the image diagonal,
    # and assume that EFL is based on *diagonal* angle of view
    image_diagonal = sqrt(width*width + height*height)
    diagonal_35mm = 12*sqrt(13) # sqrt(36²+24²); diagonal of 36 mm × 24 mm
    depth = image_diagonal * efl / diagonal_35mm # correct for focal length

    z_fix = depth # Fix Z coordinate at depth (in pixels)

    # Set corners of frame so centered at origin
    ulx, uly =     0 - center_x_px,      0 - center_y_px
    urx, ury = width - center_x_px,      0 - center_y_px
    llx, lly =     0 - center_x_px, height - center_y_px
    lrx, lry = width - center_x_px, height - center_y_px
    
    frame = ((ulx, uly), (urx, ury), (llx, lly), (lrx, lry))

    # Do up/down, left/right, and rotation separately,
    #   so can scale suitably
    # (to minimize length distortion on the central x & y axes)
    #
    # BTW, rounding error & numerical precision are *not* issues:
    # the image dimensions are nowhere near the limits of *float*.
    frame = proj_trans_ud( frame, ud_deg,  z_fix)
    frame = proj_trans_lr( frame, lr_deg,  z_fix)
    frame = proj_trans_rot(frame, rot_deg, z_fix)

    # Unpack frame
    ((ulx, uly), (urx, ury), (llx, lly), (lrx, lry)) = frame
    
    # Move center back
    ulx += center_x_px
    urx += center_x_px
    llx += center_x_px
    lrx += center_x_px

    uly += center_y_px
    ury += center_y_px
    lly += center_y_px
    lry += center_y_px

    # Return frame, packaged as (x,y), in order
    return ((ulx, uly), (urx, ury), (llx, lly), (lrx, lry))


def proj_trans_ud(frame, ud_deg, z_fix):
    """
    Compute corners of frame transformed by up/down (pitch, tilt) transform.
    (Rotation about the x-axis.)
    Correct for shift and scaling.
    
    frame = ((ulx, uly), (urx, ury), (llx, lly), (lrx, lry))
      upper left, upper right, lower left, lower right,
      as pairs of (x,y) coordinates (in pixels).
      Order determined by pdb.gimp_drawable_transform_perspective()
    ud_deg = up/down (pitch, tilt), in degrees
    z_fix = fixed focal distance, in pixels
      (necessary technical parameter, determined from EFL and image size)
    
    """
    # Unpack parameters
    ((ulx, uly), (urx, ury), (llx, lly), (lrx, lry)) = frame

    ulx, uly = proj_trans_point(ud_deg, 0, 0, ulx, uly, z_fix)
    urx, ury = proj_trans_point(ud_deg, 0, 0, urx, ury, z_fix)
    llx, lly = proj_trans_point(ud_deg, 0, 0, llx, lly, z_fix)
    lrx, lry = proj_trans_point(ud_deg, 0, 0, lrx, lry, z_fix)

    # Correction factors
    # Shift & Scale:
    # * scx = x scale factor along midline due to up/down
    # * cy  = y shift
    scx, cy = proj_trans_point(ud_deg, 0, 0, 100, 0, z_fix)

    # center, to deal with shift
    uly -= cy
    ury -= cy
    lly -= cy
    lry -= cy

    # scale
    # scale in *both* dimensions, so proportions stay constant
    scale = 100/scx    
    ulx *= scale
    urx *= scale
    llx *= scale
    lrx *= scale
    
    uly *= scale
    ury *= scale
    lly *= scale
    lry *= scale
    
    # Return frame, packaged as (x,y), in order
    return ((ulx, uly), (urx, ury), (llx, lly), (lrx, lry))


def proj_trans_lr(frame, lr_deg, z_fix):
    """
    Compute corners of frame transformed by left/right (yaw, swing) transform.
    (Rotation about the y-axis.)
    Correct for shift and scaling.
    
    frame = ((ulx, uly), (urx, ury), (llx, lly), (lrx, lry))
      upper left, upper right, lower left, lower right,
      as pairs of (x,y) coordinates (in pixels).
      Order determined by pdb.gimp_drawable_transform_perspective()
    lr_deg = left/right (yaw, swing), in degrees
    z_fix = fixed focal distance, in pixels
      (necessary technical parameter, determined from EFL and image size)
    
    """
    # Unpack parameters
    ((ulx, uly), (urx, ury), (llx, lly), (lrx, lry)) = frame
    
    ulx, uly = proj_trans_point(0, lr_deg, 0, ulx, uly, z_fix)
    urx, ury = proj_trans_point(0, lr_deg, 0, urx, ury, z_fix)
    llx, lly = proj_trans_point(0, lr_deg, 0, llx, lly, z_fix)
    lrx, lry = proj_trans_point(0, lr_deg, 0, lrx, lry, z_fix)

    # Correction factors
    # Shift & Scale:
    # * scx = x scale factor along midline due to up/down
    # * cy  = y shift
    cx, scy = proj_trans_point(0, lr_deg, 0, 0, 100, z_fix)

    # center, to deal with shift
    ulx -= cx
    urx -= cx
    llx -= cx
    lrx -= cx

    # scale
    # scale in *both* dimensions, so proportions stay constant
    scale = 100/scy
    ulx *= scale
    urx *= scale
    llx *= scale
    lrx *= scale
    
    uly *= scale
    ury *= scale
    lly *= scale
    lry *= scale

    # Return frame, packaged as (x,y), in order
    return ((ulx, uly), (urx, ury), (llx, lly), (lrx, lry))


def proj_trans_rot(frame, rot_deg, z_fix):
    """
    Compute corners of frame transformed by rotation (roll, twist) transform.
    (Rotation about the z-axis.)
    Rotation is easiest because it preserves the plane,
    and hence no shift or scale correction is needed.
    Formally, rotation is in SO(2) of the plane, not just PSO(3).
    Term “rotation” is because this is conventional in photographs:
    it’s rotation in the *image* plane.
    Other perspective changes are rotations of the camera,
    hence possible confusion; we reserve “rotation” for this z-axis rotation.
    
    frame = ((ulx, uly), (urx, ury), (llx, lly), (lrx, lry))
      upper left, upper right, lower left, lower right,
      as pairs of (x,y) coordinates (in pixels).
      Order determined by pdb.gimp_drawable_transform_perspective()
    rot_deg = rotation (roll, twist), in degrees
    z_fix = fixed focal distance, in pixels
      (necessary technical parameter, determined from EFL and image size)
    
    
    """
    # Unpack parameters
    ((ulx, uly), (urx, ury), (llx, lly), (lrx, lry)) = frame

    ulx, uly = proj_trans_point(0, 0, rot_deg, ulx, uly, z_fix)
    urx, ury = proj_trans_point(0, 0, rot_deg, urx, ury, z_fix)
    llx, lly = proj_trans_point(0, 0, rot_deg, llx, lly, z_fix)
    lrx, lry = proj_trans_point(0, 0, rot_deg, lrx, lry, z_fix)

    # Return frame, packaged as (x,y), in order
    return ((ulx, uly), (urx, ury), (llx, lly), (lrx, lry))


def proj_trans_point(ud_deg, lr_deg, rot_deg, x_in, y_in, z_fix):
    """
    Compute transform of individual point.
    Simply matrix multiplication (by rotations),
    followed by scaling to fix depth/distance at z_fix (3rd projective coordinate).
    
    Do not perform any (x,y) plane corrections
    (namely shift or scale by how much center line moves and scales),
    as those are the same for the whole frame.
    """
    # Convert to radians
    ud  = 2*pi* ud_deg/360
    lr  = 2*pi* lr_deg/360
    rot = 2*pi*rot_deg/360

    # Apply rotation to (x,y,z) coordinates,
    # then scale so z coordinate is unchanged
    #   (b/c on plane at fixed depth/z distance)
    #   (effectively, the element of PSO(3) < Aut(RP²)
    #    corresponding to this rotation,
    #    acting on the affine plane at z = z_fix)
    #
    # Cleaner with matrices, but that requires NumPy;
    # easy enough to write matrices by hand.

    # Make generic (x,y,z) while intermediary
    (x, y, z) = (x_in, y_in, z_fix)

    # Note use of tuples (rather than separate equations)
    # so all computations use the *same* (x, y, z),
    # without needing auxiliary variables

    # up/down (x-axis rotation)
    (x, y, z) = (
        x,
          cos(ud)*y - sin(ud)*z,
          sin(ud)*y + cos(ud)*z,
    )

    # left/right (y-axis rotation)
    (x, y, z) = (
        cos(lr)*x - sin(lr)*z,
                  y,
        sin(lr)*x + cos(lr)*z,
    )
    
    # rotation (z-axis rotation)
    (x, y, z) = (
        cos(rot)*x - sin(rot)*y  ,
        sin(rot)*x + cos(rot)*y  ,
                                z,
    )
    
    # Correct by z-scale factor, so z_fix → z_fix
    z_scale = z_fix/z
    x_out = x * z_scale
    y_out = y * z_scale

    return x_out, y_out

############################################################
# Register function
############################################################

register(
    "python-fu-ez-perspective-correction", # Function name
    _(""), # Blurb / description
    _("Fix camera perspective via an easy interface"), # Help
    "Nils R. Barth", # Author
    _("Creative Commons CC0; public domain"), # Copyright notice
    "2011 May 25", # Date
    _("E_Z Perspective..."), # Menu label
    "RGB*,GRAY*",
    [
      (PF_IMAGE,    "img",      _("Input image"),    None),
      (PF_DRAWABLE, "drawable", _("Input drawable"), None),

      # Have sliders and spin boxes, so can easily set, but can also be precise
      (PF_SLIDER, "uds", _("up/down angle (\\/ /\\)"),    0, (-90, 90, 1 ) ),
      (PF_SLIDER, "lrs", _("left/right angle (> <)"), 0, (-90, 90, 1 ) ),
      (PF_SLIDER, "ros", _("rotation angle"),   0, (-90, 90, 1 ) ),
    
      # Can’t deal with decimals, it seems – hack around by using tenths
      (PF_SPINNER, "ud", _("up/down angle (in tenths)"),    0, (-900, 900, 1) ),
      (PF_SPINNER, "lr", _("left/right angle (in tenths)"), 0, (-900, 900, 1) ),
      (PF_SPINNER, "ro", _("rotation angle (in tenths)"),   0, (-900, 900, 1) ),
    
      (PF_SPINNER, "efl", _("focal length (35 mm equivalent)"), 50, (10, 600, 10) ), # contains sane length, defaults to normal lens; outside the range can key in manually, naturally
      # Quality (interpolation) and crop parameters
      (PF_RADIO, "quality", _("quality"), "fast", (
          (_("_fast"), "fast"),
          (_("_good"), "good")
      )),
      (PF_RADIO, "crop", _("crop"), "adjust", (
          (_("_adjust (no clip or crop)"), "adjust"),
          (_("c_lip"), "clip"),
          (_("crop to _result"), "crop to result"),
          (_("crop with _aspect"), "crop with aspect"),
      )),
    ],
    [], # No results
    python_fu_ez_perspective_correction, # Internal function name
    menu="<Image>/Filters/Distorts", # Register in menu
    domain=("gimp20-template", locale_directory) 
  )

register(
    "python_fu_ez_perspective_correction_non_interactive", # Function name
    _(""), # Blurb / description
    _("Fix camera perspective non-interactively"), # Help
    "Nils R. Barth", # Author
    _("Creative Commons CC0; public domain"), # Copyright notice
    "2011 May 25", # Date
    "", # Don’t put in menu
    "", # Doesn’t operate on drawables
    [
      (PF_STRING, "in_file_name",  _("Input file name"),  None),
      (PF_STRING, "out_file_name", _("Output file name"), None),

      (PF_FLOAT,  "ud", _("up/down angle (in degrees)"),    None),
      (PF_FLOAT,  "lr", _("left/right angle (in degrees)"), None),
      (PF_FLOAT,  "ro", _("rotation angle (in degrees)"),   None),
      
      (PF_FLOAT,  "efl", _("focal length (35 mm equivalent)"), None),

      (PF_STRING, "quality", _("interpolation quality"), None),
      (PF_STRING, "crop",    _("crop type"), None),
    ],
    [], # No results
    python_fu_ez_perspective_correction_non_interactive)


############################################################
# Main (go!)
############################################################

main()
