/*
 * Copyright (C) 2004-2012 Andrew Mihal
 *
 * This file is part of Enblend.
 *
 * Enblend is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Enblend is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Enblend; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */


#ifndef __GPU_H__
#define __GPU_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_LIBGLEW
#define GLEW_STATIC 1

#include <GL/glew.h>
#ifdef HAVE_APPLE_OPENGL_FRAMEWORK
#include <OpenGL/OpenGL.h>
#else
#include <GL/glut.h>
#endif

extern int Verbose;

void checkGLErrors(const char* file, unsigned line);
#define CHECK_GL() checkGLErrors(__FILE__, __LINE__)

void printInfoLog(GLhandleARB obj);
bool checkFramebufferStatus();

#ifdef HAVE_APPLE_OPENGL_FRAMEWORK
CGLContextObj cgl_init();
#endif

bool initGPU(int*, char**);
bool configureGPUTextures(unsigned int k, unsigned int vars);
bool gpuGDAKernel(unsigned int k, unsigned int vars, double t,
                  float* packedEData, float* packedPiData, float* packedOutData);
bool clearGPUTextures();
bool wrapupGPU(void);
#endif /* HAVE_LIBGLEW */

#endif /* __GPU_H__ */

// Local Variables:
// mode: c++
// End:
